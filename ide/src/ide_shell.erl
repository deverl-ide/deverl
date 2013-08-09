-module(ide_shell).
-include_lib("wx/include/wx.hrl").

-behaviour(wx_object).
-export([new/1,
		 init/1, 
		 terminate/2, 
		 code_change/3, 
		 handle_info/2, 
		 handle_call/3, 
		 handle_cast/2, 
		 handle_event/2]).
     
-export([load_response/1,
         set_theme/2]).

-define(ID_SHELL_TEXT_BOX, 1).
-define(PROMPT, "> ").

-define(stc, wxStyledTextCtrl).

%% The record containing the State.
-record(state, {win, textctrl, cmd_history, current_cmd, wx_env}).

new(Config) ->
	wx_object:start_link({local, ?MODULE}, ?MODULE, Config, []).
	
%% Initialise the server's state
init(Config) ->
	Parent = proplists:get_value(parent, Config),
  Panel = wxPanel:new(Parent, []),
	MainSizer = wxBoxSizer:new(?wxVERTICAL),
  wxWindow:setSizer(Panel, MainSizer),

  ShellTextBox = ?stc:new(Panel, [{id, ?ID_SHELL_TEXT_BOX}]),
  ?stc:setMarginWidth(ShellTextBox, 0, 0),
  ?stc:setMarginWidth(ShellTextBox, 1, 0),
  ?stc:setMarginWidth(ShellTextBox, 2, 0),
  ?stc:setMarginLeft(ShellTextBox, 2),
  ?stc:setLexer(ShellTextBox, ?wxSTC_LEX_NULL),
  
  Font = wxFont:new(13, ?wxFONTFAMILY_TELETYPE, ?wxNORMAL,  ?wxNORMAL,[]),
  io:format("Family: ~p~nFace: ~p~n", [wxFont:getFamily(Font), wxFont:getFaceName(Font)]),
  
  ?stc:styleSetFont(ShellTextBox, ?wxSTC_STYLE_DEFAULT, 
                    wxFont:new(13, ?wxFONTFAMILY_TELETYPE, 
                                  ?wxNORMAL, 
                                  ?wxNORMAL,[])),
  ?stc:setCaretWidth(ShellTextBox, 1),
  
  ?stc:cmdKeyClear(ShellTextBox, ?wxSTC_KEY_UP, 0),

                                                	
	wxSizer:add(MainSizer, ShellTextBox, [{flag, ?wxEXPAND},
                                          {proportion, 1}]),
                                          
  % ?stc:connect(ShellTextBox, stc_change, [{callback, fun(E,O) -> io:format("EVENT~p~n", [O]) end}]),
  ?stc:connect(ShellTextBox, key_down, [{callback, fun(E,O) -> handle_key_event(E,O) end}]),

	{Panel, #state{win=Panel, 
					   textctrl=ShellTextBox, 
					   cmd_history=[],
					   current_cmd=0,
					   wx_env=wx:get_env()}}. %% Maintained at server


%% =====================================================================
%% @doc OTP behaviour callbacks

handle_info(Msg, State) ->
    io:format("Got Info ~p~n",[Msg]),
    {noreply,State}.

handle_cast(Msg, State) ->
    io:format("Got cast ~p~n",[Msg]),
    {noreply,State}.
    
handle_call(text_ctrl, _From, State) ->
    {reply,{State#state.wx_env,State#state.textctrl}, State};
handle_call(command_history, _From, State) ->
	{reply, {State#state.cmd_history}, State};
handle_call({update_cmd_history, CmdLst}, _From, State) ->
    {reply, ok, State#state{cmd_history=CmdLst}};
handle_call(command_index, _From, State) ->
	{reply, {State#state.current_cmd}, State};
handle_call({update_cmd_index, Index}, _From, State) ->
    {reply, ok, State#state{current_cmd=Index}};
handle_call(Msg, _From, State) ->
    io:format("Got Call ~p~n",[Msg]),
    {reply,ok,State}.
    
handle_event(_Event, State) ->
    io:format("SHELL EVENT CA~n"),
    {noreply, State}.
    
code_change(_, _, State) ->
	{stop, not_yet_implemented, State}.

terminate(_Reason, #state{win=Frame}) ->
	io:format("TERMINATE SHELL~n"),
	wxPanel:destroy(Frame).


%% =====================================================================
%% Asynchronous event callbacks

%%--- Enter
handle_key_event(#wx{obj=Console, event=#wxKey{type=key_down, keyCode=13}},O) -> 
  {Prompt,Input} = split_line_at_prompt(Console),
  ?stc:gotoPos(Console, ?stc:getLength(Console)),
  case length(Input) of
    0 -> 
      %% Single enter key pressed with no other input.
      %% Note we have manually insert the prompt because sending a single newline '\n'
      %% to the port results in no response. It is the terminal that redraws the prompt
      %% and not the ERTS. Same goes with history (up arrow/down arrow).
      %% The port will only respond through stdout when a '.' is received.
      prompt_2_console(Console, Prompt),
      call_parser(Input), %% send anyway, so any error contains the correct position integer
      ok;
    _ ->      
      Last =   fun(46) -> %% keycode 46 = '.'
            %% Deal with the case where several '.'s are entered, '...'
            prompt_or_not(Console, Input, Prompt, O);
          (_) -> %% write the newline and prompt to the console
            prompt_2_console(Console, Prompt),
            ok
          end,
      add_cmd(Input),
      Last(lists:last(Input)),
      call_parser(Input),
      ok
  end;
	
%%--- Arrow keys
handle_key_event(#wx{obj=Console, event=#wxKey{type=key_down, keyCode=?WXK_UP}},_O) ->
  SuccessFun = fun() -> ok end,
  FailFun    = fun() -> ?stc:gotoPos(Console, ?stc:getLength(Console)) end,
  check_cursor(Console, SuccessFun, FailFun, -1),
  case cmd_index() of
    0 ->
      ok;
    _ ->
      cycle_cmd_text(Console, -1)
  end;
  
handle_key_event(#wx{obj=Console, event=#wxKey{type=key_down, keyCode=?WXK_DOWN}},_O) ->
  SuccessFun = fun() -> ok end,
  FailFun    = fun() -> ?stc:gotoPos(Console, ?stc:getLength(Console)) end,
  check_cursor(Console, SuccessFun, FailFun, -1),
  LastCmd = length(cmd_history()) - 1,
  Limit = LastCmd + 1,
  case cmd_index() of
    LastCmd ->
      replace_cmd_text(Console, ""),
      update_cmd_index(cmd_index()+1);
    Limit ->
      ok;
    _ ->
      cycle_cmd_text(Console, 1)
  end;
  
handle_key_event(#wx{obj=Console, event=#wxKey{type=key_down, keyCode=?WXK_LEFT}}, O) ->
  check_cursor(Console, fun() -> wxEvent:skip(O) end, fun() -> ok end, 0);

handle_key_event(#wx{obj=Console, event=#wxKey{type=key_down, keyCode=?WXK_RIGHT}}, O) ->
  check_cursor(Console, fun() -> wxEvent:skip(O) end, fun() -> ok end, -1);
	
handle_key_event(#wx{obj=Console, event=#wxKey{type=key_down, keyCode=8}}, O) -> 
  check_cursor(Console, fun() -> wxEvent:skip(O) end, fun() -> ok end, 0);
	
handle_key_event(#wx{obj=Console, event=#wxKey{type=key_down}}, O) -> 
  SuccessFun = fun() -> wxEvent:skip(O) end,
  FailFun    = fun() -> ?stc:gotoPos(Console, ?stc:getLength(Console)), wxEvent:skip(O) end,
  check_cursor(Console, SuccessFun, FailFun, -1);
	
%% For testing:
handle_key_event(E,O) ->
  io:format("Event: ~p~n Object: ~p~n", [E,O]),
  ok.
	
	
%% =====================================================================
%% @doc Write a newline plus the repeated prompt to the console.
	
prompt_2_console(Console, Prompt) ->
  ?stc:addText(Console, io_lib:nl()),
	?stc:addText(Console, Prompt).
	
	
%% =====================================================================
%% @doc Determine whether we need to manually prompt_2_console().
%% @private

prompt_or_not(Console, Input, Prompt, EvObj) when erlang:length(Input) > 1 ->
  Penult = lists:nth(length(Input)-1, Input),
	if 
		Penult =:= 46 ->
			prompt_2_console(Console, Prompt),
			wxEvent:stopPropagation(EvObj);
		true -> 
      wxEvent:skip(EvObj)
	end;
  
prompt_or_not(_,_,_,EvObj) ->
	wxEvent:skip(EvObj).
	
	
%% =====================================================================
%% @doc
	
call_parser(Message) ->
  % io:format("Message~p~n", [Message]),
	parser:parse_input(Message).
	
	
%% =====================================================================
%% @doc
	
load_response(Response) ->
  % io:format("Response~p~n", [Response]),
	{Env, Tc} = wx_object:call(?MODULE, text_ctrl),
	wx:set_env(Env),
	?stc:addText(Tc, Response).


%% =====================================================================
%% @doc Separate the prompt and command on the most recent line.

split_line_at_prompt(Console) ->
  lists:split(get_cur_prompt_length(Console), 
    ?stc:getLine(Console, ?stc:getLineCount(Console) - 1)).	
	

%% =====================================================================
%% @doc Get the length of the current prompt.
	
get_cur_prompt_length(Console) ->
  get_prompt_length(?stc:getLine(Console, ?stc:getLineCount(Console) - 1)).
	
  
%% =====================================================================
%% @doc Get the length of the prompt.
	
get_prompt_length(Line) ->
	get_prompt_length(Line, 1).
	
get_prompt_length([Char|String], Count) ->
	case Char of
		62 -> % the prompt char '>'
			Count + 1; %% 
		_ -> 
			get_prompt_length(String, Count + 1)
	end.
	

%% =====================================================================
%% @doc
%% @private

get_line_start_pos(Console, Pos) ->
  ?stc:positionFromLine(Console, ?stc:lineFromPosition(Console, Pos)).
	
  
%% =====================================================================
%% @doc Append new command to end of command history iff last element not same as new element.

add_cmd(Command) ->
	CommandList = cmd_history(),
	case length(CommandList) of
		0 ->
			update_cmd_history(CommandList, Command),
			update_cmd_index(length(CommandList)+1);
		_ ->
			case Command =:= get_command(length(CommandList) - 1) of
				true ->
					update_cmd_index(length(CommandList));
				false ->
					update_cmd_history(CommandList, Command),
					update_cmd_index(length(CommandList)+1)
			end
	end.
	
	
%% =====================================================================
%% @doc Cycle through command history by one entry.
%% param Direction: -1 for prev cmd, +1 for next cmd.

cycle_cmd_text(Console, Direction) ->
	update_cmd_index(cmd_index() + Direction),
	Cmd = get_command(cmd_index()),
  replace_cmd_text(Console, Cmd).
	
	
%% =====================================================================
%% @doc Replace the current command with the updated command Cmd.
%% The replacement will always take place on the most recent line.

replace_cmd_text(Console, Cmd) ->
  ?stc:setSelection(Console, get_cur_prompt_length(Console) +
      get_line_start_pos(Console, ?stc:getLength(Console)),
    ?stc:getLength(Console)),
  ?stc:replaceSelection(Console, Cmd).


%% =====================================================================
%% @doc
	
cmd_index() ->
	{CommandIndex} = wx_object:call(?MODULE, command_index),
	CommandIndex.
	
	
%% =====================================================================
%% @doc

cmd_history() ->
	{CommandList} = wx_object:call(?MODULE, command_history),
	CommandList.
	
	
%% =====================================================================
%% @doc

update_cmd_index(NewIndex) ->
	wx_object:call(?MODULE, {update_cmd_index, NewIndex}).
	
	
%% =====================================================================
%% @doc

update_cmd_history(CommandList, Command) ->
	wx_object:call(?MODULE, {update_cmd_history, CommandList ++ [Command]}).
	
	
%% =====================================================================
%% @doc Retrieve command from history based on indexed position.

get_command(Index) ->
	get_command(Index, cmd_history(), 0).
get_command(Index, [H|T], Count) ->
	case Index =:= Count of
		true ->
			H;
		false ->
			get_command(Index, T, Count + 1)
	end.	
	
	
%% =====================================================================
%% @doc Replace element at given index in a list. (Not yet used)

replace(Index, Elem, List) ->
	replace(Index, Elem, List, 0, []).
replace(Index, Elem, [H|T], Count, Acc) ->
	case Index =:= Count of
		true ->
			Acc ++ [Elem] ++ T;
		false ->
			replace(Index, Elem, T, Count + 1, Acc ++ [H])
	end.
	
	
%% =====================================================================
%% @doc Check cursor is in valid position, and execute appropriate function.
	
check_cursor(Console, SuccessFun, FailFun, PromptOffset) ->
  Limit = get_line_start_pos(Console, ?stc:getLength(Console)) + 
    get_cur_prompt_length(Console),
  case (?stc:getCurrentPos(Console) > Limit + PromptOffset)  of
    true -> 
      SuccessFun();
    false ->
      FailFun()
  end,
  ok.
	
	
%% =====================================================================
%% @doc Update the shell's theme
  
set_theme(Fg, Bg) ->
  {_, Tc} = wx_object:call(?MODULE, text_ctrl),
  ?stc:styleSetBackground(Tc, ?wxSTC_STYLE_DEFAULT, Bg),
  ?stc:styleSetForeground(Tc, ?wxSTC_STYLE_DEFAULT, Fg),
  ?stc:setCaretForeground(Tc, Fg),
  ?stc:styleClearAll(Tc),
  ok.
    