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

-define(SHELL_TEXT_BOX, 1).
-define(PROMPT, "> ").

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
	
	ShellTextBox = wxTextCtrl:new(Panel, ?SHELL_TEXT_BOX, [{style, ?wxTE_MULTILINE bor ?wxTE_RICH}]),
  wxWindow:setFont(ShellTextBox, wxFont:new(12, ?wxFONTFAMILY_TELETYPE, 
                                                ?wxNORMAL, 
                                                ?wxNORMAL,[])),
                                                	
	wxSizer:add(MainSizer, ShellTextBox, [{flag, ?wxEXPAND},
                                          {proportion, 1}]),
                                          
	% Connect listener to text box	
	wxTextCtrl:connect(ShellTextBox, char, [{callback, fun(E,O) ->
													       handle_char_event(E,O)
													   end
                                           }]),
		
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

% %% ENTER
handle_char_event(#wx{obj=Console, event=#wxKey{type=char, keyCode=13}},O) -> 
	LineText = wxTextCtrl:getLineText(Console, wxTextCtrl:getNumberOfLines(Console) - 1),
	Input = string:substr(LineText, get_prompt_length(LineText) + 2),
	wxTextCtrl:setInsertionPointEnd(Console),
	case length(Input) of
		0 -> 
			%% Single enter key pressed with no other input.
			%% Note we have manually insert the prompt because sending a single newline '\n'
			%% to the port results in no response. It is the terminal that redraws the prompt
			%% and not the ERTS. Same goes with history (up arrow/down arrow).
			%% The port will only respond through stdout when a '.' is received.
			prompt_2_console(Console, LineText),
			call_parser(Input), %% send anyway, so any error contains the correct position integer
			wxEvent:stopPropagation(O);
		_ ->
			Last = 	fun(46) -> %% keycode 46 = '.'
						%% Deal with the case where several '.'s are entered, '...'
						prompt_or_not(length(Input), Input, Console, LineText, O);
					(_) -> %% write the newline and prompt to the console
						prompt_2_console(Console, LineText)
					end,
			add_cmd(Input),
			Last(lists:last(Input)),
      io:format("IN ENTER~n"),
			call_parser(Input)
	end;
	
%% Arrow keys
handle_char_event(#wx{obj=Console, event=#wxKey{type=char, keyCode=?WXK_UP}},_O) ->
	SuccessFun = fun() -> ok end,
	FailFun    = fun() -> wxTextCtrl:setInsertionPointEnd(Console) end,
	check_cursor(Console, SuccessFun, FailFun, 0),
	case cmd_index() of
		0 ->
			ok;
		_ ->
			cycle_cmd_text(Console, -1)
	end;
	
handle_char_event(#wx{obj=Console, event=#wxKey{type=char, keyCode=?WXK_DOWN}},_O) ->
	SuccessFun = fun() -> ok end,
	FailFun    = fun() -> wxTextCtrl:setInsertionPointEnd(Console) end,
	check_cursor(Console, SuccessFun, FailFun, 0),
	LastCmd = length(cmd_history()) - 1,
	Limit = LastCmd + 1,
	case cmd_index() of
		LastCmd ->
			clear_cmd_text(Console),
			update_cmd_index(cmd_index()+1);
		Limit ->
			ok;
		_ ->
			cycle_cmd_text(Console, 1)
	end;
  
handle_char_event(#wx{obj=Console, event=#wxKey{type=char, keyCode=?WXK_LEFT}},O) ->
  io:format("LEFT ARROW KEY PRESS~n"),
	SuccessFun = fun() -> wxEvent:skip(O) end,
	FailFun    = fun() -> ok end,
	check_cursor(Console, SuccessFun, FailFun, 1);

handle_char_event(#wx{obj=Console, event=#wxKey{type=char, keyCode=?WXK_RIGHT}},O) ->
	SuccessFun = fun() -> wxEvent:skip(O) end,
	FailFun    = fun() -> ok end,
	check_cursor(Console, SuccessFun, FailFun, 0);
	
%% Backspace
handle_char_event(#wx{obj=Console, event=#wxKey{type=char, keyCode=8}},O) -> 
  io:format("BACKSPACE~n"),
	SuccessFun = fun() -> wxEvent:skip(O) end,
	FailFun    = fun() -> ok end,
	check_cursor(Console, SuccessFun, FailFun, 1);
	
%% CHAR
handle_char_event(#wx{obj=Console, event=#wxKey{type=char, keyCode=_KeyCode}},O) -> 
	SuccessFun = fun() -> wxEvent:skip(O) end,
	FailFun    = fun() -> wxTextCtrl:setInsertionPointEnd(Console), wxEvent:skip(O) end,
	check_cursor(Console, SuccessFun, FailFun, 0);
	
%% Catchall  
handle_char_event(E,O) ->
	io:format("Event: ~p~n Object: ~p~n", [E,O]).
	
	
%% =====================================================================
%% @doc Write a newline plus the repeated prompt to the console.
	
prompt_2_console(Console, LineText) ->
	wxTextCtrl:writeText(Console, io_lib:nl()),
	wxTextCtrl:writeText(Console, get_current_prompt(LineText)).
	
	
%% =====================================================================
%% @doc Determine whether we need to manually prompt_2_console()
%%
%% @private
	
prompt_or_not(N,Input,Console,LineText,Ev) when N>1 ->
	Penult = lists:nth(length(Input)-1, Input),
	if 
		Penult =:= 46 ->
			prompt_2_console(Console, LineText),
			wxEvent:stopPropagation(Ev);
		true -> 
      %% BUG R16B01 - wx 2.9.4
      %% Skipping this event causes the default behaviour to occur
      %% which calls our event handler a second time, which effectively duplicates the user's input.
      wxEvent:skip(Ev)
      ok
	end;
prompt_or_not(_,_Input,_,_,Ev) ->
	wxEvent:skip(Ev).
	
	
%% =====================================================================
%% @doc
	
call_parser(Message) ->
  % io:format("Send message~n"),
  % io:format("Message~p~n", [Message]),
	parser:parse_input(Message).
	
	
%% =====================================================================
%% @doc
	
load_response(Response) ->
  % io:format("Load response~n"),
  % io:format("Response~p~n", [Response]),
	{Env, Tc} = wx_object:call(?MODULE, text_ctrl),
	wx:set_env(Env),
	wxTextCtrl:writeText(Tc, Response).
	
	
%% =====================================================================
%% @doc Get the prompt i.e. '2> ' from the given line.
	
get_current_prompt(Line) ->
	string:substr(Line, 1, get_prompt_length(Line) + 1).
	
	
%% =====================================================================
%% @doc Get the length of the prompt.
	
get_prompt_length(Line) ->
	get_prompt_length(Line, 1).
	
get_prompt_length([Char|String], Count) ->
	case Char of
		62 -> % the prompt char '>'
			Count;
		_ -> 
			get_prompt_length(String, Count + 1)
	end.
	
	
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
	clear_cmd_text(Console), 
	update_cmd_index(cmd_index() + Direction),
	Command = get_command(cmd_index()),
	wxTextCtrl:writeText(Console, Command).	
	
	
%% =====================================================================
%% @doc

clear_cmd_text(Console) ->
	LastPos = wxTextCtrl:getLastPosition(Console),
	wxTextCtrl:remove(Console, LastPos - length(get_input(Console)), LastPos).
	
	
%% =====================================================================
%% @doc

get_input(Console) ->
	LastLine = wxTextCtrl:getNumberOfLines(Console) - 1,
	LineText = wxTextCtrl:getLineText(Console, LastLine),
	string:substr(LineText, get_prompt_length(LineText) + 2).

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
  % io:format("Point: ~p~n", [wxTextCtrl:getInsertionPoint(Console)]),
  %% NOTE positionToXY is NOT implemented on wxMac, and will always return false.
	{Bool,X,Y} = wxTextCtrl:positionToXY(Console, wxTextCtrl:getInsertionPoint(Console)),
  % io:format("BOOL: ~p~n", [Bool]),
  % io:format("X: ~p~n", [X]),
  % io:format("Y: ~p~n", [Y]),
  % io:format("In. Point: ~p~n", [wxTextCtrl:getInsertionPoint(Console)]),
  LastLine = wxTextCtrl:getNumberOfLines(Console) - 1,
	PromptLen = get_prompt_length(wxTextCtrl:getLineText(Console, LastLine)),
	case (X > PromptLen + PromptOffset) and (Y =:= LastLine) of
		true -> 
      % io:format("TRUE FUN~n"),
			SuccessFun();
		false ->
      % io:format("FALSE FUN~n"),
			FailFun()
	end.
	
	
  %% =====================================================================
  %% @doc Update the shell's theme
  
set_theme(Fg, Bg) ->
  {_, Tc} = wx_object:call(?MODULE, text_ctrl),
  wxTextCtrl:setBackgroundColour(Tc, Bg),
  wxTextCtrl:setStyle(Tc, 0, wxTextCtrl:getLastPosition(Tc), wxTextAttr:new(Fg)),
  %% setDefaultStyle() not working on wxMac 2.9.4
  %% We cannot update the text colour as a result
  %% Another good reason to switch to wxStyledTextCtrl
  wxTextCtrl:setDefaultStyle(Tc, wxTextAttr:new(Fg)).
    