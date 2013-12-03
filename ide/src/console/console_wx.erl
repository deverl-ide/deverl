%% =====================================================================
%% @author
%% @copyright
%% @title
%% @version
%% @doc This module initalises the console, which is currently
%% implemented as a wxStyledTextCtrl.
%% @end
%% =====================================================================

-module(console_wx).

-include_lib("wx/include/wx.hrl").

%% wx_object
-behaviour(wx_object).
-export([init/1,
		 		 terminate/2,
				 code_change/3,
				 handle_info/2,
				 handle_call/3,
				 handle_cast/2,
				 handle_event/2,
				 handle_sync_event/3]).

%% API
-export([new/1,
				 append_command/1,
         append_message/1,
         set_theme/4,
         set_font/1,
         clear/0]).

%% Macros
-define(ID_SHELL_TEXT_BOX, 1).
-define(stc, wxStyledTextCtrl).
-define(ID_RESET_CONSOLE, 1).
-define(ID_CLEAR_CONSOLE, 2).
-define(PROMPT, "> ").
-define(STYLE_PROMPT, 12).
-define(STYLE_ERROR, 2).
-define(MARKER_MSG, 1).

%% Server state
-record(state, {win,
								textctrl,
								cmd_history,
								current_cmd,
                input,
								wx_env,
                menu,
                busy}).


%% =====================================================================
%% Client API
%% =====================================================================

%% =====================================================================
%% @doc

new(Config) ->
	wx_object:start_link({local, ?MODULE}, ?MODULE, Config, []).


%% =====================================================================
%% @doc

append_command(Response) ->
  wx_object:cast(?MODULE, {append, Response}).



%% =====================================================================
%% @doc

append_message(Msg) ->
  wx_object:cast(?MODULE, {append_msg, Msg}).


%% =====================================================================
%% @doc Update the shell's theme

set_theme(Fg, Bg, MrkrBg, ErrFg) ->
  wx_object:cast(?MODULE, {set_theme, Fg, Bg, MrkrBg, ErrFg}).


%% =====================================================================
%% @doc

set_font(Font) ->
  wx_object:cast(?MODULE, {set_font, Font}).


%% =====================================================================
%% @doc

clear() ->
  wx_object:cast(?MODULE, clear).
  

%% =====================================================================
%% Callback functions
%% =====================================================================

init(Config) ->
	Parent = proplists:get_value(parent, Config),
	Panel = wxPanel:new(Parent, []),
	MainSizer = wxBoxSizer:new(?wxVERTICAL),
	wxWindow:setSizer(Panel, MainSizer),

	Console = ?stc:new(Panel, [{id, ?ID_SHELL_TEXT_BOX}, {style, ?wxBORDER_NONE}]),
	?stc:setMarginWidth(Console, 0, 0),
	?stc:setMarginWidth(Console, 1, 0),
	?stc:setMarginWidth(Console, 2, 0),
	?stc:setMarginLeft(Console, 2),
	?stc:setLexer(Console, ?wxSTC_LEX_NULL),  
	?stc:setCaretWidth(Console, 1), 
	?stc:cmdKeyClear(Console, ?wxSTC_KEY_UP, 0),

  %% Saved styles
  {_Name, Fg, Bg, MrkrBg, ErrFg} = sys_pref_manager:get_preference(console_theme),
  set_theme(Fg, Bg, MrkrBg, ErrFg),
  set_font(sys_pref_manager:get_font(console)),
  
  %% Other styles
  % ?stc:styleSetSpec(Console, ?STYLE_PROMPT, "fore:#FF0000,bold,underline"), % For the prompt

  %% Markers
  ?stc:markerDefine(Console, ?MARKER_MSG, ?wxSTC_MARK_BACKGROUND),
  ?stc:markerSetBackground(Console, ?MARKER_MSG, MrkrBg),

	wxSizer:add(MainSizer, Console, [{flag, ?wxEXPAND}, {proportion, 1}]),

  Menu = create_menu(),

	?stc:connect(Console, key_down, [callback]),
  ?stc:connect(Console, right_up),
  ?stc:connect(Console, command_menu_selected),

  %% Add initial text
  InitText = "Erlang Evaluator (Ctrl-R to reset)\n" ++ ?PROMPT,
  ?stc:setText(Console, InitText),
  ?stc:startStyling(Console, 0, 31),
  ?stc:setStyling(Console, length(InitText) - 1, ?STYLE_PROMPT),
  
  %% Clear default key bindings
  ?stc:cmdKeyClearAll(Console),
  ?stc:cmdKeyAssign(Console, ?wxSTC_KEY_BACK, 0, ?wxSTC_CMD_DELETEBACK),
  
	%% Accelerator table
  AccelTab = wxAcceleratorTable:new(4,[wxAcceleratorEntry:new([{flags, ?wxACCEL_CTRL}, {keyCode, $R}, {cmd, ?ID_RESET_CONSOLE}]),
                                       wxAcceleratorEntry:new([{flags, ?wxACCEL_CTRL}, {keyCode, $K}, {cmd, ?ID_CLEAR_CONSOLE}]),
                                       wxAcceleratorEntry:new([{flags, ?wxACCEL_CTRL}, {keyCode, $C}, {cmd, ?wxSTC_CMD_COPY}]), %% cmdKeyAssign
                                       wxAcceleratorEntry:new([{flags, ?wxACCEL_CTRL}, {keyCode, $V}, {cmd, ?wxSTC_CMD_PASTE}])]),
  wxWindow:setAcceleratorTable(Console, AccelTab),

	State=#state{win=Panel,
				       textctrl=Console,
				       cmd_history=[],
				       current_cmd=0,
               input=[],
               menu=Menu},

  {Panel, State}.

handle_info(Msg, State) ->
  io:format("Got cast ~p~n",[Msg]),
  {noreply,State}.

handle_cast({set_theme, Fg, Bg, MrkrBg, ErrFg}, State=#state{textctrl=Console}) ->
  SetColour = fun(StyleId) ->
    ?stc:styleSetBackground(Console, StyleId, Bg),
    ?stc:styleSetForeground(Console, StyleId, Fg)
  end,
  [SetColour(Style) || Style <- [0,?wxSTC_STYLE_DEFAULT,?STYLE_PROMPT]],
  ?stc:setCaretForeground(Console, Fg),
  ?stc:markerSetBackground(Console, ?MARKER_MSG, MrkrBg),
  {noreply,State};
handle_cast({set_font, Font}, State=#state{textctrl=Console}) ->
  ?stc:styleSetFont(Console, 0, Font),
  ?stc:styleSetFont(Console, ?wxSTC_STYLE_DEFAULT, Font),
  ?stc:styleSetFont(Console, ?STYLE_PROMPT, Font),
  {noreply,State};
handle_cast({append, Response}, State=#state{textctrl=Console}) ->
  ?stc:gotoPos(Console, ?stc:getLength(Console)),
  ?stc:addText(Console, Response),
  prompt_2_console(Console, ?PROMPT),
  ?stc:gotoPos(Console, ?stc:getLength(Console)),
  {noreply, State#state{busy=false}};
handle_cast({append_msg, Msg}, State=#state{textctrl=Console}) ->
  ?stc:gotoPos(Console, ?stc:getLength(Console)),
  Line = ?stc:getCurrentLine(Console),
  ?stc:markerAdd(Console, Line, ?MARKER_MSG),
  ?stc:gotoLine(Console, Line),
  ?stc:addText(Console, Msg),
  ?stc:newLine(Console),
  ?stc:gotoPos(Console, ?stc:getLength(Console)),
  {noreply, State};
handle_cast({call_parser, Cmd, Busy}, State) ->
  console_parser:parse_input(Cmd),
  {noreply, State#state{busy=Busy, input=[]}};
handle_cast({append_input, Input}, State=#state{input=Cmd}) ->
  {noreply, State#state{input=Cmd++Input}};
handle_cast(clear, State=#state{textctrl=Console}) ->
  ?stc:clearAll(Console),
  prompt_2_console(Console, ?PROMPT, false),
  {noreply, State}.
  
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

handle_event(#wx{obj=Console, event=#wxMouse{type=right_up}},
            State=#state{menu=Menu}) ->
  wxWindow:popupMenu(Console, Menu),
	{noreply, State};
handle_event(#wx{id=?ID_RESET_CONSOLE, event=#wxCommand{type=command_menu_selected}},
            State=#state{textctrl=Console}) ->
  console_port:close_port(),
  append_message("Console reset"),
  prompt_2_console(Console, ?PROMPT),
	{noreply, State#state{busy=false}};
handle_event(#wx{id=?ID_CLEAR_CONSOLE, event=#wxCommand{type=command_menu_selected}},
            State=#state{textctrl=Console}) ->
  ?stc:clearAll(Console),
  prompt_2_console(Console, ?PROMPT, false),
  {noreply, State};
handle_event(#wx{id=?wxSTC_CMD_COPY, event=#wxCommand{type=command_menu_selected}},
            State=#state{textctrl=Console}) ->
  io:format("COPY~n"),
  ?stc:cmdKeyExecute(Console, ?wxSTC_CMD_COPY),
  {noreply, State};
handle_event(#wx{id=?wxSTC_CMD_PASTE, event=#wxCommand{type=command_menu_selected}},
            State=#state{textctrl=Console}) ->
  io:format("PASTE~n"),
  % ?stc:cmdKeyExecute(Console, ?wxSTC_CMD_PASTE),
  Cb = wxClipboard:new(),
  wxClipboard:open(Cb),
  Data = wxTextDataObject:new(),
  wxClipboard:getData(Cb, Data),
  Text = wxTextDataObject:getText(Data),
  wxClipboard:close(Cb),
  io:format("TEXT: ~p~n", [Text]),
  {noreply, State}.
% handle_event(#wx{id=Id, event=#wxCommand{type=command_menu_selected}},
%             State=#state{textctrl=Console}) when ((Id >= ?wxSTC_CMD_COPY) and (Id =< ?wxSTC_CMD_PASTE)) ->
%   io:format("COPY/PASTE~n"),
%   ?stc:cmdKeyExecute(Console, Id),
%   {noreply, State}.
    
code_change(_, _, State) ->
	{stop, not_yet_implemented, State}.

terminate(_Reason, #state{win=Frame}) ->
	io:format("TERMINATE SHELL~n"),
	wxPanel:destroy(Frame).


%% =====================================================================
%% Callback Sync event handling
%% =====================================================================

handle_sync_event(#wx{event=#wxKey{keyCode=Key, controlDown=true}}, EvtObj, #state{busy=true}) ->
  io:format("BUSY CTRL DOWN"),
  case Key of
    %% Permit even when busy
    $R -> wxEvent:skip(EvtObj);
    $K -> wxEvent:skip(EvtObj);
    $C -> wxEvent:skip(EvtObj);
    %% Discard
    _ -> ok
  end;
handle_sync_event(#wx{event=#wxKey{keyCode=?WXK_CONTROL}}, EvtObj, #state{}) ->
  io:format("CTRL DOWN~n"),
  wxEvent:skip(EvtObj);
  % case Key of
  %   $C -> wxEvent:skip(EvtObj);
  %   _ -> ok
  % end;
handle_sync_event(#wx{obj=Console, event=#wxKey{type=key_down, keyCode=13}}, EvtObj, State=#state{input=Cmd}) ->
  {Prompt,Input} = split_line_at_prompt(Console),
  ?stc:gotoPos(Console, ?stc:getLength(Console)),
  % %% (Console, EvtObj, Prompt, Input, Cmd, 0, Lc, Pc)
  % P = case length(Input) of
  %   L when L > 1 -> lists:nth(length(Input)-1, Input);
  %   _ -> undefined
  case length(Input) of
    0 ->
      prompt_2_console(Console, Prompt),
      wx_object:cast(?MODULE, {append_input, Input++"\n"});
    _ ->
      add_cmd(Input),
      process_input(Input, Cmd, Console, Prompt, EvtObj)
  end,
  ok;
%%--- Arrow keys
handle_sync_event(#wx{obj=Console, event=#wxKey{type=key_down, keyCode=?WXK_UP}}, _Event, _State) ->
  SuccessFun = fun() -> ok end,
  FailFun    = fun() -> ?stc:gotoPos(Console, ?stc:getLength(Console)) end,
  check_cursor(Console, SuccessFun, FailFun, -1),
  case cmd_index() of
    0 ->
      ok;
    _ ->
      cycle_cmd_text(Console, -1)
  end,
	ok;
handle_sync_event(#wx{obj=Console, event=#wxKey{type=key_down, keyCode=?WXK_DOWN}}, _Event, _State) ->
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
handle_sync_event(#wx{obj=Console, event=#wxKey{type=key_down, keyCode=?WXK_LEFT}}, Event, _State) ->
  check_cursor(Console, fun() -> wxEvent:skip(Event) end, fun() -> ok end, 0);
handle_sync_event(#wx{obj=Console, event=#wxKey{type=key_down, keyCode=?WXK_RIGHT}}, Event, _State) ->
  check_cursor(Console, fun() -> wxEvent:skip(Event) end, fun() -> ok end, -1);
handle_sync_event(#wx{obj=Console, event=#wxKey{type=key_down, keyCode=8}}, Event, _State) ->
  check_cursor(Console, fun() -> wxEvent:skip(Event) end, fun() -> ok end, 0);
handle_sync_event(#wx{obj=Console, event=#wxKey{type=key_down}}, Event, _State) ->
  SuccessFun = fun() -> wxEvent:skip(Event) end,
  FailFun    = fun() -> ?stc:gotoPos(Console, ?stc:getLength(Console)), wxEvent:skip(Event) end,
  check_cursor(Console, SuccessFun, FailFun, -1).


%% =====================================================================
%% Internal functions
%% =====================================================================


%% =====================================================================
%% @doc 

process_input(Input, Cmd, Console, Prompt, Event) ->
  case is_comment(Input) of
    true ->
      wx_object:cast(?MODULE, {append_input, Input ++ "\n"}),
      prompt_2_console(Console, Prompt);
    false ->
      case lists:last(Input) of
        46 ->
          wx_object:cast(?MODULE, {append_input, Input}),
          prompt_or_not(Console, Cmd++Input, Prompt, Event);
        _ ->
          wx_object:cast(?MODULE, {append_input, Input ++ "\n"}),
          prompt_2_console(Console, Prompt)
      end
  end.
  
  
%% =====================================================================
%% @doc Determine whether we need to manually prompt_2_console().
%% @private

prompt_or_not(Console, Input, Prompt, EvObj) when erlang:length(Input) > 1 ->
  Penult = lists:nth(length(Input)-1, Input),
	if
		Penult =:= $. ->
      %wx_object:cast(?MODULE, {call_parser, Input, false}),
			prompt_2_console(Console, Prompt),
			wxEvent:stopPropagation(EvObj);
		true ->
      case count_chars(34, Input) andalso count_chars(39, Input) of %% 34 = ", 39 = '
        true ->
          wxEvent:skip(EvObj),
          wx_object:cast(?MODULE, {call_parser, Input, true});
        false ->
          wx_object:cast(?MODULE, {append_input, "\n"}),
          prompt_2_console(Console, Prompt)
      end
	end;

prompt_or_not(Console, Input, Prompt, EvObj) ->
  io:format("Input <= 1~n"),
  case Input of
    [46] ->
      prompt_2_console(Console, Prompt),
      wx_object:cast(?MODULE, {call_parser, Input, false});
    _ ->
      wxEvent:skip(EvObj)
  end.
  

%% =====================================================================
%% @doc Write a newline plus the prompt to the console.

prompt_2_console(Console, Prompt) ->
  prompt_2_console(Console, Prompt, true).
  

%% =====================================================================
%% @doc Write an optional newline plus the prompt to the console.
 
prompt_2_console(Console, Prompt, Newline) ->
  case Newline of
    true -> ?stc:newLine(Console);
    false -> ok
  end,
  ?stc:addText(Console, Prompt),
  Start = ?stc:positionFromLine(Console, ?stc:getCurrentLine(Console)),
  ?stc:startStyling(Console, Start, 31),
  ?stc:setStyling(Console, length(Prompt) - 1, ?STYLE_PROMPT),
  ok.


%% =====================================================================
%% @doc Return true if the number of characters is even. If the character
%% is escaped, it does not count.

count_chars(Char, String) ->
  count_chars(Char, String, 0, false).
  
count_chars(_Char, [], Acc, _) ->
  case Acc rem 2 of
    0 ->
      true;
    _ ->
      false
  end;
count_chars(Char, [H|T], Acc, Escape) ->
  case H of 
    Char ->
      case Escape of
        true ->
          count_chars(Char, T, Acc, false);
        false ->
          count_chars(Char, T, Acc+1, false)
      end;
    92 -> %% "escape char \"
      count_chars(Char, T, Acc, not Escape);
    _ ->
      count_chars(Char, T, Acc, Escape)
  end.
  

%% =====================================================================
%% @doc Check for comment character in command.

is_comment([]) ->
  false;
is_comment([H|T]) ->
  case H of
    '%' ->
      true;
    _ ->
      is_comment(T)
  end.


%% =====================================================================
%% @doc Separate the prompt and command on the most recent line.

split_line_at_prompt(Console) ->
  lists:split(length(?PROMPT), %%get_cur_prompt_length(Console),
    ?stc:getLine(Console, ?stc:getLineCount(Console) - 1)).


%% =====================================================================
%% @doc Get the length of the current prompt.

get_cur_prompt_length(Console) ->
  get_prompt_length(?stc:getLine(Console, ?stc:getLineCount(Console) - 1)).


%% =====================================================================
%% @doc Get the length of the prompt.

get_prompt_length(Line) ->
	get_prompt_length(Line, 1).

get_prompt_length([], _) -> 0;
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
%% @doc Build the popup "right click" menu.

create_menu() ->
  Menu = wxMenu:new([]),
  wxMenu:append(Menu, ?wxSTC_CMD_COPY, "Copy\tCtrl+C", []),
  wxMenu:append(Menu, ?wxSTC_CMD_PASTE, "Paste\tCtrl+V", []),
  wxMenu:appendSeparator(Menu),
  wxMenu:append(Menu, ?ID_RESET_CONSOLE, "Reset Console\tCtrl+R", []),
  wxMenu:append(Menu, ?ID_CLEAR_CONSOLE, "Clear All\tCtrl+K", []),
  wxMenu:connect(Menu, command_menu_selected),
  Menu.
