%% =====================================================================
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%% 
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%% 
%% You should have received a copy of the GNU General Public License
%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%
%% @author Tom Richmond <tr201@kent.ac.uk>
%% @author Mike Quested <mdq3@kent.ac.uk>
%% @copyright Tom Richmond, Mike Quested 2014
%%
%% @doc This module initalises the console, which is currently
%% implemented as a wxStyledTextCtrl.
%% In the current implementation we manage the prompt ourselves
%% (deverl_console_parser strips the prompt returned from the port), so that
%% we can write to the console anywhere we like without having to
%% worry about deleting/re-numbering prompts.
%% We settled on this current implementation as a comprimise (in the
%% original implementation everything received from the port was
%% written to the console). The optimal solution would be to use the
%% erl_parse/erl_eval/erl_scan APIs in the same way that shell.erl does
%% from std lib. We may well use that approach in the future.
%% @end
%% =====================================================================

-module(deverl_console_wx).

-include_lib("wx/include/wx.hrl").
-include("deverl.hrl").

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
         append_message/2,
         set_theme/4,
         set_font/1,
         clear/0,
         paste/1,
         destroy/0]).

%% Macros
-define(stc, wxStyledTextCtrl).
-define(ID_RESET_CONSOLE, 1).
-define(ID_CLEAR_CONSOLE, 2).
-define(PROMPT, "> ").
-define(CONSOLE_HEADER, "Erlang Evaluator (Alt-R to reset, Alt-K to clear)\n").
-define(STYLE_PROMPT, 0).
-define(STYLE_ERROR, 1).
-define(MARKER_MSG, 1).

%% Server state
-record(state, {win,
								console,
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

-spec new(Config) -> wxWindow:wxWindow() when
  Config :: list().

new(Config) ->
	wx_object:start_link({local, ?MODULE}, ?MODULE, Config, []).


%% =====================================================================
%% @doc

-spec append_command(string()) -> ok.

append_command(Response) ->
  wx_object:cast(?MODULE, {append, Response}).


%% =====================================================================
%% @doc

-spec append_message(string()) -> ok.

append_message(Msg) ->
  append_message(Msg, true).

-spec append_message(string(), boolean()) -> ok.

append_message(Msg, Bool) ->
  wx_object:cast(?MODULE, {append_msg, Msg, Bool}).


%% =====================================================================
%% @doc Update the shell's theme

-spec set_theme(wxColour:wxColour(), wxColour:wxColour(), wxColour:wxColour(), wxColour:wxColour()) -> ok.

set_theme(Fg, Bg, MrkrBg, ErrFg) ->
  wx_object:cast(?MODULE, {set_theme, Fg, Bg, MrkrBg, ErrFg}).


%% =====================================================================
%% @doc

-spec set_font(wxFont:wxFont()) -> ok.

set_font(Font) ->
  wx_object:cast(?MODULE, {set_font, Font}).


%% =====================================================================
%% @doc

-spec clear() -> ok.

clear() ->
  wx_object:cast(?MODULE, clear).


%% =====================================================================
%% @doc

-spec paste(wxStyledTextCtrl:wxStyledTextCtrl()) -> ok.

paste(This) ->
  Split = re:split(get_clipboard_text(), "\\R", [{newline, any}, {return, list}, trim]),
  Fn = fun(_Fn1, []) ->
            ok;
          (_Fn1, [E]) ->
            wait(),
            ?stc:gotoPos(This, ?stc:getLength(This)),
            ?stc:addText(This, E);
          (Fn1, [H|T]) ->
            wait(),
            wx_object:call(?MODULE, {paste, H}),
            Fn1(Fn1, T)
  end,
  Fn(Fn, Split),
  ok.

%% =====================================================================
%% @doc Destroy.

destroy() ->
  wx_object:call(?MODULE, shutdown).
  
  
%% =====================================================================
%% Callback functions
%% =====================================================================
%% @hidden
init(Config) ->
	Parent = proplists:get_value(parent, Config),
	Panel = wxPanel:new(Parent, [{winid, ?WINDOW_CONSOLE}]),
	MainSizer = wxBoxSizer:new(?wxVERTICAL),
	wxWindow:setSizer(Panel, MainSizer),

	Console = ?stc:new(Panel, [{id, ?CONSOLE}, {style, ?wxBORDER_NONE}]),
	?stc:setMarginWidth(Console, 0, 0),
	?stc:setMarginWidth(Console, 1, 0),
	?stc:setMarginWidth(Console, 2, 0),
	?stc:setMarginLeft(Console, 2),
	?stc:setLexer(Console, ?wxSTC_LEX_NULL),
	?stc:setCaretWidth(Console, 1),
	?stc:cmdKeyClear(Console, ?wxSTC_KEY_UP, 0),

  %% Saved styles
  {_Name, Fg, Bg, MrkrBg, ErrFg} = deverl_sys_pref_gen:get_preference(console_theme),
  set_theme(Fg, Bg, MrkrBg, ErrFg),
  set_font(deverl_sys_pref_gen:get_font(console_font)),

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
  ?stc:connect(Console, kill_focus, [{skip, true}]),
  ?stc:connect(Console, set_focus, [{skip, true}]),
  ?stc:connect(Console, stc_updateui),

  %% Add initial text
  InitText = ?CONSOLE_HEADER ++ ?PROMPT,
  ?stc:setText(Console, InitText),
  ?stc:startStyling(Console, 0, 31),
  ?stc:setStyling(Console, length(InitText) - 1, ?STYLE_PROMPT),

  %% Clear default key bindings
  ?stc:cmdKeyClearAll(Console),
  ?stc:cmdKeyAssign(Console, ?wxSTC_KEY_BACK, 0, ?wxSTC_CMD_DELETEBACK),
  ?stc:cmdKeyAssign(Console, ?wxSTC_KEY_LEFT, 0 ,?wxSTC_CMD_CHARLEFT),
  ?stc:cmdKeyAssign(Console, ?wxSTC_KEY_RIGHT, 0 ,?wxSTC_CMD_CHARRIGHT),

	%% Accelerator table
  AccelTab = wxAcceleratorTable:new(4,[wxAcceleratorEntry:new([{flags, ?wxACCEL_ALT}, {keyCode, $R}, {cmd, ?ID_RESET_CONSOLE}]),
                                       wxAcceleratorEntry:new([{flags, ?wxACCEL_ALT}, {keyCode, $K}, {cmd, ?ID_CLEAR_CONSOLE}])]),
  wxWindow:setAcceleratorTable(Console, AccelTab),

	State=#state{win=Panel,
				       console=Console,
				       cmd_history=[],
				       current_cmd=0,
               input=[],
               menu=Menu,
               busy=false},

  {Panel, State}.
%% @hidden
handle_info(Msg, State) ->
  io:format("Got cast ~p~n",[Msg]),
  {noreply,State}.
%% @hidden
handle_cast({set_theme, Fg, Bg, MrkrBg, _ErrFg}, State=#state{console=Console}) ->
  SetColour = fun(StyleId) ->
    ?stc:styleSetBackground(Console, StyleId, Bg),
    ?stc:styleSetForeground(Console, StyleId, Fg)
  end,
  [SetColour(Style) || Style <- [0,?wxSTC_STYLE_DEFAULT,?STYLE_PROMPT]],
  ?stc:setCaretForeground(Console, Fg),
  ?stc:markerSetBackground(Console, ?MARKER_MSG, MrkrBg),
  {noreply,State};
handle_cast({set_font, Font}, State=#state{console=Console}) ->
  ?stc:styleSetFont(Console, 0, Font),
  ?stc:styleSetFont(Console, ?wxSTC_STYLE_DEFAULT, Font),
  ?stc:styleSetFont(Console, ?STYLE_PROMPT, Font),
  {noreply,State};
handle_cast({append, {response, complete}}, State=#state{console=Console}) ->
  prompt_2_console(Console, ?PROMPT, false),
  {noreply, State#state{busy=false}};
handle_cast({append, {response, Response}}, State=#state{console=Console}) ->
  ?stc:gotoPos(Console, ?stc:getLength(Console)),
  ?stc:addText(Console, Response),
  ?stc:gotoPos(Console, ?stc:getLength(Console)),
  {noreply, State};
handle_cast({append_msg, Msg, Prompt}, State=#state{console=Console}) ->
  ?stc:freeze(Console),
  ?stc:gotoPos(Console, ?stc:getLength(Console)),
  case Prompt of
    false ->
      Start = ?stc:positionFromLine(Console, ?stc:getCurrentLine(Console)),
      End = ?stc:getLineEndPosition(Console, ?stc:getCurrentLine(Console)),
      ?stc:setTargetStart(Console, Start),
      ?stc:setTargetEnd(Console, End),
      ?stc:replaceTarget(Console, "");
    _ -> ok
  end,
  ?stc:gotoPos(Console, ?stc:getLength(Console)),
  Line = ?stc:getCurrentLine(Console),
  ?stc:markerAdd(Console, Line, ?MARKER_MSG),
  ?stc:gotoLine(Console, Line),
  ?stc:addText(Console, Msg),
  ?stc:newLine(Console),
  ?stc:gotoPos(Console, ?stc:getLength(Console)),
  ?stc:thaw(Console),
  {noreply, State#state{busy=not Prompt}};
handle_cast({call_parser, Cmd, Busy}, State) ->
  deverl_console_parser:parse_input(Cmd),
  {noreply, State#state{busy=Busy, input=[]}};
handle_cast({append_input, Input}, State=#state{input=Cmd}) ->
  {noreply, State#state{input=Cmd++Input}};
handle_cast(clear, State=#state{console=Console, busy=Busy}) ->
  ?stc:clearAll(Console),
  case Busy of
    true ->
      ok;
    false ->
      prompt_2_console(Console, ?PROMPT, false)
  end,
  {noreply, State};
handle_cast(eval, State=#state{console=Console, input=Cmd, cmd_history=Hst0, current_cmd=Idx0}) ->
  {Hst1, Idx1} = case eval(Console, Cmd, Hst0) of
    ok -> {Hst0, Idx0};
    Upt -> Upt
  end,
  {noreply, State#state{cmd_history=Hst1, current_cmd=Idx1}}.
%% @hidden
handle_call({update_cmd_index, Index}, _From, State) ->
  {reply, ok, State#state{current_cmd=Index}};
handle_call({paste, Line}, _From, State=#state{console=Console, input=Cmd, cmd_history=Hst0, current_cmd=Idx0}) ->
  ?stc:gotoPos(Console, ?stc:getLength(Console)),
  ?stc:addText(Console, Line),
  {Hst1, Idx1} = case eval(Console, Cmd, Hst0) of
    ok -> {Hst0, Idx0};
    Upt -> Upt
  end,
  {reply, ok, State#state{cmd_history=Hst1, current_cmd=Idx1}};
handle_call(busy, _From, State=#state{busy=Busy}) ->
  {reply, Busy, State};
handle_call(shutdown, _From, State) ->
  {stop, normal, ok, State}.
%% @hidden
handle_event(#wx{obj=Console, event=#wxMouse{type=right_up}},
            State=#state{menu=Menu}) ->
  wxWindow:popupMenu(Console, Menu),
	{noreply, State};
handle_event(#wx{id=?ID_RESET_CONSOLE, event=#wxCommand{type=command_menu_selected}},
            State=#state{console=Console}) ->
  deverl_console_port_gen:close_port(),
  append_message("Console reset"),
  prompt_2_console(Console, ?PROMPT),
	{noreply, State#state{busy=false, input=[]}};
handle_event(#wx{id=?ID_CLEAR_CONSOLE, event=#wxCommand{type=command_menu_selected}},
            State) ->
  clear(),
  {noreply, State};
handle_event(#wx{id=?wxID_COPY, event=#wxCommand{type=command_menu_selected}},
            State=#state{console=Console}) ->
  ?stc:cmdKeyExecute(Console, ?wxSTC_CMD_COPY),
  {noreply, State};
handle_event(#wx{id=?wxID_PASTE, event=#wxCommand{type=command_menu_selected}},
            State=#state{console=Console}) ->
              Env = wx:get_env(),
  spawn(fun() -> wx:set_env(Env), paste(Console) end),
  {noreply, State};
handle_event(#wx{event=#wxFocus{type=set_focus}}, State) ->
  %% Enable undo/redo
  deverl:toggle_menu_items([?wxID_COPY, ?wxID_PASTE, ?wxID_SELECTALL], true),
  {noreply, State};

handle_event(#wx{event=#wxFocus{type=kill_focus}}, State) ->
  %% Disable undo
  deverl:toggle_menu_items([?wxID_COPY, ?wxID_PASTE, ?wxID_SELECTALL], false),
  {noreply, State};
  
handle_event(#wx{event=#wxStyledText{type=stc_updateui}},State=#state{console=Console}) ->
  case ?stc:getSelection(Console) of
    {N,N} -> 
      deverl:toggle_menu_items([?wxID_COPY], false);
    _ -> 
      deverl:toggle_menu_items([?wxID_COPY], true)
  end,
  {noreply, State}.
%% @hidden
code_change(_, _, State) ->
	{ok, State}.
%% @hidden
terminate(_Reason, #state{win=Frame}) ->
	wxPanel:destroy(Frame).


%% =====================================================================
%% Callback Sync event handling
%% =====================================================================

handle_sync_event(#wx{event=#wxKey{keyCode=Key, controlDown=true}}, EvtObj, #state{busy=true}) ->
  case Key of
    %% Permit even when busy
    $C -> wxEvent:skip(EvtObj);
    %% Discard
    _ -> ok
  end;
handle_sync_event(#wx{event=#wxKey{keyCode=Key, altDown=true}}, EvtObj, #state{busy=true}) ->
  case Key of
    %% Permit even when busy
    $R -> wxEvent:skip(EvtObj);
    $K -> wxEvent:skip(EvtObj);
    %% Discard
    _ -> ok
  end;

handle_sync_event(#wx{event=#wxKey{}}, _EvtObj, #state{busy=true}) ->
  ok;
handle_sync_event(#wx{event=#wxKey{keyCode=?WXK_CONTROL}}, EvtObj, #state{}) ->
  wxEvent:skip(EvtObj);
handle_sync_event(#wx{event=#wxKey{controlDown=true}}, EvtObj, #state{}) ->
  wxEvent:skip(EvtObj);
handle_sync_event(#wx{event=#wxKey{type=key_down, keyCode=13}}, _EvtObj, #state{}) ->
  wx_object:cast(?MODULE, eval),
  ok;
%%--- Arrow keys
handle_sync_event(#wx{obj=Console, event=#wxKey{type=key_down, keyCode=?WXK_UP}}, _Event,
                  #state{current_cmd=Idx, cmd_history=Hst}) ->
  SuccessFun = fun() -> ok end,
  FailFun    = fun() -> ?stc:gotoPos(Console, ?stc:getLength(Console)) end,
  check_cursor(Console, SuccessFun, FailFun, -1),
  case Idx of
    0 ->
      ok;
    _ ->
      cycle_cmd_text(Console, -1, Hst, Idx)

  end,
	ok;
handle_sync_event(#wx{obj=Console, event=#wxKey{type=key_down, keyCode=?WXK_DOWN}}, _Event,
                  #state{cmd_history=Hst, current_cmd=Idx}) ->
  SuccessFun = fun() -> ok end,
  FailFun    = fun() -> ?stc:gotoPos(Console, ?stc:getLength(Console)) end,
  check_cursor(Console, SuccessFun, FailFun, -1),
  LastCmd = length(Hst) - 1,
  Limit = LastCmd + 1,
  case Idx of
    LastCmd ->
      replace_cmd_text(Console, ""),
      update_cmd_index(Idx+1);
    Limit ->
      ok;
    _ ->
      cycle_cmd_text(Console, 1, Hst, Idx)
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

-spec eval(wxStyledTextCtrl:wxStyledTextCtrl(), string(), [string()]) ->  ok | {[string()], integer()}.

eval(Console, Cmd, Hst) ->
  {Prompt,Input} = split_line_at_prompt(Console),
  ?stc:gotoPos(Console, ?stc:getLength(Console)),
  eval(Console, {Prompt, Input}, Cmd, Hst).

-spec eval(wxStyledTextCtrl:wxStyledTextCtrl(), {string(), string()}, string(), [string()]) -> ok | {[string()], integer()}.

eval(Console, {Prompt, Input}, _Cmd, _Hst) when length(Input) =:= 0 ->
  prompt(Console, Prompt, Input++"\n"); %% for error's line no
eval(Console, {_Prompt, [$.]=Input}, [], _Hst) -> %% single .
  ?stc:newLine(Console),
  wx_object:cast(?MODULE, {call_parser, Input, true});
eval(Console, {Prompt, Input}, Cmd0, Hst) ->
  case lists:last(Input) of
    $. ->
      wx_object:cast(?MODULE, {append_input, Input}),
      Cmd1 = Cmd0++Input,
      prompt(Console, Cmd1, Input, Prompt, lists:nth(length(Cmd1)-1, Cmd1));
    _ ->
      prompt(Console, Prompt, Input++"\n")
  end,
  add_cmd(Input, Hst).


%% =====================================================================
%% @doc Determine whether we need to manually prompt_2_console().
%% @private

-spec prompt(wxStyledTextCtrl:wxStyledTextCtrl(), string(), string(), string(), integer()) -> ok.

prompt(Console, Cmd, "$..", _Prompt, _) ->
    wx_object:cast(?MODULE, {call_parser, Cmd, false}),
    ?stc:newLine(Console);
prompt(Console, Cmd, "$$.", _Prompt, _) ->
    wx_object:cast(?MODULE, {call_parser, Cmd, false}),
    ?stc:newLine(Console);
prompt(Console, Cmd, Input, Prompt, $$) -> % $. (Ascii shortcut)
  case length(Cmd++Input) of
    2 ->
      wx_object:cast(?MODULE, {call_parser, Cmd, false}),
      prompt(Console, Prompt, "\n");
    _ ->
      prompt(Console, Prompt, "\n")
  end;
prompt(Console, Cmd, Input, Prompt, $.) -> %% Multiple trailing dots
  {match, [{_,N}]} = re:run(Input, "[.]*$", []), %% Count
  case ((N - 1) rem 3) of % 1,4,7,10...
    0 -> %% syntax error from shell
      ?stc:newLine(Console),
      wx_object:cast(?MODULE, {call_parser, Cmd, false});
    _ ->
      prompt(Console, Prompt, "\n")
  end,
  ok;
prompt(Console, Cmd, _Input, Prompt, _) ->
  case count_chars($", Cmd) andalso count_chars($', Cmd) of
    true ->
      ?stc:newLine(Console),
      wx_object:cast(?MODULE, {call_parser, Cmd, true});
    false ->
      prompt(Console, Prompt, "\n")
  end.

-spec prompt(wxStyledTextCtrl:wxStyledTextCtrl(), string(), string()) -> ok.

prompt(Console, Prompt, Str) ->
  wx_object:cast(?MODULE, {append_input, Str}),
  prompt_2_console(Console, Prompt).

%% =====================================================================
%% @doc Write a newline plus the prompt to the console.

-spec prompt_2_console(wxStyledTextCtrl:wxStyledTextCtrl(), string()) -> ok.

prompt_2_console(Console, Prompt) ->
  prompt_2_console(Console, Prompt, true).


%% =====================================================================
%% @doc Write an optional newline plus the prompt to the console.

-spec prompt_2_console(wxStyledTextCtrl:wxStyledTextCtrl(), string(), boolean()) -> ok.

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

-spec count_chars(integer(), string()) -> boolean().

count_chars(Char, String) ->
  count_chars(Char, String, 0, false).

-spec count_chars(integer(), string(), integer(), boolean()) -> boolean().

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
    $\\ -> %% "escape char \"
      count_chars(Char, T, Acc, not Escape);
    _ ->
      count_chars(Char, T, Acc, false)
  end.


%% =====================================================================
%% @doc Separate the prompt and command on the most recent line.

-spec split_line_at_prompt(wxStyledTextCtrl:wxStyledTextCtrl()) -> {list(), list()}.

split_line_at_prompt(Console) ->
  lists:split(length(?PROMPT), %%get_cur_prompt_length(Console),
    ?stc:getLine(Console, ?stc:getLineCount(Console) - 1)).


%% =====================================================================
%% @doc Get the length of the current prompt.

-spec get_cur_prompt_length(wxStyledTextCtrl:wxStyledTextCtrl()) -> integer().

get_cur_prompt_length(Console) ->
  get_prompt_length(?stc:getLine(Console, ?stc:getLineCount(Console) - 1)).


%% =====================================================================
%% @doc Get the length of the prompt.

-spec get_prompt_length(string()) -> non_neg_integer().

get_prompt_length(Line) ->
	get_prompt_length(Line, 1).

-spec get_prompt_length(string(), integer()) -> integer().

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

-spec get_line_start_pos(wxStyledTextCtrl:wxStyledTextCtrl(), integer()) -> integer().

get_line_start_pos(Console, Pos) ->
  ?stc:positionFromLine(Console, ?stc:lineFromPosition(Console, Pos)).


%% =====================================================================
%% @doc Append new command to end of command history iff last element not same as new element.

-spec add_cmd(string(), [string()]) -> {[string()], integer()}.

add_cmd(Cmd, Hst) when length(Hst) =:= 0 ->
  add_cmd(insert, Cmd, Hst);
add_cmd(Cmd, Hst) ->
  case Cmd =:= get_cmd(Hst, length(Hst) - 1) of
    true ->
      {Hst, length(Hst)};
    false ->
      add_cmd(insert, Cmd, Hst)
  end.
add_cmd(insert, Cmd, Hst) ->
  {Hst++[Cmd], length(Hst)+1}.


%% =====================================================================
%% @doc Cycle through command history by one entry.
%% param Direction: -1 for prev cmd, +1 for next cmd.

-spec cycle_cmd_text(wxStyledTextCtrl:wxStyledTextCtrl(), Direction, [string()], integer()) -> ok when
  Direction :: -1 | 1.

cycle_cmd_text(Console, Direction, Hst, Idx) ->
  update_cmd_index(Idx + Direction),
	Cmd = get_cmd(Hst, Idx + Direction),
  replace_cmd_text(Console, Cmd).


%% =====================================================================
%% @doc Replace the current command with the updated command Cmd.
%% The replacement will always take place on the most recent line.

-spec replace_cmd_text(wxStyledTextCtrl:wxStyledTextCtrl(), string()) -> ok.

replace_cmd_text(Console, Cmd) ->
  ?stc:setSelection(Console, get_cur_prompt_length(Console) +
    get_line_start_pos(Console, ?stc:getLength(Console)),
      ?stc:getLength(Console)),
  ?stc:replaceSelection(Console, Cmd).


% %% =====================================================================
% %% @doc

-spec update_cmd_index(integer()) -> ok.

update_cmd_index(NewIndex) ->
  wx_object:call(?MODULE, {update_cmd_index, NewIndex}).


%% =====================================================================
%% @doc Retrieve command from history based on indexed position.

-spec get_cmd([string()], integer()) -> string().

get_cmd(Hst, Idx) ->
  lists:nth(Idx+1, Hst).


%% =====================================================================
%% @doc Check cursor is in valid position, and execute appropriate function.

-spec check_cursor(wxStyledTextCtrl:wxStyledTextCtrl(), function(), function(), integer()) -> ok.

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

-spec create_menu() -> wxMenu:wxMenu().

create_menu() ->
  Menu = wxMenu:new([]),
  wxMenu:append(Menu, ?wxID_COPY, "Copy\tCtrl+C", []),
  wxMenu:append(Menu, ?wxID_PASTE, "Paste\tCtrl+V", []),
  wxMenu:appendSeparator(Menu),
  wxMenu:append(Menu, ?ID_RESET_CONSOLE, "Reset Console\tAlt+R", []),
  wxMenu:append(Menu, ?ID_CLEAR_CONSOLE, "Clear All\tAlt+K", []),
  wxMenu:connect(Menu, command_menu_selected),
  Menu.


%% =====================================================================
%% @doc

-spec get_clipboard_text() -> string().

get_clipboard_text() ->
  Cb = wxClipboard:new(),
  wxClipboard:open(Cb),
  Data = wxTextDataObject:new(),
  wxClipboard:getData(Cb, Data),
  Text = wxTextDataObject:getText(Data),
  wxClipboard:close(Cb),
  Text.


%% =====================================================================
%% @doc

-spec wait() -> ok.

wait() ->
  case wx_object:call(?MODULE, busy) of
    true -> wait();
    false -> ok
  end.
