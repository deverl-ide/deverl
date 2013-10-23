%% =====================================================================
%% @author
%% @copyright
%% @title
%% @version
%% @doc This module creates a code editor instance.
%% @end
%% =====================================================================

- module(editor).

-include_lib("wx/include/wx.hrl").

%% wx_object
-behaviour(wx_object).
-export([
  init/1, 
  terminate/2,  
  code_change/3,
  handle_info/2,
  handle_call/3,
  handle_cast/2,
  handle_event/2]).

%% API
-export([
	start/1,
	is_dirty/1,
  set_savepoint/1,
  get_text/1,
  selected/2,
  find/2,
  find_all/2,
  replace_all/3,
	set_text/2,
	set_theme/3,
	set_font_style/2,
	set_tab_width/2,
	set_use_tabs/2,
	set_indent_guides/2,
	set_line_wrap/2,
	set_line_margin_visible/2,
	indent_line_left/1,
	indent_line_right/1,
	comment/1,
	go_to_position/2,
	get_current_pos/1,
	zoom_in/1,
	zoom_out/1,
	transform_uc_selection/1,
	transform_lc_selection/1,
	transform_selection/2,
	fn_list/2,
	link_poller/2,
	empty_undo_buffer/1
	]).

%% Macros	

-define(LEFT_MARGIN_WIDTH, 6).
-define(RIGHT_MARGIN_WIDTH, 6).
-define(MARGIN_LN_PT_OFFSET, 0). %% The size (pts) to reduce margin text by

%% For wx-2.9 usage
-ifndef(wxSTC_ERLANG_COMMENT_FUNCTION).
-define(wxSTC_ERLANG_COMMENT_FUNCTION, 14).
-define(wxSTC_ERLANG_COMMENT_MODULE, 15).
-define(wxSTC_ERLANG_COMMENT_DOC, 16).
-define(wxSTC_ERLANG_COMMENT_DOC_MACRO, 17).
-define(wxSTC_ERLANG_ATOM_QUOTED, 18).
-define(wxSTC_ERLANG_MACRO_QUOTED, 19).
-define(wxSTC_ERLANG_RECORD_QUOTED, 20).
-define(wxSTC_ERLANG_NODE_NAME_QUOTED, 21).
-define(wxSTC_ERLANG_BIFS, 22).
-define(wxSTC_ERLANG_MODULES, 23).
-define(wxSTC_ERLANG_MODULES_ATT, 24).
-endif.

%% Types
-type path()      :: string().
-type filename()  :: string().
-type erlangEditor() :: wxWindow:wxWindow().

%% Server state
-record(state, {parent_panel    :: erlangEditor(), 
                stc          :: wxStyledTextCtrl:wxStyledTextCtrl(),
								dirty :: boolean(),
								func_list,
								test_list
               }).


%% =====================================================================
%% Client API
%% =====================================================================

%% =====================================================================
%% @doc Start a new editor process

start(Config) ->
  wx_object:start(?MODULE, Config, []).


%% =====================================================================
%% @doc Get the status of the document

is_dirty(Editor) ->
  wx_object:call(Editor, is_dirty).
	

%% ===================================================================== 
%% @doc Add a savepoint: update the document state to unmodified

set_savepoint(EditorPid) ->
	wxStyledTextCtrl:setSavePoint(wx_object:call(EditorPid, stc)).
	

%% ===================================================================== 
%% @doc Get the text from the editor

-spec get_text(EditorPid) -> Result when
  EditorPid :: pid(),
  Result :: string().

get_text(EditorPid) ->
  wx_object:call(EditorPid, text_content).  
	

%% =====================================================================  
%% @doc Notify the editor it has been selected

selected(EditorPid, Sb) ->
	Editor = wx_object:call(EditorPid, stc),
	update_sb_line(Editor, Sb).


%% =====================================================================
%% Find and replace
%% =====================================================================

find(EditorPid, Str) ->
  TextCtrl = wx_object:call(EditorPid, stc),
  Pos = wxStyledTextCtrl:findText(TextCtrl, 0, 
		wxStyledTextCtrl:getLength(TextCtrl), Str, [{flags, ?wxSTC_FIND_WHOLEWORD}]),
	io:format("Pos: ~p~n", [position_to_x_y(TextCtrl, Pos)]).

%% ===================================================================== 
%% @doc Search/replace with next/prev

% find(EditorPid, Str) ->
%   TextCtrl = wx_object:call(EditorPid, stc),
%   Pos = wxStyledTextCtrl:findText(TextCtrl, 0, 
% 		wxStyledTextCtrl:getLength(TextCtrl), Str, [{flags, ?wxSTC_FIND_WHOLEWORD}]),
%   wxStyledTextCtrl:startStyling(TextCtrl, Pos, ?wxSTC_INDICS_MASK),
%   wxStyledTextCtrl:setStyling(TextCtrl, length(Str), ?wxSTC_INDIC0_MASK).
%   %% Bookmark line and add indicator to word


%% ===================================================================== 
%% @doc Find all occurrences of Str.

find_all(EditorPid, Str) ->
  Editor = wx_object:call(EditorPid, stc),
  find(Editor, Str, 0, wxStyledTextCtrl:getLength(Editor)).


%% ===================================================================== 
%% @doc Replace all occurrences of str with RepStr.

replace_all(EditorPid, Str, RepStr) ->
    Editor = wx_object:call(EditorPid, stc),
    replace_all(Editor, Str, RepStr, 0, wxStyledTextCtrl:getLength(Editor)).


%% =====================================================================
%% @doc Set the text.

set_text(This, Text) ->
	Editor = wx_object:call(This, stc),
	wxStyledTextCtrl:setText(Editor, Text),	
	update_line_margin(Editor).

%% =====================================================================
%% Styling
%% =====================================================================

%% =====================================================================
%% @doc Change the theme of the editor.

set_theme(Editor, Theme, Font) ->
	case theme:load_theme(Theme) of
		{error, load_theme} -> ok;
		NewTheme -> setup_theme(wx_object:call(Editor, stc), NewTheme, Font)
	end,
	ok.


%% =====================================================================
%% @doc Update the font used in the editor

-spec set_font_style(EditorPid, Font) -> 'ok' when
  EditorPid :: pid(),
  Font :: wxFont:wxFont().

set_font_style(EditorPid, Font) when is_pid(EditorPid) -> 
  Editor = wx_object:call(EditorPid, stc),
  set_font_style(Editor, Font);

%% @private
set_font_style(Editor, Font) ->
	Update = fun(Id) -> 
		wxStyledTextCtrl:styleSetFont(Editor, Id, Font)
		end,
	Update(?wxSTC_STYLE_DEFAULT),
	% wxStyledTextCtrl:styleClearAll(Editor), Needed to ensure all styles are resized
	[Update(Id) || Id <- lists:seq(?wxSTC_ERLANG_DEFAULT, ?wxSTC_ERLANG_MODULES_ATT)],
	update_line_margin(Editor),
	ok.
	

%% =====================================================================
%% @doc

set_tab_width(EditorPid, Width) ->
	wxStyledTextCtrl:setTabWidth(wx_object:call(EditorPid, stc), Width).

	
%% =====================================================================
%% @doc

set_use_tabs(EditorPid, Bool) ->
	wxStyledTextCtrl:setUseTabs(wx_object:call(EditorPid, stc), Bool).


%% =====================================================================
%% @doc

set_indent_guides(EditorPid, Bool) ->
	wxStyledTextCtrl:setIndentationGuides(wx_object:call(EditorPid, stc), Bool),
	ok.

%% =====================================================================
%% @doc

set_line_wrap(EditorPid, Bool) ->
	Result = case Bool of
		true -> 1;
		_ -> 0
	end,
	wxStyledTextCtrl:setWrapMode(wx_object:call(EditorPid, stc), Result).


%% =====================================================================
%% @doc

set_line_margin_visible(EditorPid, Bool) ->
	Editor = wx_object:call(EditorPid, stc),
	case Bool of
		true -> set_linenumber_default(Editor, user_prefs:get_user_pref({pref, font}));
		false -> wxStyledTextCtrl:setMarginWidth(Editor, 0, 0)
	end.			
	
	
%% =====================================================================
%% @doc

indent_line_left(EditorPid) ->
	Editor = wx_object:call(EditorPid, stc),
	wxStyledTextCtrl:cmdKeyExecute(Editor, ?wxSTC_CMD_BACKTAB),
	ok.


%% =====================================================================
%% @doc

indent_line_right(EditorPid) ->
	Editor = wx_object:call(EditorPid, stc),
	wxStyledTextCtrl:cmdKeyExecute(Editor, ?wxSTC_CMD_TAB),
	ok.


%% =====================================================================
%% @doc

comment(EditorPid) ->
	Editor = wx_object:call(EditorPid, stc),
	case wxStyledTextCtrl:getSelection(Editor) of
		{N,N} -> single_line_comment(Editor);
		{N,M} ->
			Sel = count_selected_lines(Editor),
			{Result,Length} = case count_commented_lines_re(Editor) of
				Sel -> remove_comments(wxStyledTextCtrl:getSelectedText(Editor));
				_ -> insert_comments(wxStyledTextCtrl:getSelectedText(Editor))
			end,
			wxStyledTextCtrl:replaceSelection(Editor, Result),
			wxStyledTextCtrl:setSelectionStart(Editor, N),
			wxStyledTextCtrl:setSelectionEnd(Editor, N + Length),
			correct_caret(Editor, wxStyledTextCtrl:getCurrentPos(Editor))
	end,
	ok.
	
	
%% =====================================================================
%% @doc Move the caret to the line Line and Column Col.

go_to_position(This, Pos) ->
	Editor = wx_object:cast(This, {goto_pos, Pos}).


%% =====================================================================
%% @doc

get_current_pos(This) ->
	Stc = wx_object:call(This, stc),
	position_to_x_y(Stc, wxStyledTextCtrl:getCurrentPos(Stc)).	
	

%% =====================================================================
%% @doc
	
zoom_in(EditorPid) ->
	Editor = wx_object:call(EditorPid, stc),
	wxStyledTextCtrl:cmdKeyExecute(Editor, ?wxSTC_CMD_ZOOMIN).


%% =====================================================================
%% @doc

zoom_out(EditorPid) ->
	Editor = wx_object:call(EditorPid, stc),
	wxStyledTextCtrl:cmdKeyExecute(Editor, ?wxSTC_CMD_ZOOMOUT).


%% =====================================================================
%% @doc

transform_uc_selection(EditorPid) ->
	Editor = wx_object:call(EditorPid, stc),
	wxStyledTextCtrl:cmdKeyExecute(Editor, ?wxSTC_CMD_UPPERCASE).


%% =====================================================================
%% @doc
	
transform_lc_selection(EditorPid) ->
	Editor = wx_object:call(EditorPid, stc),
	wxStyledTextCtrl:cmdKeyExecute(Editor, ?wxSTC_CMD_LOWERCASE).


%% =====================================================================
%% @doc
	
transform_selection(EditorPid, {transform, Type}) ->
	Editor = wx_object:call(EditorPid, stc),
	Cmd = case Type of 
		uppercase -> ?wxSTC_CMD_UPPERCASE;
		lowercase -> ?wxSTC_CMD_LOWERCASE
	end,
	wxStyledTextCtrl:cmdKeyExecute(Editor, Cmd).
	
	
%% =====================================================================
%% @doc
	
fn_list(EditorPid, Str) ->
	get_focus(wx_object:call(EditorPid, stc)),
	ok.
	

%% =====================================================================
%% @doc

link_poller(Pid, Path) ->
	wx_object:cast(Pid, {link_poller, Path}).
	

%% =====================================================================
%% @doc
	
empty_undo_buffer(This) ->
	wxStyledTextCtrl:emptyUndoBuffer(wx_object:call(This, stc)).


%% =====================================================================
%% Callback functions
%% =====================================================================

init(Config) ->
  Parent = proplists:get_value(parent, Config),
  Sb = proplists:get_value(status_bar, Config),
  Font = proplists:get_value(font, Config),
  File = proplists:get_value(file, Config, false),

  Panel = wxPanel:new(Parent),

  Sizer = wxBoxSizer:new(?wxVERTICAL),
  wxPanel:setSizer(Panel, Sizer),
  Editor = wxStyledTextCtrl:new(Panel), 
  wxSizer:add(Sizer, Editor, [{flag, ?wxEXPAND}, {proportion, 1}]),              

	%% Immutable editor styles
  wxStyledTextCtrl:setLexer(Editor, ?wxSTC_LEX_ERLANG), %% This lexer needs a lot of work, e.g. better folding support, proper display of ctrl chars etc.
	wxStyledTextCtrl:setKeyWords(Editor, 0, keywords()),
	  wxStyledTextCtrl:setSelectionMode(Editor, ?wxSTC_SEL_LINES),
	wxStyledTextCtrl:setMargins(Editor, ?LEFT_MARGIN_WIDTH, ?RIGHT_MARGIN_WIDTH), %% Left and right of text         							
	  wxStyledTextCtrl:setMarginType(Editor, 0, ?wxSTC_MARGIN_NUMBER),   	
	wxStyledTextCtrl:setMarginWidth(Editor, 1, 10),
	wxStyledTextCtrl:setMarginType(Editor, 1, ?wxSTC_MARGIN_SYMBOL),
	wxStyledTextCtrl:setMarginMask(Editor, 1, (bnot ?wxSTC_MASK_FOLDERS) - 4),
	
	%% Folding
  wxStyledTextCtrl:setMarginType(Editor, 2, ?wxSTC_MARGIN_SYMBOL),
  wxStyledTextCtrl:setMarginWidth(Editor, 2, 9),
	wxStyledTextCtrl:setMarginMask(Editor, 2, ?wxSTC_MASK_FOLDERS),
  wxStyledTextCtrl:setMarginSensitive(Editor, 2, true), %% Makes margin sensitive to mouse clicks
  wxStyledTextCtrl:setProperty(Editor, "fold", "1"),
  wxStyledTextCtrl:markerDefine(Editor, ?wxSTC_MARKNUM_FOLDER, ?wxSTC_MARK_BOXPLUS,[]),
  wxStyledTextCtrl:markerDefine(Editor, ?wxSTC_MARKNUM_FOLDEROPEN, ?wxSTC_MARK_BOXMINUS,[]),
  wxStyledTextCtrl:markerDefine(Editor, ?wxSTC_MARKNUM_FOLDERSUB, ?wxSTC_MARK_VLINE,[]),
  wxStyledTextCtrl:markerDefine(Editor, ?wxSTC_MARKNUM_FOLDEREND, ?wxSTC_MARK_BOXPLUSCONNECTED,[]),
  wxStyledTextCtrl:markerDefine(Editor, ?wxSTC_MARKNUM_FOLDEROPENMID, ?wxSTC_MARK_BOXMINUSCONNECTED,[]),
  wxStyledTextCtrl:markerDefine(Editor, ?wxSTC_MARKNUM_FOLDERMIDTAIL, ?wxSTC_MARK_TCORNER,[]),
  wxStyledTextCtrl:markerDefine(Editor, ?wxSTC_MARKNUM_FOLDERTAIL, ?wxSTC_MARK_LCORNER,[]),					     

	%% Indicators
  wxStyledTextCtrl:indicatorSetStyle(Editor,0,?wxSTC_INDIC_ROUNDBOX),
  wxStyledTextCtrl:indicatorSetForeground(Editor, 0, {255,255,0,255}),
  wxStyledTextCtrl:indicatorSetStyle(Editor,1,?wxSTC_INDIC_BOX),
  wxStyledTextCtrl:indicatorSetStyle(Editor,2,?wxSTC_INDIC_PLAIN), %% underline

	case theme:load_theme(user_prefs:get_user_pref({pref, theme})) of
		{error, load_theme} -> ok; %% Default STC settings
		Theme -> setup_theme(Editor, Theme, Font)
	end,

	%% Indentation
	wxStyledTextCtrl:setTabWidth(Editor, 
		list_to_integer(user_prefs:get_user_pref({pref, tab_width}))),
	wxStyledTextCtrl:setUseTabs(Editor, user_prefs:get_user_pref({pref, use_tabs})), 
	wxStyledTextCtrl:setIndentationGuides(Editor, user_prefs:get_user_pref({pref, indent_guides})),
	wxStyledTextCtrl:setBackSpaceUnIndents(Editor, true),
	
	%% Scrolling
	% wxStyledTextCtrl:setEndAtLastLine(Editor, user_prefs:get_user_pref({pref, scroll_past_end})),
  % Policy = ?wxSTC_CARET_SLOP bor ?wxSTC_CARET_JUMPS bor ?wxSTC_CARET_EVEN, 
  Policy = ?wxSTC_CARET_SLOP bor ?wxSTC_CARET_EVEN, 
  wxStyledTextCtrl:setYCaretPolicy(Editor, Policy, 3),
  wxStyledTextCtrl:setXCaretPolicy(Editor, Policy, 3),
  wxStyledTextCtrl:setVisiblePolicy(Editor, Policy, 3),

	%% Wrapping
	wxStyledTextCtrl:setWrapMode(Editor, user_prefs:get_user_pref({pref, line_wrap})),

	%% Attach events
  wxStyledTextCtrl:connect(Editor, stc_marginclick, []),
  wxStyledTextCtrl:connect(Editor, stc_modified, [{userData, Sb}]),
  wxStyledTextCtrl:connect(Editor, left_down, [{skip, true}, {userData, Sb}]),
  wxStyledTextCtrl:connect(Editor, left_up, [{skip, true}, {userData, Sb}]),
  wxStyledTextCtrl:connect(Editor, motion, [{skip, true}, {userData, Sb}]),
  wxStyledTextCtrl:connect(Editor, stc_charadded, [{skip, true}, {userData, Sb}]),
	wxStyledTextCtrl:connect(Editor, key_down, [{skip, true}, {userData, Sb}]),
	wxStyledTextCtrl:connect(Editor, stc_styleneeded, [{skip, true}, {userData, Sb}]),
	wxStyledTextCtrl:connect(Editor, stc_updateui, [{skip, true}, {userData, Sb}]),
	wxStyledTextCtrl:connect(Editor, stc_savepointreached, [{skip, true}, {userData, Sb}]),
	wxStyledTextCtrl:connect(Editor, stc_savepointleft, [{skip, true}, {userData, Sb}]),
	wxStyledTextCtrl:connect(Editor, set_focus, [{skip, true}]),
	wxStyledTextCtrl:connect(Editor, kill_focus, [{skip, true}]),

	%% Restrict the stc_chamge/stc_modified events to insert/delete text
	wxStyledTextCtrl:setModEventMask(Editor, ?wxSTC_MOD_DELETETEXT bor ?wxSTC_MOD_INSERTTEXT),
		
	% wxStyledTextCtrl:setSavePoint(Editor),
	wxWindow:setFocusFromKbd(Editor),
	
	%% Keyboard mapping
	% wxStyledTextCtrl:cmdKeyClearAll(Editor),
	wxStyledTextCtrl:cmdKeyAssign(Editor, 79, ?wxSTC_SCMOD_CTRL, ?wxSTC_CMD_SELECTALL),

  {Panel, #state{parent_panel=Panel, stc=Editor}}.

handle_info(Msg, State) ->
  io:format("Got Info(Editor) ~p~n",[Msg]),
  {noreply,State}.

handle_call(shutdown, _From, State=#state{parent_panel=Panel}) ->
  wxPanel:destroy(Panel),
  {stop, normal, ok, State};

handle_call(is_dirty, _From, State=#state{dirty=Mod}) ->
  {reply,Mod,State};

handle_call(text_content, _From, State) ->
  Text = wxStyledTextCtrl:getText(State#state.stc),
  {reply,Text,State};

handle_call(stc, _From, State) ->
  {reply,State#state.stc,State};

handle_call(parent_panel, _From, State) ->
  {reply,State#state.parent_panel,State};

handle_call(Msg, _From, State) ->
  io:format("Handle call catchall, editor.erl ~p~n",[Msg]),
  {reply,State,State}.

handle_cast({link_poller, Path}, State) ->
	file_poller_sup:start_link([{editor_pid, self()}, {path, Path}]),
  {noreply,State};
	
handle_cast({goto_pos, {Line, Col}}, State=#state{stc=Stc}) ->
	wxStyledTextCtrl:gotoLine(Stc, Line - 1),
	NewPos = wxStyledTextCtrl:getCurrentPos(Stc),
	EndPos = wxStyledTextCtrl:getLineEndPosition(Stc, Line - 1),
	ColPos = case NewPos + Col of
		Pos when Pos > EndPos -> EndPos;
		Pos -> Pos
	end,
	wxStyledTextCtrl:gotoPos(Stc, ColPos),
	flash_current_line(Stc, {255,0,0}, 500, 1),
  {noreply,State}.

%% =====================================================================
%% Mouse events
%% =====================================================================

handle_event(_A=#wx{event=#wxMouse{type=left_down}, userData=Sb}, 
             State = #state{stc=Editor}) ->
	wxStyledTextCtrl:setCaretWidth(Editor, 1),
	update_sb_line(Editor, Sb),
	{noreply, State};

handle_event(_A=#wx{event=#wxMouse{type=motion, leftDown=true}, userData=Sb}, 
             State = #state{stc=Editor}) ->
	wxStyledTextCtrl:setCaretWidth(Editor, 0),
	%% Update status bar selection info
	update_sb_selection(Editor, Sb),
	{noreply, State};

handle_event(_A=#wx{event=#wxMouse{type=left_up}, userData=Sb}, 
             State = #state{stc=Editor}) ->
	%% Update status bar selection info
	update_sb_selection(Editor, Sb),
	{noreply, State};

%% For testing:
handle_event(_A=#wx{event=#wxMouse{}, userData=Sb}, 
             State = #state{stc=Editor}) ->
	{noreply, State};

%% =====================================================================
%% Key events
%% =====================================================================

handle_event(_A=#wx{event=#wxKey{type=key_down, keyCode=_Kc}, userData=Sb}, 
             State = #state{stc=Editor}) ->
	update_sb_line(Editor, Sb),
	update_line_margin(Editor),
	parse_functions(Editor),
	{noreply, State};

handle_event(_A=#wx{event=#wxStyledText{type=stc_charadded, key=Key}=_E, userData=Sb}, 
						State = #state{stc=Editor}) when Key =:= 13 orelse Key =:= 10 ->
	Pos = wxStyledTextCtrl:getCurrentPos(Editor),
	Line = wxStyledTextCtrl:lineFromPosition(Editor, Pos),
	CurInd = wxStyledTextCtrl:getLineIndentation(Editor, Line - 1),
	Width = case to_indent(wxStyledTextCtrl:getLine(Editor, Line - 1)) of
		true ->
			CurInd + wxStyledTextCtrl:getTabWidth(Editor);
		false ->
			CurInd
	end,
	wxStyledTextCtrl:setLineIndentation(Editor, Line, Width),
	wxStyledTextCtrl:gotoPos(Editor, wxStyledTextCtrl:getLineEndPosition(Editor, Line)),
	update_sb_line(Editor, Sb),
	{noreply, State};

%% =====================================================================
%% Document events
%% =====================================================================

handle_event(#wx{event=#wxStyledText{type=stc_marginclick, position=Pos, margin=Margin}=_E},
             State = #state{stc=Editor}) ->
  Ln = wxStyledTextCtrl:lineFromPosition(Editor, Pos),
  Fl = wxStyledTextCtrl:getFoldLevel(Editor, Ln),
  case Margin of
    2 when Ln > 0, Fl > 0 ->
      wxStyledTextCtrl:toggleFold(Editor, Ln);
    _ -> ok
  end,
  {noreply, State};

%% =====================================================================
%% Save events
%% =====================================================================

handle_event(#wx{event=#wxStyledText{type=stc_savepointreached}}, State) ->
  {noreply, State#state{dirty=false}};

handle_event(#wx{event=#wxStyledText{type=stc_savepointleft}}, State) ->
  {noreply, State#state{dirty=true}};

handle_event(E,O) ->
  {noreply, O}.

code_change(_, _, State) ->
  {stop, not_yet_implemented, State}.

terminate(_Reason, State=#state{parent_panel=Panel}) ->
  io:format("TERMINATE EDITOR~n"),
  wxPanel:destroy(Panel).


%% =====================================================================
%% Internal functions
%% =====================================================================

%% =====================================================================
%% @doc Defines the keywords in the Erlang language
%% @private

keywords() ->
	KWS = ["after", "and", "andalso", "band", "begin", "bnot", 
	"bor", "bsl", "bsr", "bxor", "case", "catch", "cond", "div", 
	"end", "fun", "if", "let", "not", "of", "or", "orelse", 
	"receive", "rem", "try", "when", "xor"],
	L = lists:flatten([KW ++ " " || KW <- KWS]).


%% =====================================================================
%% @doc 
%% @private	

to_indent(Input) ->
	Regex = "^[^%]*((?:if|case|receive|after|fun|try|catch|begin|query)|(?:->))(?:\\s*%+.*)?$",
	case re:run(Input, Regex, [unicode, {newline, anycrlf}]) of
		nomatch -> false;
		{_,_} -> true
	end.


%% =====================================================================  
%% @doc Update status bar line/col position
%% @private

update_sb_line(Editor, Sb) ->
	{X,Y} = get_caret_position(Editor),
	ide_status_bar:set_text(Sb,{field,line}, io_lib:format("~w:~w",[X, Y])),
	ide_status_bar:set_text(Sb,{field,selection}, "").


%% =====================================================================  
%% @doc Update status bar line/col position
%% @private

update_sb_selection(Editor, Sb) ->
	case wxStyledTextCtrl:getSelection(Editor) of
	{X,X} -> ok;
	{X,Y} -> 
	{X1,Y1} = position_to_x_y(Editor, X),
	{X2,Y2} = position_to_x_y(Editor, Y),
	ide_status_bar:set_text(Sb,{field,selection}, io_lib:format("~w:~w-~w:~w",[X1,Y1,X2,Y2])),
	ide_status_bar:set_text(Sb,{field,line}, "")
	end.


%% =====================================================================  
%% @doc Convert a position to line and column number.
%% @private	

position_to_x_y(Editor, Pos) -> 
Ln = wxStyledTextCtrl:lineFromPosition(Editor, Pos),
{Ln + 1, Pos - wxStyledTextCtrl:positionFromLine(Editor, Ln)}.


%% =====================================================================  
%% @doc Get the current position of of caret.
%% x=line no, y=col no.
%% @private

-spec editor:get_caret_position(Editor) -> Result when
Editor :: wxStyledTextCtrl:wxStyledTextCtrl(),
Result :: {integer(), integer()}.

get_caret_position(Editor) ->
position_to_x_y(Editor, wxStyledTextCtrl:getCurrentPos(Editor)).


%% =====================================================================  
%% @doc Check whether a margin adjustment is required, (if the margin is
%% currently shown or not).
%% @private	

update_line_margin(Editor) ->
	case user_prefs:get_user_pref({pref, show_line_no}) of
		true ->
			set_linenumber_default(Editor, user_prefs:get_user_pref({pref, font}));
		false ->
			ok
	end.  


%% =====================================================================  
%% @doc Adjust the width of the line_number margin if necessary,
%% dynamically (it doesn't currently decrease).
%% @private

-spec adjust_margin_width(Editor) -> Result when
  Editor :: wxStyledTextCtrl:wxStyledTextCtrl(),
  Result :: 'ok'.

adjust_margin_width(Editor) ->
  Lc = wxStyledTextCtrl:getLineCount(Editor),
	Padding = wxStyledTextCtrl:textWidth(Editor, ?wxSTC_STYLE_LINENUMBER, " "),
  NewWidth = wxStyledTextCtrl:textWidth(Editor, ?wxSTC_STYLE_LINENUMBER, integer_to_list(Lc) ++ " "),
  Cw = wxStyledTextCtrl:getMarginWidth(Editor, 0),	  
  if
    NewWidth /= Cw ->
			if
				Lc < 10 ->
					wxStyledTextCtrl:setMarginWidth(Editor, 0, NewWidth + Padding);
				true ->
      		wxStyledTextCtrl:setMarginWidth(Editor, 0, NewWidth)
			end;
    true -> ok
  end,
  ok.  


%% =====================================================================
%% @doc	
%% @private

setup_theme(Editor, [Def | Lex], Font) ->
	Fg = theme:hexstr_to_rgb(proplists:get_value(fgColour, Def)),
	set_default_styles(Editor, Fg,
		theme:hexstr_to_rgb(proplists:get_value(bgColour, Def)), Font),
	set_theme_styles(Editor, Def, Font),
	apply_lexer_styles(Editor, Lex, Fg),
	ok.


%% =====================================================================
%% @doc Sets all styles to have the same attributes as STYLE_DEFAULT.
%% Must be called before setting any theme styles, as they will be reset.
%% @private

set_default_styles(Editor, Fg, Bg, Font) ->
	wxStyledTextCtrl:styleSetBackground(Editor, ?wxSTC_STYLE_DEFAULT, Bg),
	wxStyledTextCtrl:styleSetForeground(Editor, ?wxSTC_STYLE_DEFAULT, Fg),
	wxStyledTextCtrl:styleSetFont(Editor, ?wxSTC_STYLE_DEFAULT, Font),
	wxStyledTextCtrl:styleClearAll(Editor),
	ok.


%% =====================================================================
%% @doc Update all styles relating to a theme.
%% @private

set_theme_styles(Editor, Styles, Font) ->
  wxStyledTextCtrl:setCaretForeground(Editor, 
		theme:hexstr_to_rgb(proplists:get_value(caret, Styles))),
  wxStyledTextCtrl:setSelBackground(Editor, true, 
		theme:hexstr_to_rgb(proplists:get_value(selection, Styles))),
	wxStyledTextCtrl:styleSetBackground(Editor, ?wxSTC_STYLE_LINENUMBER, 
		theme:hexstr_to_rgb(proplists:get_value(marginBg, Styles))),
	wxStyledTextCtrl:styleSetForeground(Editor, ?wxSTC_STYLE_LINENUMBER, 
		theme:hexstr_to_rgb(proplists:get_value(marginFg, Styles))),
	update_line_margin(Editor),
 	set_font_style(Editor, Font),
  set_marker_colour(Editor, {theme:hexstr_to_rgb(proplists:get_value(markers, Styles)), 
		theme:hexstr_to_rgb(proplists:get_value(markers, Styles))}).


%% =====================================================================
%% @doc Update the line number margin font and width.
%% @private

set_linenumber_default(Editor, Font) ->
	wxStyledTextCtrl:styleSetSize(Editor, ?wxSTC_STYLE_LINENUMBER, 
		(wxFont:getPointSize(Font) - ?MARGIN_LN_PT_OFFSET)),
	adjust_margin_width(Editor),
	ok.


%% =====================================================================
%% @doc
%% @private

apply_lexer_styles(Editor, Styles, Fg) ->
	SetFg = fun wxStyledTextCtrl:styleSetForeground/3,
	SetFg(Editor, ?wxSTC_ERLANG_DEFAULT, Fg),
	[ SetFg(Editor, Id, Rgb) || {Id, Rgb} <- proplists:get_value(fgColour, Styles)],
	SetBg = fun wxStyledTextCtrl:styleSetBackground/3,
	[ SetBg(Editor, Id, Rgb) || {Id, Rgb} <- proplists:get_value(bgColour, Styles)],
	[ set_font_style(Editor, Id, Fs) || {Id, Fs} <- proplists:get_value(fontStyle, Styles)],
	[ set_font_size(Editor, Id, N) || {Id, N} <- proplists:get_value(fontSize, Styles)],
	ok.


%% =====================================================================
%% @doc
%% @private

set_font_style(Editor, Id, Style) ->
	Res = case string:to_lower(Style) of
		"italic" -> wxStyledTextCtrl:styleSetItalic(Editor, Id, true);
		"bold" -> wxStyledTextCtrl:styleSetBold(Editor, Id, true);
		"underlined" -> wxStyledTextCtrl:styleSetUnderline(Editor, Id, true);
		"uppercase" -> wxStyledTextCtrl:styleSetCase(Editor, Id, ?wxSTC_CASE_UPPER)
	end.


%% =====================================================================
%% @doc Update the colour of the markers used in the fold margin.
%% @private

set_marker_colour(Editor, {Fg, Bg}) ->
	Update = fun(Id) -> 
		wxStyledTextCtrl:markerSetForeground(Editor, Id, Fg),
		wxStyledTextCtrl:markerSetBackground(Editor, Id, Bg)
		end,
	[ Update(Id) || Id <- lists:seq(?wxSTC_MARKNUM_FOLDEREND, ?wxSTC_MARKNUM_FOLDEROPEN)].


%% =====================================================================
%% @doc
%% @private

set_font_size(Editor, Id, Size) ->
	wxStyledTextCtrl:styleSetSize(Editor, Id, list_to_integer(Size)).


%% =====================================================================
%% Selections, Indentations
%% =====================================================================

%% =====================================================================
%% @doc
%% @private

indent_line(Editor, Ln, Indent) ->
	wxStyledTextCtrl:setLineIndentation(Editor, Ln, (wxStyledTextCtrl:getLineIndentation(Editor, Ln) + Indent)),
	ok.
	

%% =====================================================================
%% @doc
%% @private

lines(Editor, {Start, End}) ->
	lists:seq(wxStyledTextCtrl:lineFromPosition(Editor, Start), 
		wxStyledTextCtrl:lineFromPosition(Editor, End)).


%% =====================================================================
%% @doc
%% @private

count_selected_lines(Editor) ->
	{N, M} = wxStyledTextCtrl:getSelection(Editor),
	Start = wxStyledTextCtrl:lineFromPosition(Editor, N),
	End = wxStyledTextCtrl:lineFromPosition(Editor, M),
	(End - Start) + 1.


%% =====================================================================
%% Commenting
%% =====================================================================

%% =====================================================================
%% @doc
%% @private

count_commented_lines_re(Editor) ->
	Txt = wxStyledTextCtrl:getSelectedText(Editor),
	Count = case re:run(Txt, "^\\s*%", [global, multiline]) of
		{match, Captured} ->
			length(Captured);
		nomatch -> 0
	end,
	Count.


%% =====================================================================
%% Regex replace. Returns a list.
%% @private

regex_replace(Subject, Search, Replace) ->
	{ok, Regex} = re:compile(Search, [multiline]),
	Result = re:replace(Subject, Regex, Replace, [global, {return, list}]),
	{Result,length(Result)}.


%% =====================================================================
%% Regex replace. Returns a list.	
%% @private

remove_comments(Subject) ->
	regex_replace(Subject, "(^\\s*)% ?", "\\g1").


%% =====================================================================
%% Regex replace. Returns a list.	
%% @private

insert_comments(Subject) ->
	regex_replace(Subject, "^", "% ").


%% =====================================================================
%% Regex replace. Returns a list.
%% @private

single_line_comment(Editor) ->
	Pos = wxStyledTextCtrl:getCurrentPos(Editor),
	Start = wxStyledTextCtrl:positionFromLine(Editor, wxStyledTextCtrl:lineFromPosition(Editor, Pos)),
	wxStyledTextCtrl:setTargetStart(Editor, Start),
	End = wxStyledTextCtrl:getLineEndPosition(Editor, wxStyledTextCtrl:lineFromPosition(Editor, Pos)),
	wxStyledTextCtrl:setTargetEnd(Editor, End),
	Txt = wxStyledTextCtrl:getTextRange(Editor, Start, End),
	{{Result,_},Offset} = case wxStyledTextCtrl:getCharAt(Editor, wxStyledTextCtrl:getTargetStart(Editor)) of
		37 -> {remove_comments(Txt),-1};
		_ -> {insert_comments(Txt),1}
	end,	
	wxStyledTextCtrl:replaceTarget(Editor, Result),
	%% Force caret to be at the end of the last selected line
	wxStyledTextCtrl:gotoPos(Editor, Pos + Offset),
	ok.


%% =====================================================================
%% This moves the caret back onto the last line of the selection, rather
%% @doc than the first position of the next line
%% @private

correct_caret(Editor, Pos) ->
	{X,Y} = position_to_x_y(Editor, Pos),
	case position_to_x_y(Editor, Pos) of
		{_,0} -> %% Move the caret back one position
			wxStyledTextCtrl:setCurrentPos(Editor, wxStyledTextCtrl:getCurrentPos(Editor) - 1);		
		_ -> ok
	end.


%% =====================================================================
%% @doc Parse the document for a list of current functions
%% @private

parse_functions(Editor) ->
	Input = wxStyledTextCtrl:getText(Editor),
	Regex = "^\\s*((?:[a-z]+[a-zA-Z\\d_@]*))(?:\\(.*\\))",
	Result = case re:run(Input, Regex, [unicode, global, multiline, {capture, all_but_first, list}]) of
		nomatch -> false, [];
		{_,Captured} -> Captured
	end,
	func_list:set(Result),
	ok.


%% =====================================================================
%% @doc Highlight the current line to attract the user's attention.
%% NOTE: This was originally implemented using markerAdd() using a marker
%% that wasn't in the margins mask. This caused a segmentation error on
%% OSX wx294 erlang16b01
%% @private

flash_current_line(Editor, _, _, 0) -> ok;
flash_current_line(Editor, Colour, Interval, N) ->
	wxStyledTextCtrl:setCaretLineBackground(Editor, Colour),
	wxStyledTextCtrl:setCaretLineVisible(Editor, true),
	receive
	after Interval ->
		wxStyledTextCtrl:setCaretLineVisible(Editor, false),
		flash_current_line(Editor, Colour, Interval, N - 1)
	end. 


%% =====================================================================
%% @doc
%% @private
	
get_focus(This) ->
	wxStyledTextCtrl:setFocus(This).


%% =====================================================================
%% @doc

find(Editor, Str, Start, End) ->
  case wxStyledTextCtrl:findText(Editor, Start, End, Str, [{flags, ?wxSTC_FIND_WHOLEWORD}]) of
    -1 ->
      {no_match};
    Pos ->
      wxStyledTextCtrl:startStyling(Editor, Pos, ?wxSTC_INDICS_MASK),
      wxStyledTextCtrl:setStyling(Editor, length(Str), ?wxSTC_INDIC0_MASK),
      % {last_pos, Pos + length(Str)}
      find(Editor, Str, Pos + length(Str), End)
  end.	
	
	
%% ===================================================================== 
%% @doc Replace all occurrences of str with RepStr.

replace_all(Editor, Str, RepStr, Start, End) ->
  wxStyledTextCtrl:setTargetStart(Editor, Start),
  wxStyledTextCtrl:setTargetEnd(Editor, End),
  case wxStyledTextCtrl:searchInTarget(Editor, Str) of
    -1 ->
      {no_match};
    Pos ->
      wxStyledTextCtrl:replaceTarget(Editor, RepStr),
			replace_all(Editor, Str, RepStr, Pos+length(Str), End)
      % Boob
  end.


%% ===================================================================== 
%% @doc Replace all occurrences of Str within the range Start -> End. 

replace(EditorPid, Str, Start, End) ->
  Editor = wx_object:call(EditorPid, stc),
  wxStyledTextCtrl:setTargetStart(Editor, Start),
  wxStyledTextCtrl:setTargetEnd(Editor, End),
  wxStyledTextCtrl:replaceTarget(Editor, Str).