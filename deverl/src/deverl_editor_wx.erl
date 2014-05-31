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
%% @doc This module creates a code editor instance.
%% @end
%% =====================================================================

- module(deverl_editor_wx).

-include_lib("wx/include/wx.hrl").
-include("deverl.hrl").

%% wx_object
-behaviour(wx_object).
-export([
  init/1,
  terminate/2,
  code_change/3,
  handle_info/2,
  handle_call/3,
  handle_cast/2,
  handle_event/2,
	handle_sync_event/3]).

%% API
-export([
	start/1,
	is_dirty/1,
  set_savepoint/1,
  get_text/1,
  set_focus/1,
  find/2,
  find_all/2,
  replace_all/3,
	set_text/2,
  quick_find/1,
  
	set_theme/2, %TODO single function for this group
	set_font/2,
  set_lang/2,
  
	set_tab_width/2,
	set_use_tabs/2,
	set_indent_guides/2,
	set_line_wrap/2,
	set_line_margin_visible/2,
	indent_left/1,
	indent_right/1,
	comment/1,
	go_to_position/2,
	get_current_pos/1,
	zoom_in/1,
	zoom_out/1,
	transform_uc_selection/1,
	transform_lc_selection/1,
	transform_selection/2,
  strip_trailing_whitespace/1,
	go_to_symbol/2,
	link_poller/2,
	empty_undo_buffer/1,
  get_stc/1,
  destroy/1,
  select_all/1,
  get_supported_langs/0
	]).

%% Macros
-define(stc, wxStyledTextCtrl).
-define(LEFT_MARGIN_WIDTH, 6).
-define(RIGHT_MARGIN_WIDTH, 6).
-define(MARGIN_LN_PT_OFFSET, 0). %% The size (pts) to reduce margin text by

-define(ID_SEARCH, 10).

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


%% Server state
-record(state, {parent_panel,
                main_sz,
                stc,
								dirty :: boolean(),
								indent,
                search,
                find,
                filename,
                lang
               }).

-record(search, {tc :: wxTextCtrl:wxTextCtrl(),
                 next,
                 prev,
                 match_case
                }).

-record(find, {found,
               last_pos,
               length}).
  

-record(lang_info, {name,
                    filetypes,
                    lexer,
                    words}).
                    
% too add support for a new language add a lang_info record to this list                  
-define(langs,
  % Erlang
  [#lang_info{name="Erlang",
    filetypes="*.erl;*.hrl;*.yrl;*.xrl",
    lexer=?wxSTC_LEX_ERLANG,
    words="after and andalso band begin bnot bor bsl bsr bxor case catch cond div end fun if let not of or orelse receive rem try when xor"},
  % XML
  #lang_info{name="XML",
    filetypes="*.xml",
    lexer=?wxSTC_LEX_XML},
  % Default (keep last)
  #lang_info{name="Default",
    filetypes="*.*",
    lexer=?wxSTC_LEX_NULL}]).         


%% =====================================================================
%% Client API
%% =====================================================================

get_stc(This) ->
  wx_object:call(This, stc).

destroy(This) ->
  wx_object:call(This, shutdown).

%% =====================================================================
%% @doc Start a new editor process
start(Config) ->
  wx_object:start(?MODULE, Config, []).


%% =====================================================================
%% @doc Get the status of the document
is_dirty(This) ->
  wx_object:call(This, is_dirty).


%% =====================================================================
%% @doc Add a savepoint: update the document state to unmodified
set_savepoint(This) ->
	?stc:setSavePoint(wx_object:call(This, stc)).


%% =====================================================================
%% @doc Get the text from the editor
get_text(This) ->
  wx_object:call(This, text_content).


%% =====================================================================
%% @doc Notify the editor it has been selected
set_focus(This) ->
	wx_object:cast(This, set_focus).


%% =====================================================================
%% Find and replace
%% =====================================================================
find(This, Str) ->
  TextCtrl = wx_object:call(This, stc),
  Pos = ?stc:findText(TextCtrl, 0,
		?stc:getLength(TextCtrl), Str, [{flags, ?wxSTC_FIND_WHOLEWORD}]),
	io:format("Pos: ~p~n", [position_to_x_y(TextCtrl, Pos)]).

%% =====================================================================
%% Search/replace with next/prev

% find(EditorPid, Str) ->
%   TextCtrl = wx_object:call(EditorPid, stc),
%   Pos = ?stc:findText(TextCtrl, 0,
% 		?stc:getLength(TextCtrl), Str, [{flags, ?wxSTC_FIND_WHOLEWORD}]),
%   ?stc:startStyling(TextCtrl, Pos, ?wxSTC_INDICS_MASK),
%   ?stc:setStyling(TextCtrl, length(Str), ?wxSTC_INDIC0_MASK).
%   %% Bookmark line and add indicator to word


%% =====================================================================
%% @doc Find all occurrences of Str.
find_all(EditorPid, Str) ->
  Editor = wx_object:call(EditorPid, stc),
  find(Editor, Str, 0, ?stc:getLength(Editor)).


%% =====================================================================
%% @doc Replace all occurrences of str with RepStr.
replace_all(EditorPid, Str, RepStr) ->
    Editor = wx_object:call(EditorPid, stc),
    replace_all(Editor, Str, RepStr, 0, ?stc:getLength(Editor)).


%% =====================================================================
%% @doc Set the text.
set_text(This, Text) ->
	Editor = wx_object:call(This, stc),
	?stc:setText(Editor, Text),
	update_line_margin(Editor).


%% =====================================================================
%% @doc Display the intergrated search bar.
quick_find(This) ->
  wx_object:cast(This, show_search).


%% =====================================================================
%% Styling
%% =====================================================================

%% =====================================================================
%% @doc Change the theme of the editor.
set_theme(This, Theme) ->
	wx_object:cast(This, {set_theme, Theme}).
%% =====================================================================
%% @doc Update the font used in the editor
set_font(This, Font) ->
  wx_object:cast(This, {set_prefs, Font}).
%% =====================================================================
%% @doc Update the lexer lang used
set_lang(This, Lang) ->
  wx_object:cast(This, {set_lang, Lang}).

%% =====================================================================
%% @doc

set_tab_width(This, Width) ->
	?stc:setTabWidth(wx_object:call(This, stc), Width).


%% =====================================================================
%% @doc

set_use_tabs(This, Bool) ->
	?stc:setUseTabs(wx_object:call(This, stc), Bool).


%% =====================================================================
%% @doc

set_indent_guides(This, Bool) ->
	?stc:setIndentationGuides(wx_object:call(This, stc), Bool),
	ok.


%% =====================================================================
%% @doc

set_line_wrap(This, Bool) ->
	Result = case Bool of
		true -> 1;
		_ -> 0
	end,
	?stc:setWrapMode(wx_object:call(This, stc), Result).


%% =====================================================================
%% @doc

set_line_margin_visible(This, Bool) ->
	Editor = wx_object:call(This, stc),
	case Bool of
		true ->
      set_linenumber_default(Editor, deverl_sys_pref_gen:get_font(editor_font));
		false -> ?stc:setMarginWidth(Editor, 0, 0)
	end.


%% =====================================================================
%% @doc

indent_left(EditorPid) ->
	indent(EditorPid, ?wxSTC_CMD_BACKTAB),
	ok.


%% =====================================================================
%% @doc

indent_right(EditorPid) ->
	indent(EditorPid, ?wxSTC_CMD_TAB),
	ok.


%% =====================================================================
%% @doc

comment(EditorPid) ->
	Editor = wx_object:call(EditorPid, stc),
	case ?stc:getSelection(Editor) of
		{N,N} -> single_line_comment(Editor);
		{N,_} ->
			Sel = count_selected_lines(Editor),
			{Result,Length} = case count_commented_lines_re(Editor) of
				Sel ->
					remove_comments(?stc:getSelectedText(Editor));
				_ ->
					insert_comments(?stc:getSelectedText(Editor))
			end,
			?stc:replaceSelection(Editor, Result),
			?stc:setSelectionStart(Editor, N),
			?stc:setSelectionEnd(Editor, N + Length),
			correct_caret(Editor, ?stc:getCurrentPos(Editor))
	end,
	ok.


%% =====================================================================
%% @doc Move the caret to the line Line and Column Col.

go_to_position(This, Pos) ->
	wx_object:cast(This, {goto_pos, Pos, true}).


%% =====================================================================
%% @doc

get_current_pos(This) ->
	Stc = wx_object:call(This, stc),
	position_to_x_y(Stc, ?stc:getCurrentPos(Stc)).


%% =====================================================================
%% @doc

zoom_in(EditorPid) ->
	Editor = wx_object:call(EditorPid, stc),
	?stc:cmdKeyExecute(Editor, ?wxSTC_CMD_ZOOMIN).


%% =====================================================================
%% @doc

zoom_out(EditorPid) ->
	Editor = wx_object:call(EditorPid, stc),
	?stc:cmdKeyExecute(Editor, ?wxSTC_CMD_ZOOMOUT).


%% =====================================================================
%% @doc

transform_uc_selection(EditorPid) ->
	Editor = wx_object:call(EditorPid, stc),
	?stc:cmdKeyExecute(Editor, ?wxSTC_CMD_UPPERCASE).


%% =====================================================================
%% @doc

transform_lc_selection(EditorPid) ->
	Editor = wx_object:call(EditorPid, stc),
	?stc:cmdKeyExecute(Editor, ?wxSTC_CMD_LOWERCASE).


%% =====================================================================
%% @doc

transform_selection(EditorPid, {transform, Type}) ->
	Editor = wx_object:call(EditorPid, stc),
	Cmd = case Type of
		uppercase -> ?wxSTC_CMD_UPPERCASE;
		lowercase -> ?wxSTC_CMD_LOWERCASE
	end,
	?stc:cmdKeyExecute(Editor, Cmd).


%% =====================================================================
%% @doc Strip trailing whitespace from active document.

strip_trailing_whitespace(EditorPid) ->
  Editor = wx_object:call(EditorPid, stc),
  CurrentText = ?stc:getText(Editor),
  StrippedText = re:replace(CurrentText, "\s+$", "", [global, multiline, {return, list}, {newline, any}]),
  case StrippedText of
    CurrentText ->
      ok;
    _ ->
      Pos = position_to_x_y(Editor, ?stc:getCurrentPos(Editor)),
      ?stc:setText(Editor, StrippedText),
      wx_object:cast(EditorPid, {goto_pos, Pos, false})
  end.

%% =====================================================================
%% @doc

go_to_symbol(This, Str) ->
  wx_object:cast(This, {go_to_symbol, Str}).
  

%% =====================================================================
%% @doc

link_poller(This, Path) ->
	wx_object:cast(This, {link_poller, Path}).


%% =====================================================================
%% @doc

empty_undo_buffer(This) ->
	?stc:emptyUndoBuffer(wx_object:call(This, stc)).

%% =====================================================================
%% @doc Select all has to toggle some menu items.

select_all(This) ->
  wx_object:call(This, select_all).

%% =====================================================================
%% Callback functions
%% =====================================================================
%% @hidden
init(Config) ->  
  Parent = proplists:get_value(parent, Config),
  Panel = wxPanel:new(Parent),
  Editor = ?stc:new(Panel, [{id, ?WINDOW_EDITOR}]),
  {Search, SearchRec} = quickfind_bar(Panel),
  
  % determine language (from defintions at top - if extension not recognised then default lexer NULL is used)
  Filename = proplists:get_value(filename, Config),
  PrefInfo = resolve_prefs(Filename),
  % init prefs
  Font = deverl_sys_pref_gen:get_font(editor_font),
  init_prefs(Editor, PrefInfo, deverl_sys_pref_gen:get_preference(theme), Font),

	%% Immutable editor styles
  ?stc:setSelectionMode(Editor, ?wxSTC_SEL_LINES),
  ?stc:setMargins(Editor, ?LEFT_MARGIN_WIDTH, ?RIGHT_MARGIN_WIDTH), %% Left and right of text
  ?stc:setMarginType(Editor, 0, ?wxSTC_MARGIN_NUMBER),
  ?stc:setMarginWidth(Editor, 1, 10),
  ?stc:setMarginType(Editor, 1, ?wxSTC_MARGIN_SYMBOL),
  ?stc:setMarginMask(Editor, 1, (bnot ?wxSTC_MASK_FOLDERS) - 4),
  
  %% Folding
  ?stc:setMarginType(Editor, 2, ?wxSTC_MARGIN_SYMBOL),
  ?stc:setMarginWidth(Editor, 2, 9),
  ?stc:setMarginMask(Editor, 2, ?wxSTC_MASK_FOLDERS),
  ?stc:setMarginSensitive(Editor, 2, true), %% Makes margin sensitive to mouse clicks
  ?stc:setProperty(Editor, "fold", "1"),
  ?stc:markerDefine(Editor, ?wxSTC_MARKNUM_FOLDER, ?wxSTC_MARK_BOXPLUS,[]),
  ?stc:markerDefine(Editor, ?wxSTC_MARKNUM_FOLDEROPEN, ?wxSTC_MARK_BOXMINUS,[]),
  ?stc:markerDefine(Editor, ?wxSTC_MARKNUM_FOLDERSUB, ?wxSTC_MARK_VLINE,[]),
  ?stc:markerDefine(Editor, ?wxSTC_MARKNUM_FOLDEREND, ?wxSTC_MARK_BOXPLUSCONNECTED,[]),
  ?stc:markerDefine(Editor, ?wxSTC_MARKNUM_FOLDEROPENMID, ?wxSTC_MARK_BOXMINUSCONNECTED,[]),
  ?stc:markerDefine(Editor, ?wxSTC_MARKNUM_FOLDERMIDTAIL, ?wxSTC_MARK_TCORNER,[]),
  ?stc:markerDefine(Editor, ?wxSTC_MARKNUM_FOLDERTAIL, ?wxSTC_MARK_LCORNER,[]),
  
  %% Indicators
  ?stc:indicatorSetStyle(Editor,0,?wxSTC_INDIC_ROUNDBOX),
  ?stc:indicatorSetForeground(Editor, 0, {255,255,0,255}),
  ?stc:indicatorSetStyle(Editor,1,?wxSTC_INDIC_BOX),
  ?stc:indicatorSetStyle(Editor,2,?wxSTC_INDIC_PLAIN), %% underline

  %% Indentation
  ?stc:setTabWidth(Editor,
  list_to_integer(deverl_sys_pref_gen:get_preference(tab_width))),
  ?stc:setUseTabs(Editor, deverl_sys_pref_gen:get_preference(use_tabs)),
  ?stc:setIndentationGuides(Editor, deverl_sys_pref_gen:get_preference(indent_guides)),
  ?stc:setBackSpaceUnIndents(Editor, true),

  %% Scrolling
  Policy = ?wxSTC_CARET_SLOP bor ?wxSTC_CARET_EVEN,
  ?stc:setYCaretPolicy(Editor, Policy, 3),
  ?stc:setXCaretPolicy(Editor, Policy, 3),
  ?stc:setVisiblePolicy(Editor, Policy, 3),

	%% Wrapping
	?stc:setWrapMode(Editor, deverl_sys_pref_gen:get_preference(line_wrap)),

	%% Attach events
  ?stc:connect(Editor, stc_marginclick, []),
  ?stc:connect(Editor, stc_charadded, [callback]),
  ?stc:connect(Editor, stc_savepointreached, [{skip, true}]),
  ?stc:connect(Editor, stc_savepointleft, [{skip, true}]),
  ?stc:connect(Editor, set_focus, [{skip, true}]),
  ?stc:connect(Editor, kill_focus, [{skip, true}]),
  ?stc:connect(Editor, stc_updateui, []),
  ?stc:connect(Editor, stc_change, [{skip, true}]), %% undo/redo stop points
  ?stc:connect(Editor, char, [callback]),
  ?stc:setModEventMask(Editor, ?wxSTC_LASTSTEPINUNDOREDO), %% restrict to undo/redo notices

	% ?stc:setSavePoint(Editor),
	wxWindow:setFocusFromKbd(Editor),

	%% Keyboard mapping
	% ?stc:cmdKeyClearAll(Editor),
	?stc:cmdKeyAssign(Editor, 79, ?wxSTC_SCMOD_CTRL, ?wxSTC_CMD_SELECTALL),

  Sizer = wxBoxSizer:new(?wxVERTICAL),
  wxSizer:add(Sizer, Editor, [{flag, ?wxEXPAND}, {proportion, 1}]),
  wxSizer:add(Sizer, Search, [{flag, ?wxEXPAND}, {proportion, 0}]),
  wxSizer:hide(Sizer, 1),
  wxPanel:setSizer(Panel, Sizer),

  {Panel, #state{parent_panel=Panel,
                 stc=Editor,
                 search=SearchRec,
                 main_sz=Sizer,
                 filename=Filename,
                 lang=PrefInfo#lang_info.name}}.
%% @hidden
handle_info(Msg, State) ->
  io:format("Got Info(Editor) ~p~n",[Msg]),
  {noreply,State}.
%% @hidden
handle_call(shutdown, _From, State) ->
  {stop, normal, ok, State};
handle_call(is_dirty, _From, State=#state{dirty=Mod}) ->
  {reply,Mod,State};
handle_call(text_content, _From, State=#state{stc=Editor}) ->
  Text = ?stc:getText(Editor),
  {reply,Text,State};
handle_call(stc, _From, State) ->
  {reply,State#state.stc,State};
handle_call(parent_panel, _From, State) ->
  {reply,State#state.parent_panel,State};
handle_call(select_all, _From, State) ->
  ?stc:selectAll(State#state.stc),
  deverl:enable_menu_items([?wxID_CUT, ?wxID_COPY, ?wxID_DELETE], true),
  {reply,ok,State};
handle_call(Msg, _From, State) ->
  io:format("Handle call catchall, editor.erl ~p~n",[Msg]),
  {reply,State,State}.
%% @hidden

% perform any ui updates when this editor instance is made active
handle_cast(set_focus, State) ->
  update_sb_line(State#state.stc),
  parse_functions(State#state.stc),
  deverl:check_menu_item({State#state.lang, "View"}), % check correct lang in menu
  {noreply,State};

% handle pref changes
handle_cast({set_lang, Lang}, State) ->
  init_prefs(State#state.stc, Lang, deverl_sys_pref_gen:get_preference(theme), deverl_sys_pref_gen:get_font(editor_font)),
  {noreply,State#state{lang=Lang}};
handle_cast({set_prefs, Font}, State) ->
  init_prefs(State#state.stc, State#state.lang, deverl_sys_pref_gen:get_preference(theme), Font),
  {noreply,State};
handle_cast({set_theme, Theme}, State) ->
  init_prefs(State#state.stc, State#state.lang, Theme, deverl_sys_pref_gen:get_font(editor_font)),
  {noreply,State};

handle_cast({go_to_symbol, Symbol}, State) ->
  Text = ?stc:getText(State#state.stc),
  Regex = "^"++Symbol++"\\(",
  case re:run(Text, "^"++Symbol++"\\(", [unicode, multiline]) of
    {match,[{Start,Length}]} ->
      ?stc:gotoPos(State#state.stc, Start),
      wxWindow:setFocusFromKbd(State#state.stc);
    true -> ok
  end,
  {noreply,State};
handle_cast({link_poller, Path}, State) ->
	deverl_file_poll_sup:start_link([{editor_pid, self()}, {path, Path}]),
  {noreply,State};
handle_cast({goto_pos, {Line, Col}, Flash}, State=#state{stc=Stc}) ->
	?stc:gotoLine(Stc, Line - 1),
	NewPos = ?stc:getCurrentPos(Stc),
	EndPos = ?stc:getLineEndPosition(Stc, Line - 1),
	ColPos = case NewPos + Col of
		Pos when Pos > EndPos -> EndPos;
		Pos -> Pos
	end,
	?stc:gotoPos(Stc, ColPos),
  case Flash of
    true ->
      flash_current_line(Stc, {255,0,0}, 500, 1);
    false ->
      ok
  end,
  {noreply,State};
handle_cast(ref, State=#state{stc=Editor}) ->
	update_sb_line(Editor),
	update_line_margin(Editor),
  {noreply,State};
handle_cast(show_search, State=#state{main_sz=Sz, search=#search{tc=Tc}}) ->
  wxSizer:show(Sz, 1),
  wxTextCtrl:setFocus(Tc),
  wxSizer:layout(Sz),
  {noreply,State};
handle_cast(enable_menus, State) ->
  %% Enable undo/redo
  menu_init(State#state.stc),
  {noreply, State}.

%% =====================================================================
%% Sync events
%% =====================================================================
%% @hidden
handle_sync_event(#wx{event=#wxStyledText{type=stc_charadded, key=Key}}, Event,
									#state{stc=Editor}) when Key =:= ?WXK_RETURN orelse
                                                 Key =:= ?WXK_NUMPAD_ENTER orelse
                                                 Key =:= 10 ->
  Pos = ?stc:getCurrentPos(Editor),
  Line = ?stc:lineFromPosition(Editor, Pos),
  CurInd = ?stc:getLineIndentation(Editor, Line - 1),
  Width = case to_indent(?stc:getLine(Editor, Line - 1)) of
    true ->
      CurInd + ?stc:getTabWidth(Editor);
    false ->
      CurInd
  end,
  ?stc:setLineIndentation(Editor, Line, Width),
  ?stc:gotoPos(Editor, ?stc:getLineEndPosition(Editor, Line)),
  update_line_margin(Editor),
  wxEvent:skip(Event);

%% Auto completion for paired characters.
handle_sync_event(#wx{event=#wxKey{type=char, keyCode=Key}}, Event,
									_State=#state{stc=Editor}) when Key =:= ${ orelse
                                                 Key =:= $( orelse
                                                 Key =:= $[ orelse
                                                 Key =:= $" orelse
                                                 Key =:= $' orelse
                                                 Key =:= $` ->
  {OpeningChar, ClosingChar} = case Key of
    ${ -> {"{", "}"};
    $( -> {"(", ")"};
    $[ -> {"[", "]"};
    $" -> {"\"", "\""};
    $' -> {"\'", "\'"};
    $` -> {"`", "`"}
  end,
  ?stc:beginUndoAction(Editor),
  case ?stc:getSelection(Editor) of
    %% With no selection
    {N, N} ->
      Pos = ?stc:getCurrentPos(Editor),
      ?stc:addText(Editor, ClosingChar),
      ?stc:gotoPos(Editor, Pos),
      wxEvent:skip(Event);
    %% With selection
    {Start, End} ->
      ?stc:gotoPos(Editor, Start),
      ?stc:addText(Editor, OpeningChar),
      ?stc:gotoPos(Editor, End+1),
      ?stc:addText(Editor, ClosingChar),
      ?stc:setSelection(Editor, Start, End+2)
  end,
  ?stc:endUndoAction(Editor);

%% Catch-alls.
handle_sync_event(#wx{event=#wxStyledText{type=stc_charadded}}, Event,
									_State) ->
  wxEvent:skip(Event);

handle_sync_event(#wx{event=#wxKey{type=char}}, Event,
									_State) ->
  wxEvent:skip(Event).


%% =====================================================================
%% Document events
%% =====================================================================
%% @hidden
handle_event(#wx{event=#wxStyledText{type=stc_marginclick, position=Pos, margin=Margin}=_E},
             State=#state{stc=Editor}) ->
  Ln = ?stc:lineFromPosition(Editor, Pos),
  Fl = ?stc:getFoldLevel(Editor, Ln),
  case Margin of
    2 when Ln > 0, Fl > 0 ->
      ?stc:toggleFold(Editor, Ln);
    _ -> ok
  end,
  {noreply, State};

%% =====================================================================
%% Save events
%% =====================================================================

handle_event(#wx{event=#wxStyledText{type=stc_savepointreached}}, State) ->
  deverl:enable_menu_items([?wxID_SAVE], false),
  {noreply, State#state{dirty=false}};

handle_event(#wx{event=#wxStyledText{type=stc_savepointleft}}, State) ->
  deverl:enable_menu_items([?wxID_SAVE], true),
  {noreply, State#state{dirty=true}}; %% UNDO

%% =====================================================================
%% Search events
%% =====================================================================

handle_event(#wx{id=?ID_SEARCH, event=#wxFocus{}}, State) ->
  {noreply, State#state{find=undefined}};

handle_event(#wx{id=?ID_SEARCH, event=#wxCommand{type=command_text_enter,cmdString=Str}},
             State=#state{stc=Editor, find=Find, search=#search{next=Next0, match_case=Case0}})
               when Find =/= undefined ->
  Dir  = wxRadioButton:getValue(Next0) xor wx_misc:getKeyState(?WXK_SHIFT),
  Case = wxCheckBox:getValue(Case0),
  Pos = if Find#find.found, Dir ->  %% Cycle next
	           ?stc:getAnchor(Editor);
           Find#find.found ->  %% Cycle prev
	           ?stc:getCurrentPos(Editor);
    	     Dir -> %% Not found, start at top
        		 0;
    	     true -> %% Not found, start at bottom
      		   ?stc:getLength(Editor)
  end,
  ?stc:gotoPos(Editor, Pos),
  case quick_find(Editor, Str, Case, Dir) of
    true ->
      {noreply, State#state{find=Find#find{found=true}}};
    false ->
      {noreply, State#state{find=Find#find{found=false}}}
  end;

handle_event(#wx{id=?ID_SEARCH, event=#wxCommand{cmdString=""}},
	           State=#state{stc=Editor}) ->
    Pos = ?stc:getCurrentPos(Editor),
    ?stc:gotoPos(Editor, Pos),
    {noreply, State#state{find=undefined}};

handle_event(#wx{id=?ID_SEARCH, event=#wxCommand{cmdString=Str}},
             State=#state{stc=Editor, search=#search{next=Next, match_case=Case0}, find=Find}) ->
  Dir = wxRadioButton:getValue(Next),
  Case = wxCheckBox:getValue(Case0),
  Find1 = case Find of
    undefined ->
      Pos = ?stc:getCurrentPos(Editor),
      #find{last_pos=Pos, length=length(Str)};
    #find{length=Old} when Old < length(Str) ->
      Find#find{length=length(Str)};
    _ ->
      ?stc:gotoPos(Editor, Find#find.last_pos),
      Find#find{length=length(Str)}
  end,
  case quick_find(Editor, Str, Case, Dir) of
    true ->
      {noreply, State#state{find=Find1#find{found=true}}};
    false ->
      {noreply, State#state{find=Find1#find{found=false}}}
  end;

handle_event(#wx{id=?WINDOW_EDITOR, event=#wxFocus{type=set_focus}}, State=#state{main_sz=Sz}) ->
  %% Introduce a natural delay (cast) here to ensure any previous kill_focus event is handled first
  wx_object:cast(self(), enable_menus),
  wxSizer:hide(Sz, 1),
  wxSizer:layout(Sz),
  {noreply, State};

handle_event(#wx{event=#wxFocus{type=kill_focus}}, State) ->
  deverl:enable_menu_item_group([?MENU_GROUP_NOTEBOOK_KILL_FOCUS, ?MENU_GROUP_TEXT], false),
  {noreply, State};

handle_event(#wx{event=#wxStyledText{type=stc_change}}, State=#state{stc=Editor}) ->
  case ?stc:canUndo(Editor) of
    true -> deverl:enable_menu_items([?wxID_UNDO], true);
    false -> deverl:enable_menu_items([?wxID_UNDO], false)
  end,
  case ?stc:canRedo(Editor) of
    true -> deverl:enable_menu_items([?wxID_REDO], true);
    false -> deverl:enable_menu_items([?wxID_REDO], false)
  end,
  {noreply, State};

handle_event(#wx{event=#wxStyledText{type=stc_updateui}},State=#state{stc=Editor}) ->
  case ?stc:getSelection(Editor) of
    {N,N} ->
      deverl:enable_menu_items([?wxID_CUT, ?wxID_COPY, ?wxID_DELETE], false),
      update_sb_line(Editor);
    _ ->
      deverl:enable_menu_items([?wxID_CUT, ?wxID_COPY, ?wxID_DELETE], true),
      update_sb_selection(Editor)
  end,
  update_line_margin(Editor),
  parse_functions(Editor),
  {noreply, State};

handle_event(E,State) ->
  io:format("Unhandled event in editor: ~p~n", [E]),
  {noreply, State}.
%% @hidden
code_change(_, _, State) ->
  {ok, State}.
%% @hidden
terminate(_Reason, #state{parent_panel=Panel}) ->
  wxPanel:destroy(Panel).


%% =====================================================================
%% Internal functions
%% =====================================================================
%% =====================================================================
%% @doc

quickfind_bar(Parent) ->
  SzFlags = wxSizerFlags:border(wxSizerFlags:align(wxSizerFlags:new(),
    ?wxALIGN_CENTER_VERTICAL), ?wxTOP bor ?wxBOTTOM bor ?wxLEFT, 4),
  HSz = wxBoxSizer:new(?wxHORIZONTAL),
  wxSizer:add(HSz, wxStaticText:new(Parent, ?wxID_ANY, "Find:"), SzFlags),
  Tc1 = wxTextCtrl:new(Parent, ?ID_SEARCH, [{style, ?wxTE_PROCESS_ENTER}]),
  wxSizer:add(HSz, Tc1, wxSizerFlags:proportion(SzFlags, 1)),
  Rb0 = wxRadioButton:new(Parent, ?wxID_ANY, "Next"),
  wxRadioButton:setValue(Rb0, true),
  wxSizer:add(HSz,Rb0,wxSizerFlags:proportion(SzFlags, 0)),
  Rb1 = wxRadioButton:new(Parent, ?wxID_ANY, "Previous"),
  wxSizer:add(HSz,Rb1,SzFlags),
  Cb = wxCheckBox:new(Parent, ?wxID_ANY, "Match Case"),
  wxSizer:add(HSz,Cb,SzFlags),
  wxSizer:addSpacer(HSz, 10),
  wxTextCtrl:connect(Tc1, command_text_updated),
  wxTextCtrl:connect(Tc1, command_text_enter),
  wxTextCtrl:connect(Tc1, kill_focus),
  wxWindow:connect(Parent, command_button_clicked),
  {HSz, #search{tc=Tc1,next=Rb0,prev=Rb1,match_case=Cb}}.

%% =====================================================================
%% @doc
%% @private

to_indent(Input) ->
  case deverl_sys_pref_gen:get_preference(auto_indent) of
    true ->
    	Regex = "^[^%]*((?:if|case|receive|after|fun|try|catch|begin|query)|(?:->))(?:\\s*%+.*)?",
    	case re:run(Input, Regex, [unicode, {newline, anycrlf}]) of
    		nomatch -> false;
    		{_,_} -> true
	    end;
    _ ->
      false
  end.

%% =====================================================================
%% @doc Update status bar line/col position
%% @private

update_sb_line(Editor) ->
	{X,Y} = get_caret_position(Editor),
	deverl_sb_wx:set_text({field,line}, io_lib:format("~w:~w",[X, Y])),
	deverl_sb_wx:set_text({field,selection}, "").


%% =====================================================================
%% @doc Update status bar line/col position
%% @private

update_sb_selection(Editor) ->
	case ?stc:getSelection(Editor) of
	{X,X} -> ok;
	{X,Y} ->
	{X1,Y1} = position_to_x_y(Editor, X),
	{X2,Y2} = position_to_x_y(Editor, Y),
	deverl_sb_wx:set_text({field,selection}, io_lib:format("~w:~w-~w:~w",[X1,Y1,X2,Y2])),
	deverl_sb_wx:set_text({field,line}, "")
	end.


%% =====================================================================
%% @doc Convert a position to line and column number.
%% @private

position_to_x_y(Editor, Pos) ->
Ln = ?stc:lineFromPosition(Editor, Pos),
{Ln + 1, Pos - ?stc:positionFromLine(Editor, Ln)}.


%% =====================================================================
%% @doc Get the current position of of caret.
%% x=line no, y=col no.
%% @private

-spec deverl_editor_wx:get_caret_position(Editor) -> Result when
Editor :: ?stc:?stc(),
Result :: {integer(), integer()}.

get_caret_position(Editor) ->
  position_to_x_y(Editor, ?stc:getCurrentPos(Editor)).


%% =====================================================================
%% @doc Check whether a margin adjustment is required, (if the margin is
%% currently shown or not).
%% @private

update_line_margin(Editor) ->
	case deverl_sys_pref_gen:get_preference(show_line_no) of
		true ->
      Font = deverl_sys_pref_gen:get_font(editor_font),
			set_linenumber_default(Editor, Font);
		false ->
			ok
	end.


%% =====================================================================
%% @doc Adjust the width of the line_number margin if necessary,
%% dynamically (it doesn't currently decrease).
%% @private

-spec adjust_margin_width(Editor) -> Result when
  Editor :: ?stc:?stc(),
  Result :: 'ok'.

adjust_margin_width(Editor) ->
  Lc = ?stc:getLineCount(Editor),
	Padding = ?stc:textWidth(Editor, ?wxSTC_STYLE_LINENUMBER, " "),
  NewWidth = ?stc:textWidth(Editor, ?wxSTC_STYLE_LINENUMBER, integer_to_list(Lc) ++ " "),
  Cw = ?stc:getMarginWidth(Editor, 0),
  if
    NewWidth /= Cw ->
			if
				Lc < 10 ->
					?stc:setMarginWidth(Editor, 0, NewWidth + Padding);
				true ->
      		?stc:setMarginWidth(Editor, 0, NewWidth)
			end;
    true -> ok
  end,
  ok.

%% =====================================================================
%% @doc Update the line number margin font and width.
%% @private

set_linenumber_default(Editor, Font) ->
  ?stc:styleSetSize(Editor, ?wxSTC_STYLE_LINENUMBER,
    (wxFont:getPointSize(Font) - ?MARGIN_LN_PT_OFFSET)),
  adjust_margin_width(Editor),
  ok.

%% =====================================================================
%% Selections, Indentations
%% =====================================================================

%% =====================================================================
%% @doc

indent(EditorPid, Cmd) ->
	Editor = wx_object:call(EditorPid, stc),
	{S, E} = ?stc:getSelection(Editor),
	R = ?stc:lineFromPosition(Editor, E),
	case ?stc:lineFromPosition(Editor, S) of
		R -> %% Single line
			indent_line(Editor, Cmd);
		_ ->
			?stc:cmdKeyExecute(Editor, Cmd)
	end,
	ok.

indent_line(Editor, Cmd) ->
	Pos = ?stc:getCurrentPos(Editor),
	Line = ?stc:lineFromPosition(Editor, Pos),
	Indent = ?stc:getLineIndentation(Editor, Line),
	Width = ?stc:getTabWidth(Editor),
	NewIndent = case Cmd of
		?wxSTC_CMD_BACKTAB ->
			Indent - Width;
		?wxSTC_CMD_TAB ->
			Indent + Width
	end,
	?stc:setLineIndentation(Editor, Line, NewIndent).


%% =====================================================================
%% @doc
%% @private

count_selected_lines(Editor) ->
	{N, M} = ?stc:getSelection(Editor),
	Start = ?stc:lineFromPosition(Editor, N),
	End = ?stc:lineFromPosition(Editor, M),
	(End - Start) + 1.


%% =====================================================================
%% Commenting
%% =====================================================================

%% =====================================================================
%% @doc
%% @private

count_commented_lines_re(Editor) ->
	Txt = ?stc:getSelectedText(Editor),
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
	regex_replace(Subject, "^", "%").


%% =====================================================================
%% Regex replace. Returns a list.
%% @private

single_line_comment(Editor) ->
	%% Create the region to check
	Pos = ?stc:getCurrentPos(Editor),
	Line = ?stc:lineFromPosition(Editor, Pos),
	Start = ?stc:positionFromLine(Editor, Line),
	?stc:setTargetStart(Editor, Start),
	End = ?stc:getLineEndPosition(Editor, Line),
	?stc:setTargetEnd(Editor, End),
	Str = ?stc:getTextRange(Editor, Start, End),

	{{Result,_},Offset} = case re:run(Str, "^\\s*%", [global]) of
		{match, _Captured} ->
			{remove_comments(Str), -1};
		nomatch ->
			{insert_comments(Str), 1}
	end,

	?stc:replaceTarget(Editor, Result),
	?stc:gotoPos(Editor, Pos + Offset),
	ok.


%% =====================================================================
%% This moves the caret back onto the last line of the selection, rather
%% @doc than the first position of the next line
%% @private

correct_caret(Editor, Pos) ->
	case position_to_x_y(Editor, Pos) of
		{_,0} -> %% Move the caret back one position
			?stc:setCurrentPos(Editor, ?stc:getCurrentPos(Editor) - 1);
		_ -> ok
	end.


%% =====================================================================
%% @doc Parse the document for a list of current functions
%% @private

parse_functions(Editor) ->
	Input = ?stc:getText(Editor),
	Regex = "^((?:[a-z]+[a-zA-Z\\d_@]*))(?:\\(.*\\))",
	Result = case re:run(Input, Regex, [unicode, global, multiline, {capture, all_but_first, list}]) of
		nomatch -> false, [];
		{_,Captured} -> Captured
	end,
  deverl_sl_wx:set(Result, self()).


%% =====================================================================
%% @doc Highlight the current line to attract the user's attention.
%% NOTE: This was originally implemented using markerAdd() using a marker
%% that wasn't in the margins mask. This caused a segmentation error on
%% OSX wx294 erlang16b01
%% @private

flash_current_line(_Editor, _, _, 0) -> ok;
flash_current_line(Editor, Colour, Interval, N) ->
	?stc:setCaretLineBackground(Editor, Colour),
	?stc:setCaretLineVisible(Editor, true),
	receive
	after Interval ->
		?stc:setCaretLineVisible(Editor, false),
		flash_current_line(Editor, Colour, Interval, N - 1)
	end.


%% =====================================================================
%% @doc
get_focus(This) ->
	?stc:setFocus(This),
  wx_object:cast(This, parse_functions).


%% =====================================================================
%% @doc
quick_find(Editor, Str, Case, Next) ->
  ?stc:searchAnchor(Editor),
  Flag = if Case -> ?wxSTC_FIND_MATCHCASE;
            true -> 0
  end,
  Res = if Next -> ?stc:searchNext(Editor, Flag, Str);
           true -> ?stc:searchPrev(Editor, Flag, Str)
	end,
  case Res >= 0 of
    true ->
      ?stc:scrollToLine(Editor,?stc:lineFromPosition(Editor,Res) - 3),
      true;
    false -> false
  end.


%% =====================================================================
%% @doc
find(Editor, Str, Start, End) ->
  case ?stc:findText(Editor, Start, End, Str, [{flags, ?wxSTC_FIND_WHOLEWORD}]) of
    -1 ->
      {no_match};
    Pos ->
      ?stc:startStyling(Editor, Pos, ?wxSTC_INDICS_MASK),
      ?stc:setStyling(Editor, length(Str), ?wxSTC_INDIC0_MASK),
      % {last_pos, Pos + length(Str)}
      find(Editor, Str, Pos + length(Str), End)
  end.


%% =====================================================================
%% @doc Replace all occurrences of str with RepStr.
replace_all(Editor, Str, RepStr, Start, End) ->
  ?stc:setTargetStart(Editor, Start),
  ?stc:setTargetEnd(Editor, End),
  case ?stc:searchInTarget(Editor, Str) of
    -1 ->
      {no_match};
    Pos ->
      ?stc:replaceTarget(Editor, RepStr),
			replace_all(Editor, Str, RepStr, Pos+length(Str), End)
  end.


%% =====================================================================
%% Replace all occurrences of Str within the range Start -> End.

% replace(EditorPid, Str, Start, End) ->
%   Editor = wx_object:call(EditorPid, stc),
%   ?stc:setTargetStart(Editor, Start),
%   ?stc:setTargetEnd(Editor, End),
%   ?stc:replaceTarget(Editor, Str).


%% =====================================================================
%% @doc Initialise menu items
menu_init(Editor) ->
  deverl:enable_menu_item_group([?MENU_GROUP_NOTEBOOK_SET_FOCUS], true),
  Undo = case ?stc:canUndo(Editor) of
    true -> ?wxID_UNDO;
    false -> []
  end,
  Redo = case ?stc:canRedo(Editor) of
    true -> ?wxID_REDO;
    false -> []
  end,
  Paste = case ?stc:canPaste(Editor) of
    true -> ?wxID_PASTE;
    false -> []
  end,
  %% Cut, Copy, Delete
  Others = case ?stc:getSelection(Editor) of
    {N, N} -> []; %% no selection
    _ -> [?wxID_CUT, ?wxID_COPY, ?wxID_DELETE]
  end,
  Save = case ?stc:getModify(Editor) of
    true -> ?wxID_SAVE;
    false -> []
  end,
  Enable = lists:flatten([Undo, Redo, Paste, Save, Others]),
  deverl:enable_menu_items(Enable, true),
  ok.
  
%% =====================================================================
%% @doc Determines the corrects prefs to use for Filename.
%% Returns a lang_info record for language associated to the extension.
resolve_prefs(Filename) ->
  resolve_prefs(filename:extension(Filename), ?langs).
resolve_prefs(Extension, [Prefs=#lang_info{filetypes="*.*"} | T]) -> Prefs;
resolve_prefs(Extension, [Prefs=#lang_info{filetypes=Types} | T]) ->
  case string:str(Types, Extension) of
    0 -> resolve_prefs(Extension, T);
    _ -> Prefs
  end.

%% =====================================================================
%% @doc Get the style record for StyleId.
%% Takes a list of style records.
widget_style_get(_StyleId, []) -> [];
widget_style_get(StyleId, [S=#style{id=Id} | T]) ->
  case string:to_integer(Id) of
    {StyleId, _} -> {S#style.fg_colour, S#style.bg_colour};
    {error, E} -> [];
    P -> widget_style_get(StyleId, T)
  end.
  
%% =====================================================================
%% @doc Init the prefs.
%% Called whenever a theme/font/lang is changed and at startup.
init_prefs(Editor, Lang, Theme, Font) when is_list(Lang) ->
  GetLang = fun F([L=#lang_info{name=Lang} | _T], Lang) -> L; % new named funs YAY!
                F([_L | T], Lang) -> F(T, Lang);
                F([L], _Lang) -> L
            end,
  PrefInfo = GetLang(?langs, Lang),
  init_prefs(Editor, PrefInfo, Theme, Font);
init_prefs(Editor, PrefInfo, Theme, Font) when is_record(PrefInfo, lang_info) ->
  % get the theme
  ThemeRaw = case deverl_theme:parse(Theme, PrefInfo#lang_info.name) of
    error -> []; % set default lexer
    T -> T
  end,
          
  % set lexer
  ?stc:setLexer(Editor, PrefInfo#lang_info.lexer),
  
  % set common
  ?stc:styleSetFont(Editor, ?wxSTC_STYLE_DEFAULT, Font),
  case widget_style_get(?wxSTC_STYLE_DEFAULT, proplists:get_value(widget_styles, ThemeRaw, [])) of
    [] -> ok;
    {FgColour0, BgColour0} ->
      ?stc:styleSetForeground(Editor, ?wxSTC_STYLE_DEFAULT, hexstr_to_rgb(FgColour0)),
      ?stc:styleSetBackground(Editor, ?wxSTC_STYLE_DEFAULT, hexstr_to_rgb(BgColour0))
  end,
  ?stc:styleClearAll(Editor), % set all to default
  
  % widget styles
  case proplists:get_value(widget_styles, ThemeRaw) of
    undefined -> ok;
    Sr -> 
      % Set fg/bg of the styles in Ps
      Ps = lists:seq(?wxSTC_STYLE_DEFAULT, ?wxSTC_STYLE_CALLTIP),
      lists:foreach(fun(E)->
        case widget_style_get(E, Sr) of
          [] -> ok;
          {FgColour, BgColour} ->
            ?stc:styleSetForeground(Editor, E, hexstr_to_rgb(FgColour)),
            ?stc:styleSetBackground(Editor, E, hexstr_to_rgb(BgColour))
        end
      end, Ps),
      case widget_style_get(2069, Sr) of % caret
        [] -> ok;
        {FgColour1, _} -> ?stc:setCaretForeground(Editor, hexstr_to_rgb(FgColour1))
      end
      % TODO markerSetForeground/markerSetBackground
  end,
  
  % word styles
  case proplists:get_value(word_styles, ThemeRaw) of
    undefined -> ok;
    Ss -> 
      lists:foreach(
      fun(Style=#style{id=undefined}) -> ok; % dodgy theme, undefined style id's
        (Style) ->
          case string:to_integer(Style#style.id) of
            {error, _} -> io:format("DEBUG: Parsed a non-integer");
            {Nr, _} ->
              case Style#style.fg_colour of
                undefined -> ok;
                Fg -> ?stc:styleSetForeground(Editor, Nr, hexstr_to_rgb(Fg))
              end,
              case Style#style.bg_colour of
                undefined -> ok;
                Bg -> ?stc:styleSetBackground(Editor, Nr, hexstr_to_rgb(Bg))
              end
          end
      end, Ss)
  end,
  
  % set words
  case PrefInfo#lang_info.words of
    undefined -> ok;
    Ws -> ?stc:setKeyWords(Editor, 0, Ws)
  end,
  ok.
  
%% =====================================================================
%% @doc Convert the hex string from the theme file into {R,G,B}.
hexstr_to_rgb(Rgb) ->
  hexstr_to_rgb(Rgb, []).
hexstr_to_rgb([], Acc) ->
  list_to_tuple(lists:reverse(Acc));
hexstr_to_rgb([A,B | T], Acc) ->
  try
    hexstr_to_rgb(T, [list_to_integer([A,B], 16) | Acc])
  catch
    error:_ -> {125,125,125} % prevents a crash when theme colours are invalid
  end.
  
%% =====================================================================
%% @doc Grab a list of the currently supported langs.
get_supported_langs() ->
  lists:map(fun(E) -> E#lang_info.name end, ?langs).