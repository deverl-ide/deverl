-module(doc_manager).

-include_lib("wx/include/wx.hrl").
-include("ide.hrl").

-behaviour(wx_object).

-export([new/1, init/1, terminate/2,  code_change/3,
         handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).

% API
-export([
	new_document/0, 
	new_document/1, 
	new_document_from_existing/3,
	close_selected_editor/0, 
	close_all_editors/0,
	get_selected_editor/0, 
	get_all_editors/0, 
	update_styles/1, 
	save_current_document/0,
	save_new_document/0, 
	save_all/0,
	save_document/2,
	open_document/1,
	find_replace/1,
	get_current_theme_name/0,
	set_theme/1,
	set_line_wrap/1,
	set_line_margin_visible/1,
	set_indent_tabs/1,
	set_indent_guides/1,
	indent_line_right/0,
	indent_line_left/0,
	go_to_line/1,
	comment/0,
	zoom_in/0,
	zoom_out/0,
	transform_selection/1
	]).

-record(state, {
								status_bar,
								manager,
             		workspace :: wxAuiNotebook:wxAuiNotebook(),    %% Notebook
                editor_pids :: {integer(), pid()}              %% A table containing the Id returned when an editor is created, and the associated pid
                }).


new(Config) ->
  wx_object:start_link({local, ?MODULE}, ?MODULE, Config, []).

init(Config) ->
	{Parent, Sb} = proplists:get_value(config, Config),

	Manager = wxAuiManager:new([{managed_wnd, Parent}]),
	Pane = wxAuiPaneInfo:centrePane(wxAuiPaneInfo:new()),

	Style = (0
			bor ?wxAUI_NB_TOP
			bor ?wxAUI_NB_WINDOWLIST_BUTTON
			bor ?wxAUI_NB_TAB_MOVE
			bor ?wxAUI_NB_SCROLL_BUTTONS
			bor ?wxAUI_NB_CLOSE_ON_ALL_TABS
			),

	Workspace = wxAuiNotebook:new(Parent, [{id, ?ID_WORKSPACE}, {style, Style}]),
	Editor = editor:start([{parent, Workspace}, {status_bar, Sb},
                           {font, user_prefs:get_user_pref({pref, font})}]), %% Returns an editor instance inside a wxPanel

	TabId = ets:new(editors, [public]),
	{_,Id,_,Pid} = Editor,
	ets:insert(TabId,{Id, Pid, {path, undefined}}),

	wxAuiNotebook:addPage(Workspace, Editor, ?DEFAULT_TAB_LABEL, []),

	wxAuiManager:addPane(Manager, Workspace, Pane),

	Close = fun(_,O) ->
            wxNotifyEvent:veto(O),
            close_selected_editor()
          end,

	wxAuiNotebook:connect(Workspace, command_auinotebook_bg_dclick, []),
	wxAuiNotebook:connect(Workspace, command_auinotebook_page_close, [{callback,Close},{userData,TabId}]),
	wxAuiNotebook:connect(Workspace, command_auinotebook_page_changed),

  {Workspace, #state{workspace=Workspace, manager=Manager, status_bar=Sb, editor_pids=TabId}}.

handle_info(Msg, State) ->
	io:format("Got Info ~p~n",[Msg]),
	{noreply,State}.

handle_cast(Msg, State) ->
	io:format("Got cast ~p~n",[Msg]),
	{noreply,State}.

handle_call(workspace, _, State=#state{workspace=Ws, status_bar=Sb, editor_pids=Tb}) ->
	{reply, {Ws,Sb,Tb}, State};

handle_call(Msg, _From, State) ->
	io:format("Got Call ~p~n",[Msg]),
	{reply,ok,State}.

code_change(_, _, State) ->
	{stop, ignore, State}.

terminate(_Reason, State=#state{manager=Manager, workspace=Ws}) ->
	io:format("TERMINATE DOC_MANAGER~n"),
  wxAuiManager:unInit(Manager),
  wxAuiManager:destroy(Manager),
	wxAuiNotebook:destroy(Ws).

%% =====================================================================
%% AUI handlers
%%
%% =====================================================================

handle_event(#wx{obj = _Workspace, event = #wxAuiNotebook{type = command_auinotebook_page_changed,
			selection = Index}}, State) ->
  %% Make sure editor knows (needs to update sb)
  editor:selected(get_editor_pid(Index, State#state.workspace, State#state.editor_pids), State#state.status_bar),
  {noreply, State};

handle_event(#wx{event=#wxAuiNotebook{type=command_auinotebook_bg_dclick}}, State) ->
  new_document(State#state.workspace, State#state.status_bar, 
             State#state.editor_pids),
  {noreply, State};

handle_event(Ev = #wx{}, State = #state{}) ->
  io:format("Got Event ~p~n",[Ev]),
  {noreply,State}.


%% =====================================================================
%% @doc
%%
%% @private

new_document(Workspace, Sb, TabId) ->
	new_document(Workspace, ?DEFAULT_TAB_LABEL, Sb, TabId),
	Workspace.


%% =====================================================================
%% @doc Create a new editor instance in the notebook

new_document() -> 
	new_document(?DEFAULT_TAB_LABEL).
  
%% @doc Create a new editor with specified documentname
new_document(Filename) ->
	{Workspace, Sb, TabId} = wx_object:call(?MODULE, workspace), 
	new_document(Workspace, Filename, Sb, TabId),
	ok.

%% @private
new_document(Workspace, Filename, Sb, TabId) ->
	Editor = editor:start([{parent, Workspace}, {status_bar, Sb}, {font,user_prefs:get_user_pref({pref, font})}]),
	wxAuiNotebook:addPage(Workspace, Editor, Filename, [{select, true}]),
	{_,Id,_,Pid} = Editor,
	ets:insert_new(TabId,{Id, Pid, {path, undefined}}),
	ok.
	
	
%% =====================================================================
%% @doc Add an existing document to the workspace.
		
new_document_from_existing(Path, Filename, Contents) -> 
	{Workspace, Sb, TabId} = wx_object:call(?MODULE, workspace), 
	Editor = {_,Id,_,Pid} = editor:start([{parent, Workspace}, {status_bar, Sb}, 
		{font,user_prefs:get_user_pref({pref, font})}]),
	wxAuiNotebook:addPage(Workspace, Editor, Filename, [{select, true}]),
	ets:insert_new(TabId,{Id, Pid, {path, Path}}),
	editor:set_text(Pid, Contents),
	editor:empty_undo_buffer(Pid),
	editor:set_savepoint(Pid),
	editor:link_poller(Pid, Path),
	ok.


%% =====================================================================
%% @doc Get the Pid of an editor instance at position Index.

-spec get_editor_pid(Index) -> Result when
	Index :: integer(),
	Result :: pid().

get_editor_pid(Index) ->
	{Workspace,_,PidTable} = wx_object:call(?MODULE, workspace),
	get_editor_pid(Index, Workspace, PidTable).

-spec get_editor_pid(Index, Workspace, PidTable) -> Result when
	Index :: integer(),
	Workspace :: wxAuiNotebook:wxAuiNotebook(),
	PidTable :: term(),
	Result :: pid().

get_editor_pid(Index, Workspace, PidTable) ->
	{_,Key,_,_} = wxAuiNotebook:getPage(Workspace, Index),
	[{_,Pid,_}] = ets:lookup(PidTable, Key),
	Pid.


%% =====================================================================
%% @doc Get the Pid of the currently selected editor.

-spec get_selected_editor() -> Result when
	Result :: {'error', 'no_open_editor'} |
			  {'ok', {integer(), pid()}}.

get_selected_editor() ->
	{Workspace,_,_} = wx_object:call(?MODULE, workspace),
	case wxAuiNotebook:getSelection(Workspace) of %% Get the index of the tab
		-1 -> %% no editor instance
				{error, no_open_editor};
		Index ->
				{ok, {Index, get_editor_pid(Index)}}
	end.


%% =====================================================================
%% @doc Get all open editor instances.
%% Returns a list of tuples of the form: {Index, EditorPid}, where
%% Index starts at 0.

-spec get_all_editors() -> Result when
	Result :: [{integer(), pid()}].

get_all_editors() ->
	{Workspace,_,_} = wx_object:call(?MODULE, workspace),
	Count = wxAuiNotebook:getPageCount(Workspace),
	get_all_editors(Workspace, Count - 1, []).

get_all_editors(_, -1, Acc) ->
	Acc;

get_all_editors(Workspace, Count, Acc) ->
	get_all_editors(Workspace, Count -1, [{Count, get_editor_pid(Count)} | Acc]).


%% =====================================================================
%% Open/save/close editor functions
%%
%% =====================================================================


%% =====================================================================
%% @doc Save the currently selected document to disk

save_current_document() ->
	case get_selected_editor() of
		{error, no_open_editor} ->
			%% Toolbar/menubar buttons should be disabled to prevent this action
			io:format("No editor open.~n");
		{ok, {Index, Pid}} ->
			save_document(Index, Pid)
	end.


%% =====================================================================
%% @doc Save the contents of the editor Pid located at index Index.

save_document(Index, Pid) when is_pid(Pid)->  
	{Workspace,Sb,Ets} = wx_object:call(?MODULE, workspace), 
	case editor:is_dirty(Pid) of
		false -> ok;
		_ ->
			save_document(Sb, hd(ets:lookup(Ets, editor:get_id(Pid))))
	end;
save_document(Sb, {_Id, Pid, {path, undefined}}) ->
	save_new_document();
save_document(Sb, {_Id, Pid, {path, Path}}) ->
	%% Document already exists, overwrite
	ide_io:save(Path, editor:get_text(Pid)),
	editor:set_savepoint(Pid),
	ide_status_bar:set_text_timeout(Sb, {field, help}, "Document saved.").

	
%% =====================================================================
%% @doc Save the erlang editor with pid Pid located at index position
%% Index to disk. The user will be prompted for a save path.

save_new_document() ->
	case get_selected_editor() of
		{error, no_open_editor} -> ok;
		{ok, {Index, Pid}} ->
			save_new_document(Index,Pid)
	end.

save_new_document(Index, Pid) ->
	Contents = editor:get_text(Pid),
	{Workspace,Sb,Ets} = wx_object:call(?MODULE, workspace), 
	case ide_io:save_as(Workspace, Contents) of
		{cancel} ->
			ide_status_bar:set_text_timeout(Sb, {field, help}, "Document not saved.");
		{ok, {Path, Filename}}  ->
			ets:update_element(Ets, editor:get_id(Pid), {3, {path, Path}}),
			wxAuiNotebook:setPageText(Workspace, Index, Filename),
			editor:set_savepoint(Pid),
			editor:link_poller(Pid, Path),
			ide_status_bar:set_text_timeout(Sb, {field, help}, "Document saved.")
	end,
	ok.
	

%% =====================================================================
%% @doc Save all open editors.

save_all() ->
	Fun = fun({Index, Pid}) ->
		      save_document(Index, Pid)
		  end,
	lists:map(Fun, get_all_editors()).


%% =====================================================================
%% @doc

-spec open_document(Frame) -> 'ok' when
	Frame :: wxWindow:wxWindow().

open_document(Frame) ->
	case ide_io:open_new(Frame) of
		{cancel} ->
			ok;
		{Path, Filename, Contents} ->
			new_document_from_existing(Path, Filename, Contents)
	end.


%% =====================================================================
%% @doc Close the selected editor

close_selected_editor() ->
	case get_selected_editor() of
		{error, _} ->
			{error, no_open_editor};
		{ok, {Index, EditorPid}} ->
			close_editor(EditorPid, Index)
	end.


%% =====================================================================
%% @doc Close the selected editor
	
close_editor(EditorPid, Index) ->
	{Workspace,_Sb,Tab} = wx_object:call(?MODULE, workspace),
	case editor:is_dirty(EditorPid) of
		true ->
			io:format("Close dialog needs to be displayed.~n");
		_ -> %% Go ahead, close the editor
			ets:delete(Tab, editor:get_id(EditorPid)),
      wxAuiNotebook:deletePage(Workspace, Index)
	end.


%% =====================================================================
%% @doc Close all editor instances

close_all_editors() ->
	Fun = fun({Index,Pid}) ->
		      close_editor(Pid, Index)
		  end,
	lists:map(Fun, lists:reverse(get_all_editors())),
	ok.


%% =====================================================================
%% @doc Change the font style across all open editors

update_styles(Frame) ->
  %% Display the system font picker
  FD = wxFontData:new(),
  wxFontData:setInitialFont(FD, user_prefs:get_user_pref({pref, font})),
  Dialog = wxFontDialog:new(Frame, FD),
  case wxDialog:showModal(Dialog) of
    ?wxID_OK ->
      %% Get the user selected font, and update the editors
      Font = wxFontData:getChosenFont(wxFontDialog:getFontData(Dialog)),
      user_prefs:set_user_pref(font, Font),
      Fun = fun({_, Pid}) ->
              editor:set_font_style(Pid, Font)
					  end,
      lists:map(Fun, get_all_editors()),
      ok;
    ?wxID_CANCEL ->
				ok
	end.


%% =====================================================================
%% @doc Show the find/replace dialog
%% Might be better in editor.erl

find_replace(Parent) ->
  FindData = find_replace_data:new(),

  %% This data will eventually be loaded from transient/permanent storage PREFS!!
  find_replace_data:set_options(FindData, ?IGNORE_CASE bor ?WHOLE_WORD bor ?START_WORD),
  find_replace_data:set_search_location(FindData, ?FIND_LOC_DOC),

  case erlang:whereis(find_replace_dialog) of
    undefined ->
      find_replace_dialog:show(find_replace_dialog:new(Parent, FindData));
    Pid ->
      wxDialog:raise(find_replace_dialog:get_ref(Pid))
  end.


set_theme(ThemeMenu) ->
  {ok, Ckd} = get_checked_menu_item(wxMenu:getMenuItems(ThemeMenu)),
  Fun = fun({_, Pid}) ->
          editor:set_theme(Pid, wxMenuItem:getLabel(Ckd), user_prefs:get_user_pref({pref, font}))
        end,
  lists:map(Fun, get_all_editors()),
  user_prefs:set_user_pref(theme, wxMenuItem:getLabel(Ckd)).

get_current_theme_name() ->
  Frame = wx_object:call(?MODULE, frame),
  Mb = wxFrame:getMenuBar(Frame),
  Menu = wxMenuBar:findMenu(Mb, "View"),
  Item = wxMenu:findItem(wxMenuBar:getMenu(Mb, Menu), ?MENU_ID_THEME_SELECT),
  Itms = wxMenu:getMenuItems(wxMenuItem:getSubMenu(Item)),
  {ok, Ckd} = get_checked_menu_item(Itms),
  wxMenuItem:getLabel(Ckd).

get_checked_menu_item([]) ->
  {error, nomatch};
get_checked_menu_item([H|T]) ->
  case wxMenuItem:isChecked(H) of
    true ->
      {ok, H};
    _ ->
      get_checked_menu_item(T)
  end.

set_line_wrap(Menu) ->
  Bool = wxMenuItem:isChecked(wxMenu:findItem(Menu, ?MENU_ID_LINE_WRAP)),
  Fun = fun({_, Pid}) ->
          editor:set_line_wrap(Pid, Bool)
  end,
  lists:map(Fun, get_all_editors()),
  user_prefs:set_user_pref(line_wrap, Bool).

set_line_margin_visible(Menu) ->
  Bool = wxMenuItem:isChecked(wxMenu:findItem(Menu, ?MENU_ID_LN_TOGGLE)),
  Fun = fun({_, Pid}) ->
          editor:set_line_margin_visible(Pid, Bool)
  end,
  lists:map(Fun, get_all_editors()),
  user_prefs:set_user_pref(show_line_no, Bool).

set_indent_tabs(#wx{id=Id, event=#wxCommand{type=command_menu_selected}}) ->
  Cmd = case Id of
    ?MENU_ID_INDENT_SPACES -> false;
    ?MENU_ID_INDENT_TABS -> true
  end,
  Fun = fun({_, Pid}) ->
          editor:set_use_tabs(Pid, Cmd)
  end,
  lists:map(Fun, get_all_editors()),
  user_prefs:set_user_pref(use_tabs, Cmd).

set_indent_guides(Menu) ->
  Bool = wxMenuItem:isChecked(wxMenu:findItem(Menu, ?MENU_ID_INDENT_GUIDES)),
  Fun = fun({_, Pid}) ->
          editor:set_indent_guides(Pid, Bool)
  end,
  lists:map(Fun, get_all_editors()),
  user_prefs:set_user_pref(indent_guides, Bool).

indent_line_right() ->
  {ok,{_,Pid}} = get_selected_editor(),
  editor:indent_line_right(Pid),
  ok.

indent_line_left() ->
  {ok,{_,Pid}} = get_selected_editor(),
  editor:indent_line_left(Pid),
  ok.

comment() ->
  {ok,{_,Pid}} = get_selected_editor(),
  editor:comment(Pid),
  ok.

zoom_in() ->
  {ok,{_,Pid}} = get_selected_editor(),
  editor:zoom_in(Pid).

zoom_out() ->
  {ok,{_,Pid}} = get_selected_editor(),
  editor:zoom_out(Pid).

go_to_line(Parent) ->
  Dialog = wxDialog:new(Parent, ?wxID_ANY, "Go to Line"),
  %% Force events to propagate beyond this dialog
  wxDialog:setExtraStyle(Dialog, wxDialog:getExtraStyle(Dialog) band (bnot ?wxWS_EX_BLOCK_EVENTS)),

  Panel = wxPanel:new(Dialog),
  Sz = wxBoxSizer:new(?wxVERTICAL),
  wxSizer:addSpacer(Sz, 10),

  wxSizer:add(Sz, wxStaticText:new(Panel, ?wxID_ANY, "Enter line:"),
    [{border,10}, {flag, ?wxEXPAND bor ?wxLEFT}]),
  wxSizer:addSpacer(Sz, 7),
  Input = wxTextCtrl:new(Panel, ?wxID_ANY, []),
  wxSizer:add(Sz, Input, [{border,10}, {flag, ?wxEXPAND bor ?wxLEFT bor ?wxRIGHT}, {proportion, 1}]),
  wxSizer:addSpacer(Sz, 15),

  ButtonSz = wxBoxSizer:new(?wxHORIZONTAL),
  wxSizer:addSpacer(ButtonSz, 10),
  wxSizer:add(ButtonSz, wxButton:new(Panel, ?wxID_CANCEL,
    [{label,"Cancel"}]), [{border,10}, {flag, ?wxEXPAND bor ?wxBOTTOM}]),
  DefButton = wxButton:new(Panel, ?wxID_OK, [{label,"Go"}]),
  wxButton:setDefault(DefButton),
  wxSizer:add(ButtonSz, DefButton, [{border,10}, {flag, ?wxEXPAND bor ?wxBOTTOM bor ?wxLEFT}]),
  wxSizer:addSpacer(ButtonSz, 10),
  wxSizer:add(Sz, ButtonSz),

  Self = self(),
  wxButton:connect(DefButton, command_button_clicked, [{callback, fun(E,O)->Self ! done end}]),

  wxPanel:setSizer(Panel, Sz),
  wxSizer:layout(Sz),
  wxSizer:setSizeHints(Sz, Dialog),
  wxDialog:show(Dialog),
  wxWindow:setFocusFromKbd(Input),

  receive
    done ->
      {Line, Column} = case string:tokens(wxTextCtrl:getValue(Input), ":") of
        [Ln | []] -> {Ln, 0};
        [Ln, Col | _ ] -> {Ln, Col}
      end,
      L = try
        list_to_integer(Line)
      catch _:_ -> 0
      end,
      C = try
        list_to_integer(Column)
      catch _:_ -> 0
      end,
      wxDialog:destroy(Dialog),
      {ok,{_,Ed}} = doc_manager:get_selected_editor(),
      editor:go_to_position(Ed, {L, C})
  end,
  ok.


transform_selection(#wx{id=Id, event=#wxCommand{type=command_menu_selected}}) ->
	Cmd = case Id of
		?MENU_ID_UC_SEL -> uppercase;
		?MENU_ID_LC_SEL -> lowercase
	end,
	{ok,{_,Ed}} = doc_manager:get_selected_editor(),
	editor:transform_selection(Ed, {transform, Cmd}).
