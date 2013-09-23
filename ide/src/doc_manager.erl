-module(doc_manager).

-include_lib("wx/include/wx.hrl").
-include("ide.hrl").

-behaviour(wx_object).

-export([init/1, terminate/2,  code_change/3,
         handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).

% API
-export([
	start/1,
	new_document/0, 
	new_document/1, 
	new_document_from_existing/3,
	new_document_from_existing/4,
	new_project/1,
	close_project/0,
	close_active_document/0, 
	close_all_documents/0,
	get_active_document/0, 
	set_active_project/1,
	get_open_documents/0, 
	update_styles/1, 
	save_current_document/0,
	save_new_document/0, 
	save_all/0,
	save_document/2,
	open_document/1,
	open_project/1,
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
                document_ets :: {integer(), pid()},              %% A table containing the Id returned when an editor is created, and the associated pid
								active_project
                }).


start(Config) ->
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
	Editor = {_,Id,_,Pid} = editor:start([{parent, Workspace}, {status_bar, Sb},
                           {font, user_prefs:get_user_pref({pref, font})}]), %% Returns an editor instance inside a wxPanel

	DocEts = ets:new(editors, [public]),
	insert_record(DocEts, Id, Pid, undefined),

	wxAuiNotebook:addPage(Workspace, Editor, ?DEFAULT_TAB_LABEL, []),

	wxAuiManager:addPane(Manager, Workspace, Pane),

	Close = fun(_,O) ->
            wxNotifyEvent:veto(O),
            close_active_document()
          end,

	wxAuiNotebook:connect(Workspace, command_auinotebook_bg_dclick, []),
	wxAuiNotebook:connect(Workspace, command_auinotebook_page_close, [{callback,Close},{userData,DocEts}]),
	wxAuiNotebook:connect(Workspace, command_auinotebook_page_changed),

  {Workspace, #state{workspace=Workspace, manager=Manager, status_bar=Sb, document_ets=DocEts}}.

handle_info(Msg, State) ->
	io:format("Got Info ~p~n",[Msg]),
	{noreply,State}.

handle_cast({active_project,Proj}, State) ->
	{noreply, State#state{active_project=Proj}};
handle_cast(Msg, State) ->
	io:format("Got cast ~p~n",[Msg]),
	{noreply,State}.

handle_call(workspace, _, State=#state{workspace=Ws, status_bar=Sb, document_ets=Tb}) ->
	{reply, {Ws,Sb,Tb}, State};
handle_call(active_project, _, State=#state{active_project=Proj}) ->
	{reply, Proj, State};
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

handle_event(#wx{obj = _Workspace, event = #wxAuiNotebook{type=command_auinotebook_page_changed,
			selection = Index}}, State=#state{document_ets=Ets, workspace=Ws, status_bar=Sb}) ->
  %% Make sure editor knows (needs to update sb)
	Pid = get_editor_pid(Index, Ws, Ets),
  editor:selected(Pid, Sb),
	Id = editor:get_id(Pid),
	[{_,_,_,{project, Proj}}] = ets:lookup(Ets, Id),
	io:format("PAGE CHANGED PROJ: ~p~n", [Proj]),
  {noreply, State#state{active_project=Proj}};
  % {noreply, State};
handle_event(#wx{event=#wxAuiNotebook{type=command_auinotebook_bg_dclick}}, State) ->
  new_document(State#state.workspace, State#state.status_bar, 
             State#state.document_ets),
  {noreply, State};
handle_event(Ev = #wx{}, State = #state{}) ->
  io:format("Got Event ~p~n",[Ev]),
  {noreply,State}.


%% =====================================================================
%% @doc
%%
%% @private

new_document(Workspace, Sb, DocEts) ->
	new_document(Workspace, ?DEFAULT_TAB_LABEL, Sb, DocEts),
	Workspace.


%% =====================================================================
%% @doc Create a new editor instance in the notebook

new_document() -> 
	new_document(?DEFAULT_TAB_LABEL).
  
%% @doc Create a new editor with specified documentname
new_document(Filename) ->
	{Workspace, Sb, DocEts} = wx_object:call(?MODULE, workspace), 
	new_document(Workspace, Filename, Sb, DocEts),
	ok.

%% @private
new_document(Workspace, Filename, Sb, DocEts) ->
	Editor = {_,Id,_,Pid} =editor:start([{parent, Workspace}, {status_bar, Sb}, {font,user_prefs:get_user_pref({pref, font})}]),
	wxAuiNotebook:addPage(Workspace, Editor, Filename, [{select, true}]),
	insert_record(DocEts, Id, Pid, undefined).
	
	
%% =====================================================================
%% @doc Add an existing document to the workspace.
		
new_document_from_existing(Path, Filename, Contents) -> 
	new_document_from_existing(Path, Filename, Contents, []).

new_document_from_existing(Path, Filename, Contents, Options) -> 
	{Workspace, Sb, DocEts} = wx_object:call(?MODULE, workspace), 
	Editor = {_,Id,_,Pid} = editor:start([{parent, Workspace}, {status_bar, Sb}, 
		{font,user_prefs:get_user_pref({pref, font})}]),
	insert_record(DocEts, Id, Pid, Path, proplists:get_value(project, Options)),
	wxAuiNotebook:addPage(Workspace, Editor, Filename, [{select, true}]),
	editor:set_text(Pid, Contents),
	editor:empty_undo_buffer(Pid),
	editor:set_savepoint(Pid),
	editor:link_poller(Pid, Path),
	ok.
	
new_project(Parent) ->
	Dialog = new_project_wx:start(Parent),
	case new_project_wx:showModal(Dialog) of
		?wxID_CANCEL -> ok;
		?wxID_OK ->
			new_project(Parent, Dialog),
			new_project_wx:close(Dialog)
	end.
	
new_project(Parent, Dialog) ->
  try
		Path = ide_io:create_directory_structure(Parent, 
			new_project_wx:get_name(Dialog), new_project_wx:get_path(Dialog)),
		ide_projects_tree:add_project(Path)
  catch
    throw:E -> 
			lib_dialog_wx:error_msg(Parent, E)
  end.
	
close_project() ->
	%% Check open files, save/close
	case get_active_project() of
		undefined -> ok;
		Project={Item,Root} ->
			{Workspace, Sb, DocEts} = wx_object:call(?MODULE, workspace), 
			List = get_active_project_records(Project, DocEts),
			close_project(Workspace, List),
			ide_projects_tree:delete_project(Item),
			set_active_project(undefined)
	end.
	
close_project(_,[]) -> ok;
close_project(Workspace, [{Id,Pid,_,_}|T]) ->
	close_document(Pid, get_document_index(Workspace, Pid)),
	close_project(Workspace, T).

%% =====================================================================
%% @doc

open_project(Frame) ->
	case lib_dialog_wx:get_existing_dir(Frame) of
		cancelled -> ok;
		Path -> ide_projects_tree:add_project(Path)
	end,
	ok.

get_active_project() ->
	wx_object:call(?MODULE, active_project).
	
set_active_project(Project) ->
	wx_object:cast(?MODULE, {active_project, Project}).	
	
get_active_project_records(Project, DocEts) ->
	ets:foldl(
		fun({Id, Pid, {path, Path}, {project, Proj}}=Record, Acc) when Proj =:= Project ->
			[Record | Acc];
		(_, Acc) ->
			Acc
		end, [], DocEts).
	
get_document_index(Workspace, Pid) ->
	wxAuiNotebook:getPageIndex(Workspace, editor:get_ref(Pid)).
	

%% =====================================================================
%% @doc Get the Pid of an editor instance at position Index.

-spec get_editor_pid(Index) -> Result when
	Index :: integer(),
	Result :: pid().

get_editor_pid(Index) ->
	{Workspace, _, DocEts} = wx_object:call(?MODULE, workspace),
	get_editor_pid(Index, Workspace, DocEts).

-spec get_editor_pid(Index, Workspace, DocEts) -> Result when
	Index :: integer(),
	Workspace :: wxAuiNotebook:wxAuiNotebook(),
	DocEts :: term(),
	Result :: pid().

get_editor_pid(Index, Workspace, DocEts) ->
	{_,Key,_,_} = wxAuiNotebook:getPage(Workspace, Index),
	[{_,Pid,_,_}] = ets:lookup(DocEts, Key),
	Pid.


%% =====================================================================
%% @doc Get the Pid of the currently selected editor.

-spec get_active_document() -> Result when
	Result :: {'error', 'no_open_editor'} |
			  {'ok', {integer(), pid()}}.

get_active_document() ->
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

-spec get_open_documents() -> Result when
	Result :: [{integer(), pid()}].

get_open_documents() ->
	{Workspace,_,_} = wx_object:call(?MODULE, workspace),
	Count = wxAuiNotebook:getPageCount(Workspace),
	get_open_documents(Workspace, Count - 1, []).

get_open_documents(_, -1, Acc) ->
	Acc;
get_open_documents(Workspace, Count, Acc) ->
	get_open_documents(Workspace, Count -1, [{Count, get_editor_pid(Count)} | Acc]).


%% =====================================================================
%% Open/save/close editor functions
%%
%% =====================================================================


%% =====================================================================
%% @doc Save the currently selected document to disk

save_current_document() ->
	case get_active_document() of
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
save_document(Sb, {_Id, Pid, {path, undefined}, _}) ->
	save_new_document();
save_document(Sb, {_Id, Pid, {path, Path}, _}) ->
	%% Document already exists, overwrite
	ide_io:save(Path, editor:get_text(Pid)),
	editor:set_savepoint(Pid),
	ide_status_bar:set_text_timeout(Sb, {field, help}, "Document saved.").

	
%% =====================================================================
%% @doc Save the erlang editor with pid Pid located at index position
%% Index to disk. The user will be prompted for a save path.

save_new_document() ->
	case get_active_document() of
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
	lists:map(Fun, get_open_documents()).


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

close_active_document() ->
	case get_active_document() of
		{error, _} ->
			{error, no_open_editor};
		{ok, {Index, EditorPid}} ->
			close_document(EditorPid, Index)
	end.


%% =====================================================================
%% @doc Close the selected editor
	
close_document(EditorPid, Index) ->
	{Workspace,_Sb,DocEts} = wx_object:call(?MODULE, workspace),
	case editor:is_dirty(EditorPid) of
		true ->
			io:format("File modified since last save, display save/unsave dialog.~n");
		_ -> %% Go ahead, close the editor
			ets:delete(DocEts, editor:get_id(EditorPid)),
      wxAuiNotebook:deletePage(Workspace, Index)
	end.


%% =====================================================================
%% @doc Close all editor instances

close_all_documents() ->
	Fun = fun({Index,Pid}) ->
		      close_document(Pid, Index)
		  end,
	lists:map(Fun, lists:reverse(get_open_documents())),
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
      lists:map(Fun, get_open_documents()),
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
  lists:map(Fun, get_open_documents()),
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
  lists:map(Fun, get_open_documents()),
  user_prefs:set_user_pref(line_wrap, Bool).

set_line_margin_visible(Menu) ->
  Bool = wxMenuItem:isChecked(wxMenu:findItem(Menu, ?MENU_ID_LN_TOGGLE)),
  Fun = fun({_, Pid}) ->
          editor:set_line_margin_visible(Pid, Bool)
  end,
  lists:map(Fun, get_open_documents()),
  user_prefs:set_user_pref(show_line_no, Bool).

set_indent_tabs(#wx{id=Id, event=#wxCommand{type=command_menu_selected}}) ->
  Cmd = case Id of
    ?MENU_ID_INDENT_SPACES -> false;
    ?MENU_ID_INDENT_TABS -> true
  end,
  Fun = fun({_, Pid}) ->
          editor:set_use_tabs(Pid, Cmd)
  end,
  lists:map(Fun, get_open_documents()),
  user_prefs:set_user_pref(use_tabs, Cmd).

set_indent_guides(Menu) ->
  Bool = wxMenuItem:isChecked(wxMenu:findItem(Menu, ?MENU_ID_INDENT_GUIDES)),
  Fun = fun({_, Pid}) ->
          editor:set_indent_guides(Pid, Bool)
  end,
  lists:map(Fun, get_open_documents()),
  user_prefs:set_user_pref(indent_guides, Bool).

indent_line_right() ->
  {ok,{_,Pid}} = get_active_document(),
  editor:indent_line_right(Pid),
  ok.

indent_line_left() ->
  {ok,{_,Pid}} = get_active_document(),
  editor:indent_line_left(Pid),
  ok.

comment() ->
  {ok,{_,Pid}} = get_active_document(),
  editor:comment(Pid),
  ok.

zoom_in() ->
  {ok,{_,Pid}} = get_active_document(),
  editor:zoom_in(Pid).

zoom_out() ->
  {ok,{_,Pid}} = get_active_document(),
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
      {ok,{_,Ed}} = doc_manager:get_active_document(),
      editor:go_to_position(Ed, {L, C})
  end,
  ok.


transform_selection(#wx{id=Id, event=#wxCommand{type=command_menu_selected}}) ->
	Cmd = case Id of
		?MENU_ID_UC_SEL -> uppercase;
		?MENU_ID_LC_SEL -> lowercase
	end,
	{ok,{_,Ed}} = doc_manager:get_active_document(),
	editor:transform_selection(Ed, {transform, Cmd}).


insert_record(DocEts, Id, Pid, Path) ->
	insert_record(DocEts, Id, Pid, Path, undefined).
	
insert_record(DocEts, Id, Pid, Path, Project) ->
	ets:insert(DocEts, {Id, Pid, {path, Path}, {project, Project}}),
	ok.