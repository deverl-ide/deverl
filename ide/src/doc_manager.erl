-module(doc_manager).

-include_lib("wx/include/wx.hrl").
-include("ide.hrl").

-behaviour(wx_object).

-export([init/1, terminate/2,  code_change/3,
         handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).

% API
-export([
	start/1,
  new_file/1,
	new_document/0, 
	new_document/1, 
	new_document_from_existing/3,
	new_document_from_existing/4,
	new_project/1,
	close_project/0,
	close_active_document/0, 
	close_all_documents/0,
	get_active_document/0, 
	get_active_document_ref/0,
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
             		notebook :: wxAuiNotebook:wxAuiNotebook(),    %% Notebook
                document_ets,  %% A table containing data related to open editors (path/project etc.)
								active_project,
								sizer,
								parent
                }).


start(Config) ->
  wx_object:start_link({local, ?MODULE}, ?MODULE, Config, []).

init(Config) ->
	{Parent, Sb} = proplists:get_value(config, Config),

	Style = (0
			bor ?wxAUI_NB_TOP
			bor ?wxAUI_NB_WINDOWLIST_BUTTON
			bor ?wxAUI_NB_TAB_MOVE
			bor ?wxAUI_NB_SCROLL_BUTTONS
			bor ?wxAUI_NB_CLOSE_ON_ALL_TABS
			bor ?wxAUI_NB_TAB_SPLIT
			),
			
	Panel = wxPanel:new(Parent),
	Sz = wxBoxSizer:new(?wxVERTICAL),
	wxPanel:setSizer(Panel, Sz),

	Notebook = wxAuiNotebook:new(Panel, [{id, ?ID_WORKSPACE}, {style, Style}]),
	wxSizer:add(Sz, Notebook, [{flag, ?wxEXPAND}, {proportion, 1}]),
	wxSizer:hide(Sz, 0),

	%% Create Ets table to store data relating to any open documents
	DocEts = ets:new(editors, [set, public, {keypos,1}]),
	
	Close = fun(E,O) ->
            wxNotifyEvent:veto(O),
            close_document(wxAuiNotebookEvent:getSelection(O))
          end,
					
	Ph = lib_widgets:placeholder(Panel, "No Open Documents"),
	wxSizer:add(Sz, Ph, [{flag, ?wxEXPAND}, {proportion, 1}]),

	wxAuiNotebook:connect(Notebook, command_auinotebook_bg_dclick, []),
	wxAuiNotebook:connect(Notebook, command_auinotebook_page_close, [{callback,Close},{userData,DocEts}]),
	wxAuiNotebook:connect(Notebook, command_auinotebook_page_changed),

  {Panel, #state{notebook=Notebook, status_bar=Sb, document_ets=DocEts, sizer=Sz, parent=Parent}}.
	

handle_info(Msg, State) ->
	io:format("Got Info ~p~n",[Msg]),
	{noreply,State}.

handle_cast(notebook_empty, State=#state{sizer=Sz}) ->
	%% Called when the last document is closed.
	show_placeholder(Sz),
	ide:set_title([]),
	{noreply, State};
handle_cast({active_project,Proj}, State) ->
	{noreply, State#state{active_project=Proj}}.

handle_call(notebook, _, State=#state{notebook=Nb, status_bar=Sb, document_ets=Tb}) ->
	{reply, {Nb,Sb,Tb}, State};
handle_call({new_document, Filename, Options}, _, 
						State=#state{notebook=Notebook, document_ets=DocEts, status_bar=Sb, sizer=Sz, parent=Parent}) ->
	Editor = new_document(Notebook, DocEts, Sb, Filename, Parent, Sz, Options),
	{reply, Editor, State};
handle_call(active_project, _, State=#state{active_project=Proj}) ->
	{reply, Proj, State};
handle_call(close_project, _, 
						State=#state{active_project=Proj, notebook=Notebook, document_ets=DocEts}) ->
	%% Check open files, save/close
	case Proj of
		undefined -> ok;
		Project={Item,Root} ->
			wxAuiNotebook:disconnect(Notebook, command_auinotebook_page_changed),
			Pages = get_active_project_documents(Project, DocEts, Notebook),
			lists:foreach(fun(E) -> close_document(Notebook, DocEts, get_notebook_index(Notebook, E)) end, tl(Pages)),
			wxAuiNotebook:connect(Notebook, command_auinotebook_page_changed),
			close_document(Notebook, DocEts, get_notebook_index(Notebook, hd(Pages))),
			ide_projects_tree:delete_project(Item)
	end,
	{reply, ok, State};
handle_call(close_all, _, State=#state{active_project=Proj, notebook=Notebook, document_ets=DocEts}) ->
	%% CLOSE PROJECTS FIRST (CANT JUST DELETE PROJ TREE - USER MIGHT CANCEL A CLOSE IF NOT SAVED)
	wxAuiNotebook:disconnect(Notebook, command_auinotebook_page_changed),
	Count = wxAuiNotebook:getPageCount(Notebook),
	lists:foreach(fun(E) -> close_document(Notebook, DocEts, E) end, 
		lists:reverse(lists:seq(0, Count-1))),
	wxAuiNotebook:connect(Notebook, command_auinotebook_page_changed),
	{reply, ok, State}.

code_change(_, _, State) ->
	{stop, ignore, State}.

terminate(_Reason, State=#state{notebook=Nb}) ->
	io:format("TERMINATE DOC_MANAGER~n"),
	wxAuiNotebook:destroy(Nb).

%% =====================================================================
%% AUI handlers
%%
%% =====================================================================

handle_event(#wx{obj=Notebook, event = #wxAuiNotebook{type=command_auinotebook_page_changed,
			selection=Index}}, State=#state{document_ets=DocEts, notebook=Nb, status_bar=Sb}) ->
	PageText = wxAuiNotebook:getPageText(Notebook, Index),
  % Make sure editor knows (needs to update sb)
  editor:selected(index_to_ref(DocEts, Notebook, Index), Sb),
	Proj = lookup_project(DocEts, wxAuiNotebook:getPage(Notebook, Index)),
	Str = case Proj of
		undefined -> 
			ide_menu:update_label(ide:get_menubar(), ?MENU_ID_CLOSE_PROJECT, "Close Project"),
			PageText;
		{_, Path} -> 
			Name = filename:basename(Path),
			ide_menu:update_label(ide:get_menubar(), ?MENU_ID_CLOSE_PROJECT, "Close Project (" ++ Name ++ ")"),
			PageText ++ " (" ++ Name ++ filename:extension(Path) ++ ")"
	end,
	ide:set_title(Str),
	
  {noreply, State#state{active_project=Proj}};
handle_event(#wx{event=#wxAuiNotebook{type=command_auinotebook_bg_dclick}}, 
						 State=#state{notebook=Nb, status_bar=Sb, document_ets=DocEts, parent=Parent, sizer=Sz}) ->
	new_document(Nb, DocEts, Sb, ?DEFAULT_TAB_LABEL, Parent, Sz, []),
  {noreply, State}.


%% =====================================================================
%% @doc Add a new document to the notebook
	
new_document() -> 
	new_document(?DEFAULT_TAB_LABEL).
	
	
%% =====================================================================
%% @doc Add a new document to the notebook with filename

new_document(Filename) ->
	wx_object:call(?MODULE, {new_document, Filename, []}),
	ok.

%% @private
%% @hidden
new_document(Notebook, DocEts, Sb, Filename, Parent, Sz, Options)	->
	case wxWindow:isShown(Notebook) of
		false -> 
			show_notebook(Sz);
		true -> ok
	end,
	Editor = editor:start([{parent, Parent}, {status_bar, Sb}, {font,user_prefs:get_user_pref({pref, font})}]),
	Index = insert_page(Notebook, Editor, Filename), %% Page changed event not serviced until this completes
	insert_rec(DocEts, Index, Editor, proplists:get_value(path, Options), proplists:get_value(project, Options)),
	Editor.
	
	
%% =====================================================================
%% @doc Add an existing document to the notebook.
		
new_document_from_existing(Path, Filename, Contents) -> 
	new_document_from_existing(Path, Filename, Contents, []).


%% =====================================================================
%% @doc Add an existing document to the notebook.

new_document_from_existing(Path, Filename, Contents, Options) -> 
	Editor = wx_object:call(?MODULE, {new_document, Filename, [{path, Path}] ++ Options}),
	editor:set_text(Editor, Contents),
	editor:empty_undo_buffer(Editor),
	editor:set_savepoint(Editor),
	editor:link_poller(Editor, Path),
  ok.


%% =====================================================================
%% @doc Display the "New File" dialog.
  
new_file(Parent) ->
  OpenProjects = get_open_projects(),
  case get_active_project() of
    undefined ->
      Dialog = new_file:start({Parent, OpenProjects, "No Project"});
    {_, ActiveProject} ->
      Dialog = new_file:start({Parent, OpenProjects, filename:basename(ActiveProject)})
  end,
  case wxDialog:showModal(Dialog) of
    ?wxID_CANCEL ->
      ok;
    ?wxID_OK ->
      ok
  end.
      
			
%% =====================================================================
%% @doc Add a new project.

new_project(Parent) ->
	Dialog = new_project_wx:start(Parent),
	case new_project_wx:showModal(Dialog) of
		?wxID_CANCEL -> 
      ok;
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
			lib_dialog_wx:msg_error(Parent, E)
  end.
	
	
%% =====================================================================
%% @doc Close an open project.
%% This will close any files belonging to the project, and remove the
%% tree from the project tree. 

close_project() ->
	%% Check open files, save/close
	wx_object:call(?MODULE, close_project).


%% =====================================================================
%% @doc Open an existing project.

open_project(Frame) ->
	case lib_dialog_wx:get_existing_dir(Frame) of
		cancelled -> ok;
		Path -> ide_projects_tree:add_project(Path)
	end,
	ok.


%% =====================================================================
%% @doc Get the currently active project.
%% This is either the project to which the active document belongs, or
%% the last clicked item in the project tree if this is more recent.

get_active_project() ->
	wx_object:call(?MODULE, active_project).
	
	
%% =====================================================================
%% @doc Set the currently active project.

set_active_project(Project) ->
	wx_object:cast(?MODULE, {active_project, Project}).	
	
	
%% =====================================================================
%% @doc Returns a list containing all open 
%% documents belonging to the project Project.
	
get_active_project_documents(Project, DocEts, Notebook) ->
	ets:foldl(
		fun(Record, Acc) ->
			case record_get_project(Record) of
				Project ->
					[record_get_key(Record) | Acc];
				_ -> Acc
			end
		end, [], DocEts).


%% =====================================================================
%% @doc Gets the index of the currently active document, i.e. the 
%% document most recently in focus.

get_active_document() ->
	{Notebook,_,_} = wx_object:call(?MODULE, notebook),
	case wxAuiNotebook:getSelection(Notebook) of %% Get the index of the tab
		-1 -> %% no editor instance
				{error, no_open_editor};
		Index ->
				Index
	end.

	
%% =====================================================================
%% @doc Gets the reference to the currently active document.

get_active_document_ref() ->
	{Notebook,_,DocEts} = wx_object:call(?MODULE, notebook),
	index_to_ref(DocEts, Notebook, get_active_document()).
	

%% =====================================================================
%% @doc Get all open editor instances.

get_open_documents() ->
	{Notebook,_,_} = wx_object:call(?MODULE, notebook),
	Count = wxAuiNotebook:getPageCount(Notebook),
	lists:seq(0, Count - 1).


%% =====================================================================
%% @doc

-spec get_open_projects() -> Result when
	Result :: [string()].

get_open_projects() ->
  OpenProjects = ide_projects_tree:get_open_projects(),
  get_open_projects(OpenProjects, []).
get_open_projects([], Acc) ->
  Acc;
get_open_projects([{_,Path}|Projects], Acc) ->
  get_open_projects(Projects, Acc ++ [Path]).


%% =====================================================================
%% Open/save/close editor functions
%%
%% =====================================================================

%% =====================================================================
%% @doc Save the currently selected document to disk

save_current_document() ->
	case get_active_document() of
		{error, no_open_editor} -> ok;
		Index ->
			save_document(Index)
	end.


%% =====================================================================
%% @doc Save the contents of the editor located at index Index.

save_document(Index) ->  
	{Notebook,Sb,DocEts} = wx_object:call(?MODULE, notebook), 
	case editor:is_dirty(index_to_ref(DocEts, Notebook, Index)) of
		false -> ok;
		_ ->
			save_document(Sb, get_record(DocEts, index_to_key(Notebook, Index)))
	end.
	
save_document(Sb, Record) ->
	case record_get_path(Record) of
		undefined -> save_new_document();
		Path -> %% Document already exists, overwrite
			ide_io:save(Path, editor:get_text(record_get_ref(Record))),
			editor:set_savepoint(record_get_ref(Record)),
			ide_status_bar:set_text_timeout(Sb, {field, help}, "Document saved.")
	end.


%% =====================================================================
%% @doc Save the erlang editor with pid Pid located at index position
%% Index to disk. The user will be prompted for a save path.

save_new_document() ->
	case get_active_document() of
		{error, no_open_editor} -> ok;
		Index ->
			save_new_document(Index)
	end.

save_new_document(Index) ->
	{Notebook,Sb,DocEts} = wx_object:call(?MODULE, notebook), 
	Contents = editor:get_text(index_to_ref(DocEts, Notebook, Index)),
	case ide_io:save_as(Notebook, Contents) of
		{cancel} ->
			ide_status_bar:set_text_timeout(Sb, {field, help}, "Document not saved.");
		{ok, {Path, Filename}}  ->
			update_path(DocEts, index_to_key(Notebook, Index), Path), 
			wxAuiNotebook:setPageText(Notebook, Index, Filename),
			editor:set_savepoint(index_to_ref(DocEts, Notebook, Index)),
			editor:link_poller(index_to_ref(DocEts, Notebook, Index), Path),
			ide_status_bar:set_text_timeout(Sb, {field, help}, "Document saved.")
	end,
	ok.
	

%% =====================================================================
%% @doc Save all open editors.

save_all() ->
	lists:map(fun save_document/1, get_open_documents()).


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
		Index ->
			close_document(Index)
	end.


%% =====================================================================
%% @doc Close the selected editor
	
close_document(Index) ->
	{Notebook,_Sb,DocEts} = wx_object:call(?MODULE, notebook),
	Key = wxAuiNotebook:getPage(Notebook, Index),
	case editor:is_dirty(get_ref(DocEts, Key)) of
		true ->
			io:format("File modified since last save, display save/unsave dialog.~n");
		_ -> %% Go ahead, close the editor
			delete_record(DocEts, Key),
      wxAuiNotebook:deletePage(Notebook, Index)
	end,
	case wxAuiNotebook:getPageCount(Notebook) of
		0 -> wx_object:cast(?MODULE, notebook_empty);
		_ -> ok
	end,
	ok.

close_document(Notebook, DocEts, Index) ->
 	Key = wxAuiNotebook:getPage(Notebook, Index),
 	case editor:is_dirty(get_ref(DocEts, Key)) of
 		true ->
 			io:format("File modified since last save, display save/unsave dialog.~n");
 		_ -> %% Go ahead, close the editor
 			delete_record(DocEts, Key),
       wxAuiNotebook:deletePage(Notebook, Index)
 	end,
 	case wxAuiNotebook:getPageCount(Notebook) of
 		0 -> wx_object:cast(?MODULE, notebook_empty);
 		_ -> ok
 	end.

%% =====================================================================
%% @doc Close all editor instances,
%% Note: must always delete documents starting with the highest index
%% first, as the notebook shifts all indexes dwon when one is deleted.

close_all_documents() ->
	wx_object:call(?MODULE, close_all).


%% =====================================================================
%% @doc The following functions operate on all open documents.
	
set_theme(ThemeMenu) ->
  {ok, Ckd} = ide_menu:get_checked_menu_item(wxMenu:getMenuItems(ThemeMenu)),
	apply_to_all_documents(fun editor:set_theme/3, [wxMenuItem:getLabel(Ckd), 
		user_prefs:get_user_pref({pref, font})]),
  user_prefs:set_user_pref(theme, wxMenuItem:getLabel(Ckd)).
	
set_line_wrap(Menu) ->
  Bool = wxMenuItem:isChecked(wxMenu:findItem(Menu, ?MENU_ID_LINE_WRAP)),
	apply_to_all_documents(fun editor:set_line_wrap/2, [Bool]),
  user_prefs:set_user_pref(line_wrap, Bool).

set_line_margin_visible(Menu) ->
  Bool = wxMenuItem:isChecked(wxMenu:findItem(Menu, ?MENU_ID_LN_TOGGLE)),
	apply_to_all_documents(fun editor:set_line_margin_visible/2, [Bool]),
  user_prefs:set_user_pref(show_line_no, Bool).

set_indent_tabs(#wx{id=Id, event=#wxCommand{type=command_menu_selected}}) ->
  Cmd = case Id of
    ?MENU_ID_INDENT_SPACES -> false;
    ?MENU_ID_INDENT_TABS -> true
  end,
	apply_to_all_documents(fun editor:set_use_tabs/2, [Cmd]),
  user_prefs:set_user_pref(use_tabs, Cmd).

set_indent_guides(Menu) ->
  Bool = wxMenuItem:isChecked(wxMenu:findItem(Menu, ?MENU_ID_INDENT_GUIDES)),
	apply_to_all_documents(fun editor:set_indent_guides/2, [Bool]),
  user_prefs:set_user_pref(indent_guides, Bool).

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
			apply_to_all_documents(fun editor:set_font_style/2, [Font]),		
      ok;
    ?wxID_CANCEL ->
				ok
	end.

apply_to_all_documents(Fun, Args) ->
	{Notebook,_,DocEts} = wx_object:call(?MODULE, notebook),
	Fun1 = fun(E) -> index_to_ref(DocEts, Notebook, E) end,
  case get_open_documents() of
		[] -> ok;
		Docs ->
			List = lists:map(Fun1, get_open_documents()),
			Fun2 = fun(E) -> apply(Fun, [E | Args]) end,
			lists:foreach(Fun2, List)
	end.
	
	
%% =====================================================================
%% @doc Apply the function Fun to the active document
%% Equivalent to apply_to_active_document(Fun, []), although this should
%% be quicker.
		
apply_to_active_document(Fun) ->
	{Notebook,_,DocEts} = wx_object:call(?MODULE, notebook),
	try
		Index = get_active_document(),
		Fun(index_to_ref(DocEts, Notebook, Index))
	catch
		error:E ->
			lib_dialog_wx:msg_notice(Notebook, "There are no documents currently open.")
	end.
	
apply_to_active_document(Fun, Args) ->
	{Notebook,_,DocEts} = wx_object:call(?MODULE, notebook),
	try
		Index = get_active_document(),
		apply(Fun, [index_to_ref(DocEts, Notebook, Index) | Args])
	catch
		error:_ -> 
			lib_dialog_wx:msg_notice(Notebook, "There are no documents currently open.")
	end.


%% =====================================================================
%% @doc The following functions operate on a single document.

transform_selection(#wx{id=Id, event=#wxCommand{type=command_menu_selected}}) ->
	Cmd = case Id of
		?MENU_ID_UC_SEL -> uppercase;
		?MENU_ID_LC_SEL -> lowercase
	end,
	apply_to_active_document(fun editor:transform_selection/2, [{transform, Cmd}]).
		
comment() -> apply_to_active_document(fun editor:comment/1).
zoom_in() -> apply_to_active_document(fun editor:zoom_in/1).
zoom_out() -> apply_to_active_document(fun editor:zoom_out/1).
indent_line_left() -> apply_to_active_document(fun editor:indent_line_left/1).
indent_line_right() -> apply_to_active_document(fun editor:indent_line_right/1).



insert_page(Notebook, Page) ->
	insert_page(Notebook, Page, ?DEFAULT_TAB_LABEL).
	
insert_page(Notebook, Page, Filename) ->
	wxAuiNotebook:addPage(Notebook, Page, Filename, [{select, true}]),
	wxAuiNotebook:getPage(Notebook, get_notebook_index(Notebook, Page)).
	
get_notebook_index(Notebook, Page) ->
	wxAuiNotebook:getPageIndex(Notebook, Page).
	
%% =====================================================================
%% Functions for accessing/modifying the document Ets table.
%% Always use these so that we can change the tables structure 
%% =====================================================================

index_to_key(Notebook, Index) ->
	wxAuiNotebook:getPage(Notebook, Index).

	
index_to_ref(DocEts, Notebook, Index) ->
	get_ref(DocEts, index_to_key(Notebook, Index)).
	
index_to_ref(Index) ->
	{Notebook,_,DocEts} = wx_object:call(?MODULE, notebook),
	index_to_ref(DocEts, Notebook, Index).	
	
get_ref(DocEts, Key) ->
	ets:lookup_element(DocEts, Key, 2).
	
get_record(DocEts, Key) ->
	hd(ets:lookup(DocEts, Key)).

delete_record(DocEts, Key) ->
	ets:delete(DocEts, Key).
	
lookup_project(DocEts, Key) ->
	{project, Proj} = ets:lookup_element(DocEts, Key, 4),
	Proj.
	
lookup_path(DocEts, Key) ->
	{path, Path} = ets:lookup_element(DocEts, Key, 3),
	Path.	
	
update_path(DocEts, Key, Path) ->
	ets:update_element(DocEts, Key, {3, {path, Path}}).


%% =====================================================================
%% @doc Insert a new record into the Ets table.

insert_rec(DocEts, Index, EditorRef, Path) ->
	insert_rec(DocEts, Index, EditorRef, Path, undefined).
	
insert_rec(DocEts, Index, EditorRef, Path, Project) ->
	ets:insert(DocEts, {Index, EditorRef, {path, Path}, {project, Project}}).
	

%% =====================================================================
%% The following functions access individuals elements from a single
%% record. 
%% NOTE: Always use these functions to access a record's elements, as 
%% the representation is internal and can be changed without notice.
	
record_get_key({Key,_,_,_}) -> Key.
record_get_ref({_,Ref,_,_}) ->	Ref.
record_get_path({_,_,{path,Path},_}) -> Path.
record_get_project({_,_,_,{project, Project}}) -> Project.

	
%% =====================================================================
%% @doc Display the notebook, hiding and other siblings.

show_notebook(Sz) ->
	wxSizer:hide(Sz, 1),
	wxSizer:show(Sz, 0),
	wxSizer:layout(Sz).


%% =====================================================================
%% @doc Display the placeholder, hiding any other siblings.
	
show_placeholder(Sz) ->
	wxSizer:hide(Sz, 0),
	wxSizer:show(Sz, 1),
	wxSizer:layout(Sz).
	
	
%% =====================================================================
%% @doc Show the find/replace dialog
%% Might be better in editor.erl

find_replace(Parent) ->
  FindData = find_replace_data:new(),
  find_replace_data:set_options(FindData, ?IGNORE_CASE bor ?WHOLE_WORD bor ?START_WORD),
  find_replace_data:set_search_location(FindData, ?FIND_LOC_DOC),
  case erlang:whereis(find_replace_dialog) of
    undefined ->
      find_replace_dialog:show(find_replace_dialog:new(Parent, FindData));
    Pid ->
      wxDialog:raise(find_replace_dialog:get_ref(Pid))
  end.


%% =====================================================================
%% @doc Display the goto line dialog
	
go_to_line(Parent) ->
	Callback =
	fun(#wx{obj=Dialog, id=?wxID_OK, userData=Input},O) -> %% OK clicked
		wxEvent:skip(O),
	  {Line, Column} = case string:tokens(wxTextCtrl:getValue(Input), ":") of
			[] ->  {0, 0};
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
		editor:go_to_position(get_active_document_ref(), {L,C});
	(_,O) -> wxEvent:skip(O) %% Cancel/Close
	end,
	{Ln, Col} = editor:get_current_pos(get_active_document_ref()),
	lib_dialog_wx:text_input_dialog(Parent, "Go to Line", "Enter line:", "Go", 
		[{callback, Callback}, {init_text, integer_to_list(Ln)++":"++integer_to_list(Col)}]).
