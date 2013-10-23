%% =====================================================================
%% @author
%% @copyright
%% @title
%% @version
%% @doc This module manages documents in the notebook.
%% @end
%% =====================================================================

-module(doc_manager).

-include_lib("wx/include/wx.hrl").
-include("ide.hrl").

%% wx_object
-behaviour(wx_object).
-export([init/1, terminate/2,  code_change/3,
         handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).

% API
-export([
	start/1,
  new_file/1,
	new_document/0, 
	new_document/1, 
	new_document_from_existing/2,
	new_document_from_existing/3,
	close_project/0,
	close_active_document/0, 
	close_all_documents/0,
	get_active_document/0, 
	get_active_document_ref/0,
	get_open_documents/0, 
	save_current_document/0,
	save_new_document/0, 
	save_all/0,
	save_document/1,
	open_document/1,
  apply_to_active_document/1,
  apply_to_all_documents/2
	]).

%% Server state
-record(state, {
								status_bar,
             		notebook :: wxAuiNotebook:wxAuiNotebook(),    %% Notebook
                document_ets,  %% A table containing data related to open editors (path/project id etc.)
								sizer,
								parent
                }).

								
%% =====================================================================
%% Client API
%% =====================================================================

start(Config) ->
  wx_object:start_link({local, ?MODULE}, ?MODULE, Config, []).
	
	
%% =====================================================================
%% @doc Create a new file and load it into the notebook.
  
new_file(Parent) ->
  OpenProjects = project_manager:get_open_projects(),
  
  case project_manager:get_active_project() of
    undefined ->
      Dialog = new_file:start({Parent, OpenProjects, "No Project"});
    ProjectId ->
      Dialog = new_file:start({Parent, OpenProjects, project_manager:get_name(ProjectId)})
  end,
  case wxDialog:showModal(Dialog) of
    ?wxID_CANCEL ->
      ok;
    ?wxID_OK ->
      ok
  end.  
	
	
%% =====================================================================
%% @doc Add a new document to the notebook
	
new_document() -> 
	new_document(?DEFAULT_TAB_LABEL).
	
	
%% =====================================================================
%% @doc Add a new document to the notebook with filename

new_document(Filename) ->
	wx_object:call(?MODULE, {new_document, Filename, []}),
	ok.

	
%% =====================================================================
%% @doc Add an existing document to the notebook.
		
new_document_from_existing(Path, Contents) -> 
	new_document_from_existing(Path, Contents, []).


%% =====================================================================
%% @doc Add an existing document to the notebook.

new_document_from_existing(Path, Contents, Options) -> 
	Editor = wx_object:call(?MODULE, {new_document, filename:basename(Path), [{path, Path}] ++ Options}),
	editor:set_text(Editor, Contents),
	editor:empty_undo_buffer(Editor),
	editor:set_savepoint(Editor),
	editor:link_poller(Editor, Path),
  ok.


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
	close_document(Notebook, DocEts, Index).

close_document(Notebook, DocEts, Index) ->
 	Key = wxAuiNotebook:getPage(Notebook, Index),
	io:format("REF: ~p~n", [get_ref(DocEts, Key)]),
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
		false -> 
			ok,
			Record  = get_record(DocEts, index_to_key(Notebook, Index)),
			record_get_path(Record);
		_ ->
			save_document(Sb, get_record(DocEts, index_to_key(Notebook, Index)))
	end.
	
save_document(Sb, Record) ->
	Result  = case record_get_path(Record) of
		undefined -> save_new_document();
		Path -> %% Document already exists, overwrite
			ide_io:save(Path, editor:get_text(record_get_ref(Record))),
			editor:set_savepoint(record_get_ref(Record)),
			ide_status_bar:set_text_timeout(Sb, {field, help}, "Document saved."),
			Path
	end,
	Result.


%% =====================================================================
%% @doc

save_new_document() ->
	case get_active_document() of
		{error, no_open_editor} -> ok;
		Index ->
			save_new_document(Index)
	end.

save_new_document(Index) ->
	{Notebook,Sb,DocEts} = wx_object:call(?MODULE, notebook), 
	Contents = editor:get_text(index_to_ref(DocEts, Notebook, Index)),
	Result = case ide_io:save_as(Notebook, Contents) of
		{cancel} ->
			ide_status_bar:set_text_timeout(Sb, {field, help}, "Document not saved."),
			undefined;
		{ok, {Path, Filename}}  ->
			update_path(DocEts, index_to_key(Notebook, Index), Path), 
			wxAuiNotebook:setPageText(Notebook, Index, Filename),
			editor:set_savepoint(index_to_ref(DocEts, Notebook, Index)),
			editor:link_poller(index_to_ref(DocEts, Notebook, Index), Path),
			ide_status_bar:set_text_timeout(Sb, {field, help}, "Document saved."),
			Path
	end,
	Result.
	

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
			new_document_from_existing(Path, Contents)
	end.


%% =====================================================================
%% @doc Close all editor instances,
%% Note: must always delete documents starting with the highest index
%% first, as the notebook shifts all indexes dwon when one is deleted.

close_all_documents() ->
	wx_object:call(?MODULE, close_all).


%% =====================================================================
%% @doc Apply the function Fun to the active document.
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
%% @doc Close an open project.
%% This will close any files belonging to the project, and remove the
%% tree from the project tree. 

close_project() ->
	%% Check open files, save/close
	wx_object:call(?MODULE, close_project).
  
	
%% =====================================================================
%% @doc Apply the function Fun to all open documents.

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
%% Callback functions
%% =====================================================================

init(Config) ->
	{Parent, Sb} = proplists:get_value(config, Config),

	Style = (0
			bor ?wxAUI_NB_TOP
			bor ?wxAUI_NB_WINDOWLIST_BUTTON
			bor ?wxAUI_NB_TAB_MOVE
			bor ?wxAUI_NB_SCROLL_BUTTONS
			bor ?wxAUI_NB_CLOSE_ON_ALL_TABS
			bor ?wxAUI_NB_TAB_SPLIT
			bor ?wxBORDER_NONE
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
	{noreply, State}.

handle_call(notebook, _, State=#state{notebook=Nb, status_bar=Sb, document_ets=Tb}) ->
	{reply, {Nb,Sb,Tb}, State};
handle_call({new_document, Filename, Options}, _, 
						State=#state{notebook=Notebook, document_ets=DocEts, status_bar=Sb, sizer=Sz, parent=Parent}) ->
	Editor = new_document(Notebook, DocEts, Sb, Filename, Parent, Sz, Options),
	{reply, Editor, State};
% handle_call(close_project, _, 
% 						State=#state{active_project=Proj, notebook=Notebook, document_ets=DocEts}) ->
% 	%% Check open files, save/close
% 	case Proj of
% 		undefined -> ok;
% 		Project={Item,Root} ->
% 			wxAuiNotebook:disconnect(Notebook, command_auinotebook_page_changed),
% 			Pages = get_active_project_documents(Project, DocEts, Notebook),
% 			lists:foreach(fun(E) -> close_document(Notebook, DocEts, get_notebook_index(Notebook, E)) end, tl(Pages)),
% 			wxAuiNotebook:connect(Notebook, command_auinotebook_page_changed),
% 			close_document(Notebook, DocEts, get_notebook_index(Notebook, hd(Pages))),
% 			ide_projects_tree:delete_project(Item)
% 	end,
	% {reply, ok, State};
handle_call(close_all, _, State=#state{notebook=Notebook, document_ets=DocEts}) ->
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
%% =====================================================================

handle_event(#wx{obj=Notebook, event=#wxAuiNotebook{type=command_auinotebook_page_changed,
			selection=Index}}, State=#state{document_ets=DocEts, notebook=Nb, status_bar=Sb}) ->
	PageText = wxAuiNotebook:getPageText(Notebook, Index),
  % Make sure editor knows (needs to update sb)
  editor:selected(index_to_ref(DocEts, Notebook, Index), Sb),
	Proj = lookup_project(DocEts, wxAuiNotebook:getPage(Notebook, Index)),
	Str = case Proj of
		undefined -> 
			PageText;
		ProjectId -> 
			project_manager:set_active_project(ProjectId)
	end,	
  {noreply, State};
handle_event(#wx{event=#wxAuiNotebook{type=command_auinotebook_bg_dclick}}, 
						 State=#state{notebook=Nb, status_bar=Sb, document_ets=DocEts, parent=Parent, sizer=Sz}) ->
	new_document(Nb, DocEts, Sb, ?DEFAULT_TAB_LABEL, Parent, Sz, []),
  {noreply, State}.
    

%% =====================================================================
%% Internal functions
%% =====================================================================

insert_page(Notebook, Page) ->
	insert_page(Notebook, Page, ?DEFAULT_TAB_LABEL).
	
insert_page(Notebook, Page, Filename) ->
	wxAuiNotebook:addPage(Notebook, Page, Filename, [{select, true}]),
	wxAuiNotebook:getPage(Notebook, get_notebook_index(Notebook, Page)).
	
get_notebook_index(Notebook, Page) ->
	wxAuiNotebook:getPageIndex(Notebook, Page).
	
%% Functions for accessing/modifying the document Ets table.
%% Always use these so that we can change the tables structure 

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
	ets:lookup_element(DocEts, Key, 4).	
	
lookup_path(DocEts, Key) ->
	{path, Path} = ets:lookup_element(DocEts, Key, 3),
	Path.	
	
update_path(DocEts, Key, Path) ->
	ets:update_element(DocEts, Key, {3, {path, Path}}).


%% =====================================================================
%% @doc Insert a new record into the Ets table.
%% We need to keep track of all open documents and their associated 
%% properties. To do this we use an ETS table (irrelevant). 
%% We need a unique index which is accessible during a page_changed
%% event that acts as the key to the ETS table.
%% We cannot use the index from wxAuiNotebook, as these are dynamic
%% and we'd have to track them when tabs are removed/dragged.
%% The only other option is to use the wx_object() reference
%% returned from the call to editor:start(), and acknowledge the
%% fact that in future wxErlang releases, this representation may change.
%% The #wxRef{} returned from editor:start() is of the form:
%% {wx_ref,515,wxPanel,<0.110.0>}, so you might think we coud use
%% wx_object:get_pid/1, and use the pid as the index, but this gets
%% stripped from the tuple when added to the notebook, meaning subsequent
%% calls to wxAuiNotebook:getPage return a tuple of the form:
%% {wx_ref,515,wxWindow,[]}.
%% By having functions for inserting/retrieving/updating ETS records
%% we hide the implementation/format of a single record, which
%% means any future changes to the API can be easily adapted to.

insert_rec(DocEts, Editor, Path, ProjectId) ->
	Pid = wx_object:get_pid(Editor),
	{A,B,C,_} = Editor,
	Key = {A,B,C,[]},
	New = wx:typeCast(Key, wxWindow),
	ets:insert(DocEts, {New, Pid, {path, Path}, ProjectId}).

insert_rec(DocEts, Index, EditorRef, Path, ProjectId) ->
	ets:insert(DocEts, {Index, EditorRef, {path, Path}, ProjectId}).

%% =====================================================================
%% The following functions access individuals elements from a single
%% record. 
%% NOTE: Always use these functions to access a record's elements, as 
%% the representation is internal and can be changed without notice.
	
record_get_key({Key,_,_,_}) -> Key.
record_get_ref({_,Ref,_,_}) ->	Ref.
record_get_path({_,_,{path,Path},_}) -> Path.
record_get_project({_,_,_,ProjectId}) -> ProjectId.

	
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
%% @doc
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
	insert_rec(DocEts, Index, Editor, proplists:get_value(path, Options), proplists:get_value(project_id, Options)),
	Editor.
