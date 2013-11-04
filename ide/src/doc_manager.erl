%% =====================================================================
%% @author
%% @copyright
%% @title
%% @version
%% @doc This module manages the open documents.
%% @end
%% =====================================================================

-module(doc_manager).

-include_lib("wx/include/wx.hrl").
-include("ide.hrl").

%% wx_object
-behaviour(wx_object).
-export([init/1, terminate/2,  code_change/3,
         handle_info/2, handle_call/3, handle_cast/2, handle_event/2, handle_sync_event/3]).
				 
%% API
-export([start/1,
				 new_document/1,
				 create_document/2,
         close_all/0,
         close_active_document/0,
         close_active_project/0]).
    
%% Server state
-record(document, {path :: string(), 
                   file_poller :: file_poller:file_poller(), 
                   editor :: editor:editor(), 
                   project_id :: project_manager:project_id()}).
           
%% Types
-type document_id() :: {integer(), integer(), integer()}. 
-type document_record() :: {document_id(), #document{}}.


                  
%% Server state
-record(state, {
                notebook :: wxAuiNotebook:wxAuiNotebook(),    %% Notebook
                page_to_doc_id :: [{wxWindow:wxWindow(), document_id()}],
                doc_records :: [document_record()],
								sizer,
								parent
                }).


%% =====================================================================
%% Client API
%% =====================================================================

start(Config) ->
  wx_object:start_link({local, ?MODULE}, ?MODULE, Config, []).
  
  
new_document(Parent) ->
  OpenProjects = project_manager:get_open_projects(),
  Dialog = new_file:start({Parent, OpenProjects, project_manager:get_active_project()}),
  case wxDialog:showModal(Dialog) of
    ?wxID_CANCEL ->
      ok;
    ?wxID_OK ->
      create_document(new_file:get_path(Dialog), new_file:get_project_id(Dialog))
  end.  


close_all() ->
  close_documents(get_open_documents()).
  

close_active_document() ->
  close_documents([get_active_document()]).
  
  
close_active_project() ->
  close_project(project_manager:get_active_project()).
  

%% =====================================================================
%% Callbacks
%% =====================================================================

init(Config) ->
	Parent = proplists:get_value(parent, Config),

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
					
	Ph = lib_widgets:placeholder(Panel, "No Open Documents"),
	wxSizer:add(Sz, Ph, [{flag, ?wxEXPAND}, {proportion, 1}]),

	wxAuiNotebook:connect(Notebook, command_auinotebook_page_close, [callback]),
	wxAuiNotebook:connect(Notebook, command_auinotebook_page_changed),

  {Panel, #state{notebook=Notebook, page_to_doc_id=[], doc_records=[], sizer=Sz, parent=Parent}}.

handle_info(Msg, State) ->
	io:format("Got Info ~p~n",[Msg]),
	{noreply,State}.

handle_cast({close_doc, DocId}, State=#state{notebook=Nb, doc_records=DocRecords, page_to_doc_id=PageToDocId}) ->
  Record = get_record(DocId, DocRecords),
  {NewDocRecords, NewPageToDocId} = remove_document(Nb, DocId, doc_id_to_page_id(Nb, DocId, PageToDocId), DocRecords, PageToDocId),
  case wxAuiNotebook:getPageCount(Nb) of
 		0 -> wx_object:cast(?MODULE, notebook_empty);
 		_ -> ok 
 	end,
  {noreply, State#state{doc_records=NewDocRecords, page_to_doc_id=NewPageToDocId}};

handle_cast(notebook_empty, State=#state{sizer=Sz}) ->
	%% Called when the last document is closed.
	show_placeholder(Sz),
	ide:set_title([]),
	{noreply, State};
  
handle_cast(_, State) ->
	{noreply, State}.
  
handle_call({create_doc, Path, ProjectId}, _From, 
    State=#state{notebook=Nb, sizer=Sz, doc_records=DocRecords, page_to_doc_id=PageToDocId}) ->
	ensure_notebook_visible(Nb, Sz),
  Editor = editor:start([{parent, Nb}, {font, user_prefs:get_user_pref({pref, font})}]),
  Bool = wxAuiNotebook:addPage(Nb, Editor, filename:basename(Path)),
  DocId = generate_id(),
  Document = #document{path=Path, editor=Editor, project_id=ProjectId},
  NewDocRecords = [{DocId, Document}|DocRecords],
  Key = wxAuiNotebook:getPage(Nb, wxAuiNotebook:getPageCount(Nb)-1),
	load_editor_contents(Editor, Path),
  {reply, ok, State#state{doc_records=NewDocRecords, page_to_doc_id=[{Key, DocId}|PageToDocId]}};
  
handle_call(get_open_docs, _From, State=#state{notebook=Nb, doc_records=DocRecords}) ->
  {reply, lists:map(fun({DocId, _}) -> DocId end, DocRecords), State};
  
handle_call(get_active_doc, _From, 
    State=#state{notebook=Nb, doc_records=DocRecords, page_to_doc_id=PageToDocId}) ->
  DocId = proplists:get_value(wxAuiNotebook:getPage(Nb, wxAuiNotebook:getSelection(Nb)), PageToDocId),
  {reply, DocId, State};
  
handle_call({get_project_docs, ProjectId}, _From, 
    State=#state{notebook=Nb, doc_records=DocRecords, page_to_doc_id=PageToDocId}) ->
  DocList = lists:foldl(fun({DocId, #document{project_id=Project}}, Acc) when Project =:= ProjectId -> [DocId|Acc]; 
                           (_, Acc) -> Acc end, [], DocRecords),
  {reply, DocList, State};
  
handle_call({get_modified_docs, DocIdList}, _From, 
    State=#state{notebook=Nb, parent=Parent, doc_records=DocRecords, page_to_doc_id=PageToDocId}) ->
  List = lists:foldl(
    fun(DocId, Acc) -> 
      Record = get_record(DocId, DocRecords), 
      case editor:is_dirty(Record#document.editor) of
        true ->
          [DocId|Acc];
        _ ->
          Acc
      end
    end, [], DocIdList),
  {reply, {List, Parent}, State};
  
handle_call({get_doc_names, DocIdList}, _From, 
    State=#state{notebook=Nb, doc_records=DocRecords, page_to_doc_id=PageToDocId}) ->
  DocNameList = lists:map(
    fun(DocId) -> 
      Record = proplists:get_value(DocId, DocRecords),
      filename:basename(Record#document.path)
    end, DocIdList),
  {reply, DocNameList, State}.
  
handle_sync_event(#wx{}, Event, State=#state{notebook=Nb, page_to_doc_id=PageToDoc}) ->
  wxNotifyEvent:veto(Event),
  DocId = page_id_to_doc_id(Nb, wxAuiNotebookEvent:getSelection(Event), PageToDoc),
	close_documents([DocId]),
	ok.

handle_event(#wx{}, State) ->
  {noreply, State}.

code_change(_, _, State) ->
	{stop, ignore, State}.

terminate(_Reason, State=#state{notebook=Nb}) ->
	ok.
         


%% =====================================================================
%% Internal functions
%% =====================================================================

create_document(Path, ProjectId) ->
  case ide_io:create_new_file(Path) of
    error ->
      % file not created dialog
      error;
    ok ->
      wx_object:call(?MODULE, {create_doc, Path, ProjectId})
  end.
  
  
load_editor_contents(Editor, Path) ->
	try 
		editor:set_text(Editor, ide_io:read_file(Path)),
		editor:empty_undo_buffer(Editor),
		editor:set_savepoint(Editor)
		%editor:link_poller(Editor, Path)
	catch
		Throw ->
			io:format("LOAD EDITOR ERROR~n")
	end.

  
close_documents(Documents) ->
  case get_modified_docs(Documents) of
    {[], _Parent} ->
      close(Documents),
      ok;
    {ModifiedDocs, Parent} ->
      show_save_changes_dialog(Parent, get_doc_names(ModifiedDocs), ModifiedDocs)
  end.
  
  
show_save_changes_dialog(Parent, DocNames, DocIdList) ->
  Dialog = lib_dialog_wx:save_changes_dialog(Parent, DocNames),
  case wxDialog:showModal(Dialog) of
		?wxID_CANCEL -> %% Cancel close
			cancelled;
		?wxID_REVERT_TO_SAVED ->  %% Close without saving
			close(DocIdList);
		?wxID_SAVE -> %% Save the document
      %% needs to save here
			close(DocIdList)
	end.
  
  
close([]) ->
  ok;
close([DocId|Documents]) ->
  wx_object:cast(?MODULE, {close_doc, DocId}),
  close(Documents).
  
  
%% Documents :: list of DocId
get_modified_docs(Documents) ->
  wx_object:call(?MODULE, {get_modified_docs, Documents}).
  
  
close_project(ProjectId) ->
  case close_documents(get_project_documents(ProjectId)) of
    ok ->
      ide_projects_tree:delete_project(ProjectId);
    cancelled ->
      ok
  end. 
    
    
%% Remove document records from state and delete page from auinotebook
remove_document(Nb, DocId, PageId, DocRecords, PageToDocId) ->
  NewDocRecords = proplists:delete(DocId, DocRecords),
  NewPageToDocId = proplists:delete(PageId, PageToDocId),
  wxAuiNotebook:deletePage(Nb, PageId),
  {NewDocRecords, NewPageToDocId}.
  

page_id_to_doc_id(Notebook, PageId, PageToDocId) ->
  Page = wxAuiNotebook:getPage(Notebook, PageId),
  proplists:get_value(Page, PageToDocId). 


doc_id_to_page_id(Notebook, DocId, []) ->
  error("No Corresponding Page ID~n");
doc_id_to_page_id(Notebook, DocId, [{Page, DocId}|Rest]) ->
  wxAuiNotebook:getPageIndex(Notebook, Page);
doc_id_to_page_id(Notebook, DocId, [{Page, _}|Rest]) ->
  doc_id_to_page_id(Notebook, DocId, Rest).


get_open_documents() ->
  wx_object:call(?MODULE, get_open_docs).
  
get_active_document() ->
  wx_object:call(?MODULE, get_active_doc).

get_project_documents(ProjectId) ->
  wx_object:call(?MODULE, {get_project_docs, ProjectId}).
  
get_doc_names(DocIdList) ->
  wx_object:call(?MODULE, {get_doc_names, DocIdList}).

%% =====================================================================
%% @doc

get_record(DocId, DocRecords) ->
  proplists:get_value(DocId, DocRecords).


%% =====================================================================
%% @doc Generate a unique document id.
  
generate_id() ->
	now().
	
  
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
%% @doc Ensure the notebook is visible (placeholder hidden).
 
ensure_notebook_visible(Notebook, Sz) ->
	case wxWindow:isShown(Notebook) of
		false -> 
			show_notebook(Sz);
		true -> ok
	end.
