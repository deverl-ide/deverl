-module(doc_manager).

-include_lib("wx/include/wx.hrl").
-include("ide.hrl").

%% wx_object
-behaviour(wx_object).
-export([init/1, terminate/2,  code_change/3,
         handle_info/2, handle_call/3, handle_cast/2, handle_event/2, handle_sync_event/3]).
         
-record(document, {path :: string(), 
                   file_poller :: file_poller:file_poller(), 
                   editor :: editor:editor(), 
                   project_id :: project_manager:project_id()}).
                   
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
      create_document(new_file:get_path(Dialog), new_file:get_project(Dialog))
  end.  


%% =====================================================================
%% Callbacks
%% =====================================================================

init(Config) ->
  ok.

handle_info(Msg, State) ->
	io:format("Got Info ~p~n",[Msg]),
	{noreply,State}.

handle_cast(_, State) ->
	{noreply, State}.
  
handle_call({create_doc, Path, ProjectId}, _From, 
    State=#state{notebook=Nb, doc_records=DocRecords, page_to_doc_id=PageToDocId}) ->
  Editor = editor:start([{parent, Nb}, {font, user_prefs:get_user_pref({pref, font})}]),
  wxAuiNotebook:addPage(Nb, Editor, filename:basename(Path)),
  DocId = generate_id(),
  Document = #document{path=Path, editor=Editor, project_id=ProjectId},
  NewDocRecords = [{DocId, Document}|DocRecords],
  Key = wxAuiNotebook:getPage(Nb, wxAuiNotebook:getPageCount(Nb)),
  {reply, ok, State#state{doc_records=NewDocRecords, page_to_doc_id=[{Key, DocId}|PageToDocId]}}.

handle_sync_event(#wx{}, Event, State=#state{notebook=Nb}) ->
	wxNotifyEvent:veto(Event),
	close_document(wxAuiNotebook:getSelection(Nb)),
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
  

close_document(DocId) ->
    ok.

  
  
generate_id() ->
	now().
  
  
  
  
  
