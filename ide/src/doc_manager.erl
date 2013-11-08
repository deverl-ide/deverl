%% =====================================================================
%% @author
%% @copyright
%% @title
%% @version 0.15
%% @doc This module manages open documents.
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
         open_document_dialog/1,
         close_all/0,
         close_active_document/0,
         close_active_project/0,
				 save_as/0,
				 save_all/0,
         save_active_document/0,
         save_active_project/0,
				 apply_to_all_documents/2,
				 apply_to_active_document/2]).

%% Records
-record(document, {path :: string(),
                   file_poller :: file_poller:file_poller(),
                   editor :: editor:editor(),
                   project_id :: project_manager:project_id()}).

%% Types
-type document_id() :: {integer(), integer(), integer()}.
-type document_record() :: {document_id(), #document{}}.

%% Server state
-record(state, {
                notebook :: wxAuiNotebook:wxAuiNotebook(), %% Notebook
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


%% =====================================================================
%% @doc

new_document(Parent) ->
  OpenProjects = project_manager:get_open_projects(),
  Dialog = new_file:start({Parent, OpenProjects, project_manager:get_active_project()}),
  case wxDialog:showModal(Dialog) of
    ?wxID_CANCEL ->
      ok;
    ?wxID_OK ->
      create_document(new_file:get_path(Dialog), new_file:get_project_id(Dialog))
  end.


%% =====================================================================
%% @doc

create_document(Path, ProjectId) ->
  case ide_io:create_new_file(Path) of
    error ->
      % file not created dialog
      error;
    ok ->
      wx_object:call(?MODULE, {create_doc, Path, ProjectId})
  end.


%% =====================================================================
%% @doc

open_document_dialog(Frame) ->
  case ide_io:open_new(Frame) of
		cancel ->
			ok;
		Path ->
      io:format("EXISTING PROJECTS ~p~n", [sys_pref_manager:get_preference(projects)]),
      open_document(Path)
	end.


%% =====================================================================
%% @doc

close_all() ->
  close_documents(get_open_documents()).


%% =====================================================================
%% @doc

close_active_document() ->
  close_documents([get_active_document()]).


%% =====================================================================
%% @doc

close_active_project() ->
  case project_manager:get_active_project() of
    undefined ->
      ok;
    ActiveProject ->
      close_project(ActiveProject)
  end.


%% =====================================================================
%% @doc

save_as() ->
	save_as(get_active_document()).


%% =====================================================================
%% @doc

save_all() ->
	save_documents(get_open_documents()).


%% =====================================================================
%% @doc

save_active_document() ->
	%% Saving unmodified documents unnecessarily atm
	save_documents([get_active_document()]).


%% =====================================================================
%% @doc

save_active_project() ->
	save_project(project_manager:get_active_project()).


%% =====================================================================
%% @doc Apply the function Fun to all open documents.

apply_to_all_documents(Fun, Args) ->
	apply_to_documents(Fun, Args, get_open_documents()).


%% =====================================================================
%% @doc

apply_to_active_document(Fun, Args) ->
	apply_to_documents(Fun, Args, [get_active_document()]).


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
	wxAuiNotebook:connect(Notebook, command_auinotebook_page_changed, []),

  {Panel, #state{notebook=Notebook, page_to_doc_id=[], doc_records=[], sizer=Sz, parent=Parent}}.

handle_info(Msg, State) ->
	io:format("Got Info ~p~n",[Msg]),
	{noreply,State}.

handle_cast({close_doc, DocId}, State=#state{notebook=Nb, doc_records=DocRecords, page_to_doc_id=PageToDocId}) ->
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
	case is_already_open(Path, DocRecords) of
		false ->
			ensure_notebook_visible(Nb, Sz),
			Font = wxFont:new(sys_pref_manager:get_preference(editor_font_size),
												sys_pref_manager:get_preference(editor_font_family),
												sys_pref_manager:get_preference(editor_font_style),
												sys_pref_manager:get_preference(editor_font_weight), []),
		  Editor = editor:start([{parent, Nb}, {font, Font}]),
		  wxAuiNotebook:addPage(Nb, Editor, filename:basename(Path), [{select, true}]),
		  DocId = generate_id(),
		  Document = #document{path=Path, editor=Editor, project_id=ProjectId},
		  NewDocRecords = [{DocId, Document}|DocRecords],
		  Key = wxAuiNotebook:getPage(Nb, wxAuiNotebook:getPageCount(Nb)-1),
			load_editor_contents(Editor, Path),
			{reply, ok, State#state{doc_records=NewDocRecords, page_to_doc_id=[{Key, DocId}|PageToDocId]}};
		DocId ->
			wxAuiNotebook:setSelection(Nb, doc_id_to_page_id(Nb, DocId, PageToDocId)),
			{reply, ok, State}
	end;

handle_call(get_open_docs, _From, State=#state{doc_records=DocRecords}) ->
  {reply, lists:map(fun({DocId, _}) -> DocId end, DocRecords), State};

handle_call(get_active_doc, _From,
				    State=#state{notebook=Nb, page_to_doc_id=PageToDocId}) ->
  DocId = proplists:get_value(wxAuiNotebook:getPage(Nb, wxAuiNotebook:getSelection(Nb)), PageToDocId),
  {reply, DocId, State};

handle_call({get_project_docs, ProjectId}, _From,
				    State=#state{doc_records=DocRecords}) ->
  DocList = lists:foldl(
		fun({DocId, #document{project_id=Project}}, Acc) when Project =:= ProjectId ->
			[DocId|Acc];
    (_, Acc) ->
			Acc
		end, [], DocRecords),
  {reply, DocList, State};

handle_call({get_modified_docs, DocIdList}, _From,
				    State=#state{parent=Parent, doc_records=DocRecords}) ->
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
				    State=#state{doc_records=DocRecords}) ->
  DocNameList = lists:map(
    fun(DocId) ->
      Record = proplists:get_value(DocId, DocRecords),
      filename:basename(Record#document.path)
    end, DocIdList),
  {reply, DocNameList, State};

handle_call({save, DocId}, _From,
				    State=#state{parent=Parent, doc_records=DocRecords}) ->
  Record = proplists:get_value(DocId, DocRecords),
  Path = Record#document.path,
  Contents = editor:get_text(Record#document.editor),
  Result = try
    ide_io:save(Path, Contents),
    {ok, DocRecords}
  catch
    _:Msg ->
      {Msg, Parent}
  end,
{reply, Result, State};

handle_call({save_as, DocId}, _From,
				    State=#state{notebook=Nb, doc_records=DocRecords, page_to_doc_id=PageToDocId}) ->
  Record = proplists:get_value(DocId, DocRecords),
	Editor = Record#document.editor,
	Contents = editor:get_text(Editor),
	{NewRecs, NewPage2Ids} = case ide_io:save_as(Nb, Contents) of
		{cancel} ->
			{DocRecords, PageToDocId};
		{ok, {Path, Filename}}  ->
			wxAuiNotebook:setPageText(Nb, wxAuiNotebook:getSelection(Nb), Filename),
			NewId = generate_id(),
			NewPageToDocId = proplists:delete(wxAuiNotebook:getPage(Nb, wxAuiNotebook:getSelection(Nb)), PageToDocId),
			NewDocRecords = proplists:delete(DocId, DocRecords),
			NewRecord = #document{path=Path, editor=Editor},
			editor:set_savepoint(Editor),
			{[{NewId, NewRecord} | NewDocRecords], [{wxAuiNotebook:getPage(Nb, wxAuiNotebook:getSelection(Nb)), NewId} | NewPageToDocId]}
	end,
	{reply, ok, State#state{doc_records=NewRecs, page_to_doc_id=NewPage2Ids}};

handle_call({apply_to_docs, {Fun, Args, DocIds}}, _From, State=#state{doc_records=DocRecords}) ->
	Fun2 = fun(Editor) -> apply(Fun, [Editor | Args]) end,
	List = lists:map(
		fun(DocId) ->
			Record = proplists:get_value(DocId, DocRecords),
			Record#document.editor
		end, DocIds),
	lists:foreach(Fun2, List),
	{reply, ok, State}.

handle_sync_event(#wx{}, Event, #state{notebook=Nb, page_to_doc_id=PageToDoc}) ->
  wxNotifyEvent:veto(Event),
  DocId = page_id_to_doc_id(Nb, wxAuiNotebookEvent:getSelection(Event), PageToDoc),
	close_documents([DocId]),
	ok.

handle_event(#wx{event=#wxAuiNotebook{type=command_auinotebook_page_changed,
			selection=Index}}, State=#state{notebook=Nb}) ->
  io:format("PAGE CHANGED~n"),
  {noreply, State}.

code_change(_, _, State) ->
	{stop, ignore, State}.

terminate(_Reason, #state{}) ->
	ok.


%% =====================================================================
%% Internal functions
%% =====================================================================

%% =====================================================================
%% @doc

load_editor_contents(Editor, Path) ->
	try
		editor:set_text(Editor, ide_io:read_file(Path)),
		editor:empty_undo_buffer(Editor),
		editor:set_savepoint(Editor)
		%editor:link_poller(Editor, Path)
	catch
		_Throw ->
			io:format("LOAD EDITOR ERROR~n")
	end.


%% =====================================================================
%% @doc

close_documents(Documents) ->
  case get_modified_docs(Documents) of
    {[], _Parent} ->
      close(Documents),
      ok;
    {ModifiedDocs, Parent} ->
      show_save_changes_dialog(Parent, get_doc_names(ModifiedDocs), ModifiedDocs),
			close(lists:subtract(Documents, ModifiedDocs))
  end.


%% =====================================================================
%% @doc

show_save_changes_dialog(Parent, DocNames, DocIdList) ->
  Dialog = lib_dialog_wx:save_changes_dialog(Parent, DocNames),
  case wxDialog:showModal(Dialog) of
		?wxID_CANCEL -> %% Cancel close
			cancelled;
		?wxID_REVERT_TO_SAVED ->  %% Close without saving
			close(DocIdList);
		?wxID_SAVE -> %% Save the document
      do_save(DocIdList)
	end.


%% =====================================================================
%% @doc

do_save(DocIdList) ->
  case save_documents(DocIdList) of
    [] ->
			ok;
    DocsToClose ->
      close(DocsToClose)
  end.


%% =====================================================================
%% @doc

close([]) ->
  ok;
close([DocId|Documents]) ->
  wx_object:cast(?MODULE, {close_doc, DocId}),
  close(Documents).


%% =====================================================================
%% @doc Takes a lists of document ids and filters out the unmodified
%% documents, returning a list of modified document id's.

-spec get_modified_docs([document_id()]) -> [document_id()].

get_modified_docs(Documents) ->
  wx_object:call(?MODULE, {get_modified_docs, Documents}).


%% =====================================================================
%% @doc

close_project(ProjectId) ->
  case close_documents(get_project_documents(ProjectId)) of
    ok ->
      ide_projects_tree:remove_project(ProjectId);
    cancelled ->
      ok
  end.


%% =====================================================================
%% @doc

save_as(DocId) ->
	wx_object:call(?MODULE, {save_as, DocId}).


%% =====================================================================
%% @doc

save_documents(DocIdList) ->
  save_documents(DocIdList, []).

save_documents([], Acc) ->
  Acc;
save_documents([DocId|DocIdList], Acc) ->
	Result = case wx_object:call(?MODULE, {save, DocId}) of
    {ok, DocRecords} ->
      Record = get_record(DocId, DocRecords),
      editor:set_savepoint(Record#document.editor),
      [DocId|Acc];
    {Msg, Parent} ->
      lib_dialog_wx:msg_error(Parent, Msg),
      Acc
  end,
  save_documents(DocIdList, Result).


%% =====================================================================
%% @doc

save_project(ProjectId) ->
	save_documents(get_project_documents(ProjectId)).


%% =====================================================================
%% @doc Remove document records from state and delete page from the
%% notebook.

remove_document(Nb, DocId, PageId, DocRecords, PageToDocId) ->
  NewDocRecords = proplists:delete(DocId, DocRecords),
  NewPageToDocId = proplists:delete(PageId, PageToDocId),
  wxAuiNotebook:deletePage(Nb, PageId),
  {NewDocRecords, NewPageToDocId}.


%% =====================================================================
%% @doc

page_id_to_doc_id(Notebook, PageId, PageToDocId) ->
  Page = wxAuiNotebook:getPage(Notebook, PageId),
  proplists:get_value(Page, PageToDocId).


%% =====================================================================
%% @doc

doc_id_to_page_id(_Nb, _DocId, []) ->
  error("No Corresponding Page ID~n");
doc_id_to_page_id(Notebook, DocId, [{Page, DocId}|_]) ->
  wxAuiNotebook:getPageIndex(Notebook, Page);
doc_id_to_page_id(Notebook, DocId, [_|Rest]) ->
  doc_id_to_page_id(Notebook, DocId, Rest).


%% =====================================================================
%% @doc

get_open_documents() ->
  wx_object:call(?MODULE, get_open_docs).


%% =====================================================================
%% @doc

get_active_document() ->
  wx_object:call(?MODULE, get_active_doc).


%% =====================================================================
%% @doc

get_project_documents(ProjectId) ->
  wx_object:call(?MODULE, {get_project_docs, ProjectId}).


%% =====================================================================
%% @doc

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


%% =====================================================================
%% @doc Check whether a file is already open.
%% @private

is_already_open(_, []) ->
	false;
is_already_open(Path, [{DocId, #document{path=Path}} | _]) ->
	DocId;
is_already_open(Path, [_ | T]) ->
	is_already_open(Path, T).


%% =====================================================================
%% @doc

apply_to_documents(Fun, Args, Docs) ->
	wx_object:call(?MODULE, {apply_to_docs, {Fun, Args, Docs}}).


%% =====================================================================
%% @doc

open_document(Path) ->
  ProjectId = case project_manager:is_known_project(Path) of
    {true, ProjectPath} ->
      open_from_existing_project(ProjectPath);
    false ->
      ide_projects_tree:add_standalone_document(Path),
      undefined
  end,
  wx_object:call(?MODULE, {create_doc, Path, ProjectId}).


%% =====================================================================
%% @doc

open_from_existing_project(ProjectPath) ->
  Result = case project_manager:get_project(ProjectPath) of
    undefined ->
      project_manager:open_project(ProjectPath);
    ProjectId ->
      ProjectId
  end.





