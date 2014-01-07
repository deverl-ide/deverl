%% =====================================================================
%% @author
%% @copyright
%% @title
%% @version 0.15
%% @doc This module manages open documents.
%% @end
%% =====================================================================

-module(ide_doc_man_wx).

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
         close_project/1,
				 save_as/0,
				 save_all/0,
         save_document/1,
         save_active_document/0,
         save_active_project/0,
				 apply_to_all_documents/2,
				 apply_to_active_document/2,
         get_active_document/0,
         get_path/1,
         set_selection/1]).

%% Records
-record(document, {path :: string(),
                   ide_file_poll_gen :: ide_file_poll_gen:ide_file_poll_gen(),
                   editor :: ide_editor_wx:editor(),
                   project_id :: ide_proj_man:project_id()}).

%% Types
-type document_id() :: {integer(), integer(), integer()}.
-type document_record() :: {document_id(), #document{}}.

%% Server state
-record(state, {notebook :: wxAuiNotebook:wxAuiNotebook(), %% Notebook
                page_to_doc_id :: [{wxWindow:wxWindow(), document_id()}],
                doc_records :: [document_record()],
								sizer,
								parent
                }).


%% =====================================================================
%% Client API
%% =====================================================================

% -spec start([Config]) -> wx_object:wx_object() when
  

start(Config) ->
  wx_object:start_link({local, ?MODULE}, ?MODULE, Config, []).


%% =====================================================================
%% @doc Create a new file and insert it into the workspace.

new_document(Parent) ->
  OpenProjects = ide_proj_man:get_open_projects(),
  Dialog = ide_new_file_dlg_wx:start({Parent, OpenProjects, ide_proj_man:get_active_project()}),
  case wxDialog:showModal(Dialog) of
    ?wxID_CANCEL ->
      ok;
    ?wxID_OK ->
      create_document(ide_new_file_dlg_wx:get_path(Dialog), ide_new_file_dlg_wx:get_project_id(Dialog)),
      ide_new_file_dlg_wx:close(Dialog)
  end.


%% =====================================================================
%% @doc Insert a documents into the workspace.

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

close_project(ProjectId) ->
  close_documents(get_project_documents(ProjectId)).


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

-spec save_document(document_id()) -> 'ok' | 'cancelled'.

save_document(DocId) ->
  case save_documents([DocId]) of
    {_Saved, []} ->
      ok;
    {_Saved, _Failed} ->
      cancelled
  end.


%% =====================================================================
%% @doc

save_active_document() ->
	%% Saving unmodified documents unnecessarily atm
	save_documents([get_active_document()]).


%% =====================================================================
%% @doc

save_active_project() ->
	case save_project(ide_proj_man:get_active_project()) of
    {Saved, []} ->
      ok;
    {_Saved, _Failed} ->
      cancelled
  end.


%% =====================================================================
%% @doc Apply the function Fun to all open documents.

apply_to_all_documents(Fun, Args) ->
	apply_to_documents(Fun, Args, get_open_documents()).


%% =====================================================================
%% @doc

apply_to_active_document(Fun, Args) ->
	apply_to_documents(Fun, Args, [get_active_document()]).


%% =====================================================================
%% @doc

get_active_document() ->
  wx_object:call(?MODULE, get_active_doc).


%% =====================================================================
%% @doc

get_path(DocId) ->
  wx_object:call(?MODULE, {get_path, DocId}).


%% =====================================================================
%% @doc

set_selection(Direction) ->
  wx_object:cast(?MODULE, {set_sel, Direction}).


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

	Ph = ide_lib_widgets:placeholder(Panel, "No Open Documents"),
	wxSizer:add(Sz, Ph, [{flag, ?wxEXPAND}, {proportion, 1}]),

	wxAuiNotebook:connect(Notebook, command_auinotebook_page_close, [callback]),
	wxAuiNotebook:connect(Notebook, command_auinotebook_page_changed, []),
  
  State = #state{notebook=Notebook,
                 page_to_doc_id=[],
                 doc_records=[],
                 sizer=Sz,
                 parent=Parent},

  {Panel, State}.

handle_info(Msg, State) ->
	io:format("Got Info ~p~n",[Msg]),
	{noreply,State}.

handle_cast(freeze_notebook, State=#state{notebook=Nb, parent=Parent}) ->
  wxWindow:freeze(Parent),
	{noreply, State};
handle_cast(thaw_notebook, State=#state{notebook=Nb, parent=Parent}) ->
  wxWindow:thaw(Parent),
	{noreply, State};
handle_cast({set_sel, Direction}, State=#state{notebook=Nb, sizer=Sz}) ->
  Cur = wxAuiNotebook:getSelection(Nb),
  N = wxAuiNotebook:getPageCount(Nb),
  Idx = case Direction of
    left -> 
      (Cur - 1) rem N;
    right ->
      (Cur + 1) rem N
  end,
  wxAuiNotebook:setSelection(Nb, Idx),
  {noreply, State}.

handle_call({create_doc, Path, ProjectId}, _From,
						State=#state{notebook=Nb, sizer=Sz, doc_records=DocRecords, page_to_doc_id=PageToDocId}) ->
	case is_already_open(Path, DocRecords) of
		false ->
			ensure_notebook_visible(Nb, Sz),
      Font = ide_sys_pref_gen:get_font(editor),
		  Editor = ide_editor_wx:start([{parent, Nb}, {font, Font}]),
		  wxAuiNotebook:addPage(Nb, Editor, filename:basename(Path), [{select, true}]),
		  DocId = generate_id(),
		  Document = #document{path=Path, editor=Editor, project_id=ProjectId},
		  NewDocRecords = [{DocId, Document}|DocRecords],
		  Key = wxAuiNotebook:getPage(Nb, wxAuiNotebook:getPageCount(Nb)-1),
			load_editor_contents(Editor, Path),
      case ProjectId of
        undefined ->
          ide_proj_tree_wx:add_standalone_document(Path);
        _ ->
          ide_proj_tree_wx:set_has_children(filename:dirname(Path))
      end,
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
      case ide_editor_wx:is_dirty(Record#document.editor) of
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

handle_call({get_path, DocId}, _From,
				    State=#state{doc_records=DocRecords}) ->
  #document{path=Path} = proplists:get_value(DocId, DocRecords),
  {reply, Path, State};

handle_call({save, DocId}, _From,
				    State=#state{parent=Parent, doc_records=DocRecords}) ->
  Record=#document{path=Path} = proplists:get_value(DocId, DocRecords),
  Contents = ide_editor_wx:get_text(Record#document.editor),
  Result = try
    ide_io:save(Path, Contents),
    {ok, DocRecords}
  catch
    _:Msg ->
      {error, {Msg, Parent}}
  end,
  {reply, Result, State};

handle_call({save_as, DocId}, _From,
				    State=#state{notebook=Nb, doc_records=DocRecords, page_to_doc_id=PageToDocId}) ->
  Record = proplists:get_value(DocId, DocRecords),
	Editor = Record#document.editor,
	Contents = ide_editor_wx:get_text(Editor),
	{NewRecs, NewPage2Ids} = case ide_io:save_as(Nb, Contents) of
		{cancel} ->
			{DocRecords, PageToDocId};
		{ok, {Path, Filename}}  ->
			wxAuiNotebook:setPageText(Nb, wxAuiNotebook:getSelection(Nb), Filename),
			NewId = generate_id(),
			NewPageToDocId = proplists:delete(wxAuiNotebook:getPage(Nb, wxAuiNotebook:getSelection(Nb)), PageToDocId),
			NewDocRecords = proplists:delete(DocId, DocRecords),
			NewRecord = #document{path=Path, editor=Editor},
			ide_editor_wx:set_savepoint(Editor),
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
	{reply, ok, State};
  
handle_call({close_docs, Docs}, _From, State=#state{notebook=Nb, doc_records=DocRecords, page_to_doc_id=PageToDocId, sizer=Sz}) ->
  F = fun(G, [], Dr, P2d) -> {Dr, P2d};
         (G, [DocId | T], Dr, P2d) ->    
            Record = proplists:get_value(DocId, DocRecords),
            case Record#document.project_id of
              undefined ->
                ide_proj_tree_wx:remove_standalone_document(Record#document.path);
              _ ->
                ok
            end,
            {NewDocRecords, NewPageToDocId} = remove_document(Nb, DocId, doc_id_to_page_id(Nb, DocId, P2d), Dr, P2d),
            G(G, T, NewDocRecords, NewPageToDocId)
       end,
  {S, D} = F(F, Docs, DocRecords, PageToDocId),
  case wxAuiNotebook:getPageCount(Nb) of
    0 -> 
      %% Called when the last document is closed.
      ide:toggle_menu_group(?MENU_GROUP_NOTEBOOK_EMPTY, false),
      show_placeholder(Sz),
      ide:set_title([]);
    _ -> ok
  end,
	{reply, ok, State#state{doc_records=S, page_to_doc_id=D}}.

%% Close event
handle_sync_event(#wx{}, Event, #state{notebook=Nb, page_to_doc_id=PageToDoc}) ->
  wxNotifyEvent:veto(Event),
  DocId = page_id_to_doc_id(Nb, wxAuiNotebookEvent:getSelection(Event), PageToDoc),
  Env = wx:get_env(),
  spawn(fun() -> wx:set_env(Env), close_documents([DocId]) end),
	ok.

handle_event(#wx{event=#wxAuiNotebook{type=command_auinotebook_page_changed,
			selection=Index}}, State=#state{notebook=Nb}) ->
  {noreply, State}.

code_change(_, _, State) ->
	{ok, State}.

terminate(_Reason, #state{}) ->
	ok.


%% =====================================================================
%% Internal functions
%% =====================================================================

%% =====================================================================
%% @doc

load_editor_contents(Editor, Path) ->
	try
		ide_editor_wx:set_text(Editor, ide_io:read_file(Path)),
		ide_editor_wx:empty_undo_buffer(Editor),
		ide_editor_wx:set_savepoint(Editor)
		%ide_editor_wx:link_poller(Editor, Path)
	catch
		_Throw ->
			io:format("LOAD EDITOR ERROR~n")
	end.


%% =====================================================================
%% @doc

show_save_changes_dialog(Parent, ModifiedDocNames, ModifiedDocIdList, DocNames, DocIdList) ->
  Dialog = ide_lib_dlg_wx:save_changes_dialog(Parent, ModifiedDocNames),
  case wxDialog:showModal(Dialog) of
		?wxID_CANCEL -> %% Cancel close
			cancelled;
		?wxID_REVERT_TO_SAVED ->  %% Close without saving
			close(DocIdList);
		?wxID_SAVE -> %% Save the document
      close(lists:subtract(DocIdList, ModifiedDocIdList)),
      save_and_close(ModifiedDocIdList)
	end.
  

%% =====================================================================
%% @doc

save_as(DocId) ->
	wx_object:call(?MODULE, {save_as, DocId}).


%% =====================================================================
%% @doc

save_documents(DocIdList) ->
  save_documents(DocIdList, {[], []}).

save_documents([], Acc) ->
  Acc;
save_documents([DocId|DocIdList], {Saved, Failed}) ->
	case wx_object:call(?MODULE, {save, DocId}) of
    {ok, DocRecords} ->
      Record = get_record(DocId, DocRecords),
      ide_editor_wx:set_savepoint(Record#document.editor),
      save_documents(DocIdList, {[DocId|Saved], Failed});
    {error, {Msg, Parent}} ->
      ide_lib_dlg_wx:msg_error(Parent, Msg),
      save_documents(DocIdList, {Saved, [DocId|Failed]})
  end.


%% =====================================================================
%% @doc

save_and_close(DocIdList) ->
  case save_documents(DocIdList) of
    {Saved, []} ->
      close(Saved);
    {Saved, Failed} ->
      close(Saved),
      cancelled
  end.


%% =====================================================================
%% @doc

save_project(ProjectId) ->
	save_documents(get_project_documents(ProjectId)).
  
  
%% =====================================================================
%% @doc Should an io error occur, only those documents saved up to that
%% point will be saved.

close_documents(Documents) ->
  case get_modified_docs(Documents) of
    {[], _Parent} ->
      close(Documents);
    {ModifiedDocs, Parent} ->
      show_save_changes_dialog(Parent, get_doc_names(ModifiedDocs), ModifiedDocs, get_doc_names(Documents), Documents)
  end.
  

%% =====================================================================
%% @doc

close(Docs) ->
  wx_object:call(?MODULE, {close_docs, Docs}).


%% =====================================================================
%% @doc Takes a lists of document ids and filters out the unmodified
%% documents, returning a list of modified document id's.

-spec get_modified_docs([document_id()]) -> {[document_id()], Parent} when
  Parent :: wx_object:wx_object().

get_modified_docs(Documents) ->
  wx_object:call(?MODULE, {get_modified_docs, Documents}).


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
      %% enable menu items
      ide:toggle_menu_group(?MENU_GROUP_NOTEBOOK_EMPTY, true),
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
  ProjectId = case ide_proj_man:is_known_project(Path) of
    {true, ProjectPath} ->
      open_from_existing_project(ProjectPath);
    false ->
      ide_proj_tree_wx:add_standalone_document(Path),
      undefined
  end,
  wx_object:call(?MODULE, {create_doc, Path, ProjectId}).


%% =====================================================================
%% @doc

open_from_existing_project(ProjectPath) ->
  Result = case ide_proj_man:get_project(ProjectPath) of
    undefined ->
      ide_proj_man:open_project(ProjectPath);
    ProjectId ->
      ProjectId
  end.