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
         get_active_module/0,
         get_path/1,
         set_selection/1,
         get_standalone_src_files/0
         ]).

%% Records
-record(document, {path :: string(),
                   ide_file_poll_gen :: ide_file_poll_gen:ide_file_poll_gen(),
                   editor :: ide_editor_wx:editor(),
                   project_id :: project_id()}).

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

-spec start([Config]) -> wx_object:wx_object() when
  Config :: [{parent, wxWindow:wxWindow()}].

start(Config) ->
  wx_object:start_link({local, ?MODULE}, ?MODULE, Config, []).


%% =====================================================================
%% @doc Create a new file and insert it into the workspace.

-spec new_document(wxFrame:wxFrame()) -> ok.

new_document(Parent) ->
  OpenProjects = ide_proj_man:get_open_projects(),
  Dlg = ide_dlg_new_file_wx:start({Parent, OpenProjects, ide_proj_man:get_active_project()}),
  case wxDialog:showModal(Dlg) of
    ?wxID_CANCEL ->
      ok;
    ?wxID_OK ->
      create_document(ide_dlg_new_file_wx:get_path(Dlg), 
                      ide_dlg_new_file_wx:get_project_id(Dlg),
                      ide_dlg_new_file_wx:get_type(Dlg))
  end,
  %ide_dlg_new_file_wx:destroy(Dlg).
  wxDialog:destroy(Dlg).

%% =====================================================================
%% @doc Insert a document into the workspace.

-spec create_document(string(), project_id()) -> ok | error.

create_document(Path, ProjectId) ->
  create_document(Path, ProjectId, undefined).

-spec create_document(string(), project_id(), atom()) -> ok | error.
 
create_document(Path, ProjectId, Type) ->
  case ide_io:create_new_file(Path, [{template, Type}]) of
    error ->
      % file not created dialog
      error;
    ok ->
      wx_object:call(?MODULE, {create_doc, Path, ProjectId})
  end.


%% =====================================================================
%% @doc

-spec open_document_dialog(wxFrame:wxFrame()) -> ok.

open_document_dialog(Frame) ->
  case ide_io:open_new(Frame) of
		cancel ->
			ok;
		Path ->
      open_document(Path)
	end.


%% =====================================================================
%% @doc

-spec close_all() -> ok | cancelled.

close_all() ->
  close_documents(get_open_documents()).


%% =====================================================================
%% @doc

-spec close_active_document() -> ok | cancelled.

close_active_document() ->
  close_documents([get_active_document()]).


%% =====================================================================
%% @doc

-spec close_project(project_id()) -> ok | cancelled.

close_project(ProjectId) ->
  close_documents(get_projects_open_docs(ProjectId)).


%% =====================================================================
%% @doc

-spec save_as() -> ok.

save_as() ->
	save_as(get_active_document()).


%% =====================================================================
%% @doc

-spec save_all() -> {Saved, Failed} when
  Saved :: [document_id()],
  Failed :: [document_id()].

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

-spec save_active_document() -> {Saved, Failed} when
  Saved :: [document_id()],
  Failed :: [document_id()].

save_active_document() ->
	%% Saving unmodified documents unnecessarily atm
	save_documents([get_active_document()]).


%% =====================================================================
%% @doc

-spec save_active_project() -> ok | cancelled.

save_active_project() ->
	case save_project(ide_proj_man:get_active_project()) of
    {_Saved, []} ->
      ok;
    {_Saved, _Failed} ->
      cancelled
  end.


%% =====================================================================
%% @doc Apply the function Fun to all open documents.

-spec apply_to_all_documents(function(), list()) -> ok.

apply_to_all_documents(Fun, Args) ->
	apply_to_documents(Fun, Args, get_open_documents()).


%% =====================================================================
%% @doc

-spec apply_to_active_document(function(), list()) -> ok.

apply_to_active_document(Fun, Args) ->
	apply_to_documents(Fun, Args, [get_active_document()]).


%% =====================================================================
%% @doc

-spec get_active_document() -> document_id().

get_active_document() ->
  wx_object:call(?MODULE, get_active_doc).


%% =====================================================================
%% @doc

-spec get_active_module() -> atom().

get_active_module() ->
  DocID = wx_object:call(?MODULE, get_active_doc),
  list_to_atom(filename:basename(ide_doc_man_wx:get_path(DocID), ".erl")).
  
  
%% =====================================================================
%% @doc

-spec get_path(document_id()) -> string().

get_path(DocId) ->
  wx_object:call(?MODULE, {get_path, DocId}).


%% =====================================================================
%% @doc

-spec set_selection(atom()) -> ok.

set_selection(Direction) ->
  wx_object:cast(?MODULE, {set_sel, Direction}).


%% =====================================================================
%% @doc Get the src files for all standalone files.

get_standalone_src_files() ->
  wx_object:call(?MODULE, stdln_src).


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

handle_cast({set_sel, Direction}, State=#state{notebook=Nb}) ->
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
      Font = ide_sys_pref_gen:get_font(editor_font),
		  Editor = ide_editor_wx:start([{parent, Nb}, {font, Font}]),
		  wxAuiNotebook:addPage(Nb, Editor, filename:basename(Path), [{select, true}]),
		  DocId = generate_id(),
		  Document = #document{path=Path, editor=Editor, project_id=ProjectId},
		  NewDocRecords = [{DocId, Document}|DocRecords],
		  Key = wxAuiNotebook:getPage(Nb, wxAuiNotebook:getPageCount(Nb)-1),
			load_editor_contents(Editor, Path),
      case ProjectId of
        undefined ->
          code:add_path(filename:dirname(Path)),
          ide_proj_tree_wx:add_standalone_document(Path);
        _ ->
          ok
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
  
handle_call(stdln_src, _From, State=#state{doc_records=DocRecords}) -> 
  SrcFiles = lists:foldl(
      fun
      ({_DocId, Doc=#document{project_id=undefined}}, Acc) ->
        [Doc#document.path | Acc];
      (_, Acc) ->
        Acc
      end, [], DocRecords),
  {reply, SrcFiles, State};

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
  F = fun(_G, [], Dr, P2d) -> {Dr, P2d};
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
      %% Called when the last document is closed
      ide_testpane:clear(),
      ide:toggle_menu_group([?MENU_GROUP_NOTEBOOK_EMPTY], false),
      show_placeholder(Sz),
      ide:set_title([]);
    _ -> ok
  end,
  {reply, ok, State#state{doc_records=S, page_to_doc_id=D}}.

%% Close event
handle_sync_event(#wx{}, Event, #state{notebook=Nb, page_to_doc_id=PageToDoc}) ->
  wxNotifyEvent:veto(Event),
  DocId = page_idx_to_doc_id(Nb, wxAuiNotebookEvent:getSelection(Event), PageToDoc),
  Env = wx:get_env(),
  spawn(fun() -> wx:set_env(Env), close_documents([DocId]) end),
  wxEvent:skip(Event),
	ok.

handle_event(#wx{event=#wxAuiNotebook{type=command_auinotebook_page_changed, selection=Idx}},
             State=#state{notebook=Nb, page_to_doc_id=PageToDoc, doc_records=DocRecords}) ->
  case wxAuiNotebook:getPageCount(Nb) of
    0 -> ok;
    _ ->
      DocId = page_idx_to_doc_id(Nb, Idx, PageToDoc),
      DocRec = get_record(DocId, DocRecords),
      ide_proj_man:set_active_project(DocRec#document.project_id),
      Ebin = list_to_atom(filename:basename(filename:rootname(wxAuiNotebook:getPageText(Nb, Idx)))),
      ide_testpane:add_module_tests(Ebin)
  end,
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

-spec load_editor_contents(ide_editor_wx:editor(), string()) -> ok | error.

load_editor_contents(Editor, Path) ->
	try
		ide_editor_wx:set_text(Editor, ide_io:read_file(Path)),
		ide_editor_wx:empty_undo_buffer(Editor),
		ide_editor_wx:set_savepoint(Editor)
		%ide_editor_wx:link_poller(Editor, Path)
	catch
		_Throw ->
			io:format("LOAD EDITOR ERROR~n"),
      error
	end.


%% =====================================================================
%% @doc

-spec show_save_changes_dialog(wxWindow:wxWindow(), [string()], [document_id()], [document_id()]) ->
  cancelled | ok.

show_save_changes_dialog(Parent, ModifiedDocNames, ModifiedDocIdList, DocIdList) ->
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

-spec save_as(document_id()) -> ok.

save_as(DocId) ->
	wx_object:call(?MODULE, {save_as, DocId}).


%% =====================================================================
%% @doc

-spec save_documents([document_id()]) -> {Saved, Failed} when
  Saved :: [document_id()],
  Failed :: [document_id()].

save_documents(DocIdList) ->
  save_documents(DocIdList, {[], []}).


-spec save_documents([document_id()], {Saved, Failed}) -> {Saved, Failed} when
  Saved :: [document_id()],
  Failed :: [document_id()].

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

-spec save_and_close([document_id()]) -> ok | cancelled.

save_and_close(DocIdList) ->
  case save_documents(DocIdList) of
    {Saved, []} ->
      close(Saved);
    {Saved, _Failed} ->
      close(Saved),
      cancelled
  end.


%% =====================================================================
%% @doc

-spec save_project(project_id()) -> {Saved, Failed} when
  Saved :: [document_id()],
  Failed :: [document_id()].

save_project(ProjectId) ->
	save_documents(get_projects_open_docs(ProjectId)).


%% =====================================================================
%% @doc Should an io error occur, only those documents saved up to that
%% point will be saved.

-spec close_documents([document_id()]) -> ok | cancelled.

close_documents(Documents) ->
  case get_modified_docs(Documents) of
    {[], _Parent} ->
      close(Documents);
    {ModifiedDocs, Parent} ->
      show_save_changes_dialog(Parent, get_doc_names(ModifiedDocs), ModifiedDocs, Documents)
  end.


%% =====================================================================
%% @doc

-spec close([document_id()]) -> ok.

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

-spec remove_document(wxAuiNotebook:wxAuiNotebook(),
                      document_id(),
                      integer(),
                      [document_record()],
                      [{wxWindow:wxWindow(), document_id()}]) ->
  {[document_record()], [{wxWindow:wxWindow(), document_id()}]}.

remove_document(Nb, DocId, PageIdx, DocRecords, PageToDocId) ->

  %% Grab the stc, so we can make sure it's deleted
  % Rec = get_record(DocId, DocRecords),
  % Ed = Rec#document.editor,
  % 
  % ide_editor_wx:destroy(Ed),
  % wxAuiNotebook:removePage(Nb, PageIdx), %% These 2 lines =/= to deletePage/2
  
  NewDocRecords = proplists:delete(DocId, DocRecords),
  NewPageToDocId = proplists:delete(PageIdx, PageToDocId),
  
  wxAuiNotebook:deletePage(Nb, PageIdx), %% SEG FAULT OSX, wx3.0 see ticket #81

  {NewDocRecords, NewPageToDocId}.


%% =====================================================================
%% @doc

-spec page_idx_to_doc_id(wxAuiNotebook:wxAuiNotebook(),
                         integer(),
                         [{wxWindow:wxWindow(), document_id()}]) ->
  document_id().

page_idx_to_doc_id(Notebook, PageIdx, PageToDocId) ->
  Page = wxAuiNotebook:getPage(Notebook, PageIdx),
  proplists:get_value(Page, PageToDocId).


%% =====================================================================
%% @doc

-spec doc_id_to_page_id(wxAuiNotebook:wxAuiNotebook(),
                        document_id(),
                        [{wxWindow:wxWindow(), document_id()}]) ->
  integer().

doc_id_to_page_id(_Nb, _DocId, []) ->
  error("No Corresponding Page ID~n");
doc_id_to_page_id(Notebook, DocId, [{Page, DocId}|_]) ->
  wxAuiNotebook:getPageIndex(Notebook, Page);
doc_id_to_page_id(Notebook, DocId, [_|Rest]) ->
  doc_id_to_page_id(Notebook, DocId, Rest).


%% =====================================================================
%% @doc

-spec get_open_documents() -> [document_id()].

get_open_documents() ->
  wx_object:call(?MODULE, get_open_docs).


%% =====================================================================
%% @doc

-spec get_projects_open_docs(project_id()) -> [document_id()].

get_projects_open_docs(ProjectId) ->
  wx_object:call(?MODULE, {get_project_docs, ProjectId}).


%% =====================================================================
%% @doc

-spec get_doc_names([document_id()]) -> [string()].

get_doc_names(DocIdList) ->
  wx_object:call(?MODULE, {get_doc_names, DocIdList}).


%% =====================================================================
%% @doc

-spec get_record(document_id(), [document_record()]) -> document_record().

get_record(DocId, DocRecords) ->
  proplists:get_value(DocId, DocRecords).


%% =====================================================================
%% @doc Generate a unique document id.

-spec generate_id() -> erlang:timestamp().

generate_id() ->
	now().


%% =====================================================================
%% @doc Display the notebook, hiding and other siblings.

-spec show_notebook(wxSizer:wxSizer()) -> ok.

show_notebook(Sz) ->
  wxSizer:hide(Sz, 1),
  wxSizer:show(Sz, 0),
  wxSizer:layout(Sz).


%% =====================================================================
%% @doc Display the placeholder, hiding any other siblings.

-spec show_placeholder(wxSizer:wxSizer()) -> ok.

show_placeholder(Sz) ->
  wxSizer:hide(Sz, 0),
  wxSizer:show(Sz, 1),
  wxSizer:layout(Sz).


%% =====================================================================
%% @doc Ensure the notebook is visible (placeholder hidden).

-spec ensure_notebook_visible(wxAuiNotebook:wxAuiNotebook(), wxSizer:wxSizer()) -> ok | boolean().

ensure_notebook_visible(Notebook, Sz) ->
	case wxWindow:isShown(Notebook) of
		false ->
      %% enable menu items
      ide:toggle_menu_group([?MENU_GROUP_NOTEBOOK_EMPTY], true),
			show_notebook(Sz);
		true ->
      ok
	end.


%% =====================================================================
%% @doc Check whether a file is already open.
%% @private

-spec is_already_open(string(), [{document_id(), string()}]) -> document_id() | false.

is_already_open(_, []) ->
	false;
is_already_open(Path, [{DocId, #document{path=Path}} | _]) ->
	DocId;
is_already_open(Path, [_ | T]) ->
	is_already_open(Path, T).


%% =====================================================================
%% @doc

-spec apply_to_documents(function(), list(), [document_id()]) -> ok.

apply_to_documents(Fun, Args, Docs) ->
	wx_object:call(?MODULE, {apply_to_docs, {Fun, Args, Docs}}).


%% =====================================================================
%% @doc

-spec open_document(string()) -> ok.

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

-spec open_from_existing_project(string()) -> project_id().

open_from_existing_project(ProjectPath) ->
  case ide_proj_man:get_project(ProjectPath) of
    undefined ->
      ide_proj_man:open_project(ProjectPath);
    ProjectId ->
      ProjectId
  end.
