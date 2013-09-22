-module(ide_projects_tree).

-include_lib("wx/include/wx.hrl").
-include("ide.hrl").

-behaviour(wx_object).
-export([
        init/1,
        terminate/2,
        code_change/3,
        handle_info/2,
        handle_call/3,
        handle_cast/2,
        handle_event/2]).

-export([
        start/1,
				add_project/1,
        get_open_projects/0,
        refresh_tree/0]).

-record(state, {panel, tree}).

-define(FOLDER_IMAGE, 0).
-define(FILE_IMAGE, 1).

start(Parent) ->
	wx_object:start_link({local, ?MODULE}, ?MODULE, Parent, []).

init(Parent) ->
	Panel = wxPanel:new(Parent),
	Sz = wxBoxSizer:new(?wxVERTICAL),
	wxPanel:setSizer(Panel, Sz),
		
  Tree = wxTreeCtrl:new(Panel, [{style, ?wxTR_HAS_BUTTONS bor
                                         ?wxTR_HIDE_ROOT bor
                                         ?wxTR_FULL_ROW_HIGHLIGHT}]),
  
	ImgList = wxImageList:new(24,24),
	wxImageList:add(ImgList, wxArtProvider:getBitmap("wxART_FOLDER", [{client,"wxART_MENU"}])),
	wxImageList:add(ImgList, wxArtProvider:getBitmap("wxART_NORMAL_FILE", [{client,"wxART_MENU"}])),
	wxImageList:add(ImgList, wxArtProvider:getBitmap("wxART_HELP_BOOK", [{client,"wxART_MENU"}])),
	wxTreeCtrl:assignImageList(Tree, ImgList),
	
  % ProjectDir = user_prefs:get_user_pref({pref, project_dir}),
	
  wxTreeCtrl:addRoot(Tree, "ProjectTreeRoot"),
	wxSizer:add(Sz, Tree, [{proportion, 1}, {flag, ?wxEXPAND}]),
	
  wxTreeCtrl:connect(Tree, command_tree_item_activated, []),

	{Panel, #state{panel=Panel, tree=Tree}}.


%% =====================================================================
%% OTP callbacks
%%
%% =====================================================================

handle_info(Msg, State) ->
  io:format("Got Info ~p~n",[Msg]),
  {noreply,State}.

handle_cast(Msg, State) ->
  io:format("Got cast ~p~n",[Msg]),
  {noreply,State}.

handle_call(tree, _From, State) ->
  {reply,State#state.tree,State};
handle_call(Msg, _From, State) ->
  io:format("Got Call ~p~n",[Msg]),
  {reply,ok,State}.

handle_event(#wx{obj=Tree, event=#wxTree{type=command_tree_item_activated}}, State) ->
	SelectedItem = wxTreeCtrl:getSelection(Tree),
	File = wxTreeCtrl:getItemData(Tree, SelectedItem),
	case filelib:is_dir(File) of
		true ->
			wxTreeCtrl:toggle(Tree, SelectedItem),
			ok;
		_ ->
			%% CHECK IF FILE CAN BE OPENED AS TEXT
			Filename = filename:basename(File),
			{_, FileContents} = file:read_file(File),
			doc_manager:new_document_from_existing(File, Filename, binary_to_list(FileContents))
	end,
	{noreply, State}.

code_change(_, _, State) ->
	{stop, not_yet_implemented, State}.

terminate(_Reason, #state{panel=Panel}) ->
	io:format("TERMINATE PROJECTS TREE~n"),
	wxPanel:destroy(Panel).


%% =====================================================================
%% @doc Get a list of files in a given root directory then build its
%% subdirectories.

add_project(Dir) ->
	Tree = wx_object:call(?MODULE, tree),
	Id = wxTreeCtrl:appendItem(Tree, wxTreeCtrl:getRootItem(Tree), filename:basename(Dir), [{data, Dir}]),
	wxTreeCtrl:setItemImage(Tree, Id, 2),
	build_tree(Tree, Id, Dir),
	ide:set_title(filename:basename(Dir)),
	ok.
	
	
%% =====================================================================
%% @doc Get a list of files in a given root directory then build its
%% subdirectories.

build_tree(Tree, Parent, Dir, main) ->
  Files = filelib:wildcard(Dir ++ "/*"),
	add_files(Tree, Parent, Files).
build_tree(Tree, Parent, Dir) ->
	Files = filelib:wildcard(Dir ++ "/*"),
	add_files(Tree, Parent, lists:reverse(Files)).


%% =====================================================================
%% @doc Add files to the given directory.

add_files(_, _, []) ->
	ok;
add_files(Tree, Root, [File|Files]) ->
	FileName = filename:basename(File),
	IsDir = filelib:is_dir(File),
	case IsDir of
		true ->
			Child = wxTreeCtrl:appendItem(Tree, Root, FileName, [{data, File}]),
			wxTreeCtrl:setItemImage(Tree, Child, ?FOLDER_IMAGE),
			build_tree(Tree, Child, File);
		_ ->
			Child = wxTreeCtrl:prependItem(Tree, Root, FileName, [{data, File}]),
			wxTreeCtrl:setItemImage(Tree, Child, ?FILE_IMAGE)
	end,
	add_files(Tree, Root, Files).


%% =====================================================================
%% @doc Refresh projects tree.

refresh_tree() ->
  Tree = wx_object:call(?MODULE, tree),
  wxTreeCtrl:deleteAllItems(Tree),
  ProjectDir = user_prefs:get_user_pref({pref, project_dir}),
  Root = wxTreeCtrl:addRoot(Tree, ProjectDir),
  build_tree(Tree, Root, ProjectDir, main).


%% =====================================================================
%% @doc Get all open projects. Returns a list of tuples that contain 
%% the tree id and path of the root folder.

-spec get_open_projects() -> Result when
	Result :: [{integer(), string()}].

get_open_projects() ->
  Tree = wx_object:call(?MODULE, tree),
  Root = wxTreeCtrl:getRootItem(Tree),
  get_projects(Tree, Root).

get_projects(Tree, Root) ->
  {FirstChild, FirstCookie} = wxTreeCtrl:getFirstChild(Tree, Root),
  case wxTreeCtrl:isTreeItemIdOk(FirstChild) of
    true ->
      Path = wxTreeCtrl:getItemData(Tree, FirstChild),
      get_projects(Tree, Root, FirstCookie, [{FirstChild, Path}]);
    false ->
      []
  end.

get_projects(Tree, Root, Cookie, List) ->
  {NextChild, NextCookie} = wxTreeCtrl:getNextChild(Tree, Root, Cookie),
  case wxTreeCtrl:isTreeItemIdOk(NextChild) of
    true ->
      Path = wxTreeCtrl:getItemData(Tree, NextChild),
      get_projects(Tree, Root, NextCookie, List ++ [{NextChild, Path}]);
    false ->
      List
  end.

