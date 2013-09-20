-module(ide_projects_tree).

-include_lib("wx/include/wx.hrl").
-include("ide.hrl").

-behaviour(wx_object).
-export([
        new/1,
        init/1,
        terminate/2,
        code_change/3,
        handle_info/2,
        handle_call/3,
        handle_cast/2,
        handle_event/2]).

-export([
        get_open_projects/0,
        refresh_tree/0]).

-record(state, {tree}).

-define(FOLDER_IMAGE, 0).
-define(FILE_IMAGE, 1).

new(Config) ->
	wx_object:start_link({local, ?MODULE}, ?MODULE, Config, []).

init(Config) ->
	ProjectTree = make_tree(Config),
	{ProjectTree, #state{tree=ProjectTree}}.


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
	File         = wxTreeCtrl:getItemData(Tree, SelectedItem),
	Text         = wxTreeCtrl:getItemText(Tree, SelectedItem),
	IsDir        = filelib:is_dir(File),
	case IsDir of
		true ->
			wxTreeCtrl:toggle(Tree, SelectedItem),
			ok;
		_ ->
			%% CHECK IF FILE CAN BE OPENED AS TEXT
			Filename = filename:basename(File),
			{_, FileContents} = file:read_file(File),
			doc_manager:new_document_from_existing(File, Filename, binary_to_list(FileContents))
	end,
	{noreply, State};
handle_event(_Event, State) ->
  io:format("SIDE BAR EVENT CA~n"),
  {noreply, State}.

code_change(_, _, State) ->
	{stop, not_yet_implemented, State}.

terminate(_Reason, #state{tree=Tree}) ->
	io:format("TERMINATE PROJECTS TREE~n"),
	wxTreeCtrl:destroy(Tree).


%% =====================================================================
%% @doc Make a directory tree from project directory. Gets root from
%% user preferences.

-spec make_tree(Parent) -> Result when
	Parent :: wxPanel:wxPanel(),
	Result :: wxTreeCtrl:wxTreeCtrl().

make_tree(Parent) ->
  Tree = wxTreeCtrl:new(Parent, [{style, ?wxTR_HAS_BUTTONS bor
                                         ?wxTR_HIDE_ROOT bor
                                         ?wxTR_FULL_ROW_HIGHLIGHT}]),
  ImgList = wxImageList:new(24,24),
	wxImageList:add(ImgList, wxArtProvider:getBitmap("wxART_FOLDER")),
	wxImageList:add(ImgList, wxArtProvider:getBitmap("wxART_NORMAL_FILE")),
	wxTreeCtrl:assignImageList(Tree, ImgList),

  ProjectDir = user_prefs:get_user_pref({pref, project_dir}),
  Root = wxTreeCtrl:addRoot(Tree, ProjectDir),
  build_tree(Tree, Root, ProjectDir, main),
  wxTreeCtrl:connect(Tree, command_tree_item_activated, []),
	Tree.


%% =====================================================================
%% @doc Get a list of files in a given root directory then build its
%% subdirectories.

build_tree(Tree, Root, Dir, main) ->
  Files = filelib:wildcard(Dir ++ "/*"),
	add_files(Tree, Root, Files).
build_tree(Tree, Root, Dir) ->
	Files = filelib:wildcard(Dir ++ "/*"),
	add_files(Tree, Root, lists:reverse(Files)).


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

