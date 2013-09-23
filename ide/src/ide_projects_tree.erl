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
				delete_project/1,
        get_open_projects/0,
        refresh_tree/0]).

-record(state, {frame, panel, tree}).

-define(FOLDER_IMAGE, 0).
-define(FILE_IMAGE, 1).

start(Config) ->
	wx_object:start_link({local, ?MODULE}, ?MODULE, Config, []).

init(Config) ->
	Parent = proplists:get_value(parent, Config),
	Frame = proplists:get_value(frame, Config),
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
	wxTreeCtrl:connect(Tree, command_tree_sel_changed, []),
	
	Dir1 = "/Users/tommo/Desktop/erlang/erlangIDE/ide/priv",
	Dir2 = "/Users/tommo/Desktop/erlang/erlangIDE/ide/include",
	Dir3 = "/Users/tommo/Desktop/erlang/erlangIDE/ide/src",
	
	Id1 = wxTreeCtrl:appendItem(Tree, wxTreeCtrl:getRootItem(Tree), filename:basename(Dir1), [{data, Dir1}]),
	wxTreeCtrl:setItemImage(Tree, Id1, 2),
	build_tree(Tree, Id1, Dir1),
	% 
	% Id2 = wxTreeCtrl:appendItem(Tree, wxTreeCtrl:getRootItem(Tree), filename:basename(Dir2), [{data, Dir2}]),
	% wxTreeCtrl:setItemImage(Tree, Id2, 2),
	% build_tree(Tree, Id2, Dir2),
	% 
	% Id3 = wxTreeCtrl:appendItem(Tree, wxTreeCtrl:getRootItem(Tree), filename:basename(Dir3), [{data, Dir3}]),
	% wxTreeCtrl:setItemImage(Tree, Id3, 2),
	% build_tree(Tree, Id3, Dir3),
	% 
	% io:format("Item1: ~p~n", [wxTreeCtrl:getItemText(Tree, Id1)]),
	% io:format("Item2: ~p~n", [wxTreeCtrl:getItemText(Tree, Id2)]),
	% io:format("Item3: ~p~n", [wxTreeCtrl:getItemText(Tree, Id3)]),
	
	Dir4 = "/Users/tommo/Desktop/erlang/erlangIDE/ide/ebin",
	% wxTreeCtrl:deleteChildren(Tree, Id1),
	wxTreeCtrl:delete(Tree, Id1),
	% wxTreeCtrl:deleteAllItems(Tree),
	Id4 = wxTreeCtrl:appendItem(Tree, wxTreeCtrl:getRootItem(Tree), filename:basename(Dir4), [{data, Dir4}]),
	wxTreeCtrl:setItemImage(Tree, Id1, 2),
	build_tree(Tree, Id4, Dir4),
	
	print_tree_debug(Tree),
	

	{Panel, #state{frame=Frame, panel=Panel, tree=Tree}}.

print_tree_debug(Tree) ->
	Root = wxTreeCtrl:getRootItem(Tree),
	print_tree_debug(Tree, Root, 0).
	
print_tree_debug(Tree, Node, Indent) ->
	Spac = lists:duplicate(Indent + 1, "---"),
	io:format(Spac ++ "Node: ~p~n", [wxTreeCtrl:getItemData(Tree, Node)]),
	case wxTreeCtrl:itemHasChildren(Tree, Node) of
		true ->
			{Child, _} = wxTreeCtrl:getFirstChild(Tree, Node),
			print_tree_debug(Tree, Child, Indent + 1);
		false -> ok
	end,
	Sibling = wxTreeCtrl:getNextSibling(Tree, Node),
	case wxTreeCtrl:isTreeItemIdOk(Sibling) of
		true -> print_tree_debug(Tree, Sibling, Indent);
		false -> ok
	end.
	

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
  {reply,State#state.tree,State}.

handle_event(#wx{obj=Tree, event=#wxTree{type=command_tree_sel_changed, item=Item}}, 
						 State=#state{frame=Frame}) ->
	io:format("~nTREE INVISIBLE ROOT: ~p~n", [wxTreeCtrl:getRootItem(Tree)]),
	io:format("PROJECT ROOT: ~p~n", [get_project_root(Tree, Item)]),
	ProjRoot = get_project_root(Tree, Item),
	Data = wxTreeCtrl:getItemData(Tree, ProjRoot),
	% ProjName = filename:basename(Data),
	% ide:set_title(ProjName),
	% ide_menu:update_label(wxFrame:getMenuBar(Frame), ?MENU_ID_CLOSE_PROJECT, "Close Project (" ++ ProjName ++ ")"),
	doc_manager:set_active_project({ProjRoot, Data}),
	{noreply, State};
handle_event(#wx{obj=Tree, event=#wxTree{type=command_tree_item_activated, item=Item}}, 
						State=#state{frame=Frame}) ->
	File = wxTreeCtrl:getItemData(Tree, Item),
	case filelib:is_dir(File) of
		true ->
			wxTreeCtrl:toggle(Tree, Item),
			ok;
		_ ->
			%% CHECK IF FILE CAN BE OPENED AS TEXT
			try 
				FileContents = ide_io:read_file(File),
				doc_manager:new_document_from_existing(File, filename:basename(File), 
					FileContents, [{project,{wxTreeCtrl:getRootItem(Tree), 
						wxTreeCtrl:getItemData(Tree, get_project_root(Tree, Item))}}])
			catch
				throw:_ -> lib_dialog_wx:error_msg(Frame, "The file could not be loaded.")
			end
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
	Root = wxTreeCtrl:getRootItem(Tree),
	Item = wxTreeCtrl:appendItem(Tree, Root, filename:basename(Dir), [{data, Dir}]),
	wxTreeCtrl:setItemImage(Tree, Item, 2),
	build_tree(Tree, Item, Dir),
	% ide:set_title(filename:basename(Dir)),
	% doc_manager:set_active_project({Root, Dir}),
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


%% =====================================================================
%% @doc Get the project's root item when given any item.

get_project_root(Tree, Item) ->
	get_project_root(Tree, wxTreeCtrl:getRootItem(Tree), 
		wxTreeCtrl:getItemParent(Tree, Item), Item).

get_project_root(Tree, Root, Root, Item) ->
	Item;
get_project_root(Tree, Root, Parent, Item) ->
	get_project_root(Tree, Root, wxTreeCtrl:getItemParent(Tree, Parent), 
			wxTreeCtrl:getItemParent(Tree, Item)).


delete_project(Id) ->
	wxTreeCtrl:delete(wx_object:call(?MODULE, tree), Id).