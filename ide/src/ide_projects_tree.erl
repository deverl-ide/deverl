%% =====================================================================
%% @author
%% @copyright
%% @title
%% @version
%% @doc
%% Each node in the tree contains the path of the file, accept for the
%% root of each project which contains the project id and path.
%% @end
%% =====================================================================

-module(ide_projects_tree).

-include_lib("wx/include/wx.hrl").
-include("ide.hrl").

%% wx_object
-behaviour(wx_object).
-export([
        init/1,
        terminate/2,
        code_change/3,
        handle_info/2,
        handle_call/3,
        handle_cast/2,
        handle_event/2
        ]).

%% API
-export([
        start/1,
        add_project/2,
				add_project/3,
				delete_project/1,
        refresh_project/1
        ]).

%% Server state
-record(state, {frame, sizer, panel, tree, placeholder}).


%% =====================================================================
%% Client API
%% =====================================================================

start(Config) ->
	wx_object:start_link({local, ?MODULE}, ?MODULE, Config, []).


%% =====================================================================
%% @doc Add a project directory to the tree.

add_project(Id, Dir) ->
	wx_object:cast(?MODULE, {add, Id, Dir}).
add_project(Id, Dir, Pos) ->
	wx_object:cast(?MODULE, {add, Id, Dir, Pos}).


%% =====================================================================
%% @doc Delete an item from the tree

delete_project(Id) ->
	wx_object:cast(?MODULE, {delete, Id}).


%% =====================================================================
%% @doc Update the tree item (including children) whose data is Path.

refresh_project(Path) ->
  Tree = wx_object:call(?MODULE, tree),
  % Root = wxTreeCtrl:getRootItem(Tree),
	ProjectItem  = get_item_from_path(Tree, get_all_items(Tree), Path),
	{ProjectId, _} = wxTreeCtrl:getItemData(Tree, ProjectItem),
  wxTreeCtrl:delete(Tree, ProjectItem),
	%%%%%%
  wx_object:cast(?MODULE, {add, ProjectId, Path, 0}),
	ok.


%% =====================================================================
%% Callback functions
%% =====================================================================

init(Config) ->
	Parent = proplists:get_value(parent, Config),
	Frame = proplists:get_value(frame, Config),

	Panel = wxPanel:new(Parent),
	MainSz = wxBoxSizer:new(?wxVERTICAL),
	wxPanel:setSizer(Panel, MainSz),

	Placeholder = lib_widgets:placeholder(Panel, "No Open Projects"),
	wxSizer:add(MainSz, Placeholder, [{proportion, 1}, {flag, ?wxEXPAND}]),

  Tree = wxTreeCtrl:new(Panel, [{style, ?wxTR_HAS_BUTTONS bor
                                        ?wxTR_HIDE_ROOT bor
                                        ?wxTR_FULL_ROW_HIGHLIGHT}]),
	wxTreeCtrl:setIndent(Tree, 10),
	ImgList = wxImageList:new(16,16),
	wxImageList:add(ImgList, wxArtProvider:getBitmap("wxART_FOLDER", [{client,"wxART_MENU"}])),
	wxImageList:add(ImgList, wxArtProvider:getBitmap("wxART_NORMAL_FILE", [{client,"wxART_MENU"}])),
	wxImageList:add(ImgList, wxBitmap:new(wxImage:new("../icons/book.png"))),
	wxTreeCtrl:assignImageList(Tree, ImgList),

  wxTreeCtrl:addRoot(Tree, "ProjectTreeRoot"),
	wxSizer:add(MainSz, Tree, [{proportion, 1}, {flag, ?wxEXPAND}]),

	hide_tree(MainSz, Tree),

  wxTreeCtrl:connect(Tree, command_tree_item_activated, []),
	wxTreeCtrl:connect(Tree, command_tree_sel_changed, []),
	wxTreeCtrl:connect(Tree, command_tree_item_expanded, []),
	wxTreeCtrl:connect(Tree, command_tree_item_collapsed, []),

	{Panel, #state{frame=Frame, sizer=MainSz, panel=Panel, tree=Tree, placeholder=Placeholder}}.

handle_info(Msg, State) ->
  io:format("Got Info ~p~n",[Msg]),
  {noreply,State}.

handle_cast({delete, Item}, State=#state{sizer=Sz, panel=Panel, tree=Tree}) ->
	wxPanel:freeze(Panel),
  wxTreeCtrl:delete(Tree, Item),
	case wxTreeCtrl:getCount(Tree) of
		0 -> show_placeholder(Sz);
		_ -> ok
	end,
	alternate_background(Tree),
	wxPanel:thaw(Panel),
  {noreply,State};
	
handle_cast({add, Id, Dir}, State=#state{sizer=Sz, tree=Tree}) ->
	case wxWindow:isShown(Tree) of
		false ->
			show_tree(Sz);
		true -> 
      ok
	end,
  Root = wxTreeCtrl:getRootItem(Tree),
  Item = wxTreeCtrl:appendItem(Tree, Root, filename:basename(Dir), [{data, {Id, Dir}}]),
  wxTreeCtrl:setItemImage(Tree, Item, 2),
  check_dir_has_contents(Tree, Item, Dir),
  wxTreeCtrl:selectItem(Tree, Item),
	alternate_background(Tree),
  {noreply,State};
	
handle_cast({add, Id, Dir, Pos}, State=#state{sizer=Sz, tree=Tree}) ->
	case wxWindow:isShown(Tree) of
		false ->
			show_tree(Sz);
		true -> ok
	end,
	Root = wxTreeCtrl:getRootItem(Tree),
	Item = wxTreeCtrl:appendItem(Tree, Root, filename:basename(Dir), [{data, {Id, Dir}}]),
	wxTreeCtrl:setItemImage(Tree, Item, 2),
  check_dir_has_contents(Tree, Item, Dir),
  wxTreeCtrl:selectItem(Tree, Item),
	alternate_background(Tree),
  {noreply,State};

handle_cast(Msg, State) ->
  io:format("Got cast ~p~n",[Msg]),
  {noreply,State}.

handle_call(tree, _From, State) ->
  {reply,State#state.tree,State}.

handle_event(#wx{obj=Tree, event=#wxTree{type=command_tree_item_expanded}}, State) ->
	alternate_background(Tree),
  %print_tree_debug(Tree),
	{noreply, State};
handle_event(#wx{obj=Tree, event=#wxTree{type=command_tree_item_collapsed, item=Item}}, State) ->
	alternate_background(Tree),
  %print_tree_debug(Tree),
	{noreply, State};
handle_event(#wx{obj=Tree, event=#wxTree{type=command_tree_sel_changed, item=Item, itemOld=OldItem}},
						 State=#state{frame=Frame}) ->
	case wxTreeCtrl:isTreeItemIdOk(OldItem) of
		false ->  %% Deleted item
			ok;
		true ->
			ProjRoot = get_project_root(Tree, Item),
			{ProjectId, _Path} = wxTreeCtrl:getItemData(Tree, ProjRoot),
			project_manager:set_active_project(ProjectId)
	end,
	{noreply, State};
handle_event(#wx{obj=Tree, event=#wxTree{type=command_tree_item_activated, item=Item}},
						State=#state{frame=Frame}) ->
	File = get_path(Tree, Item),
	case filelib:is_dir(File) of
		true ->
			io:format("EEWEQQWDQW~n"),
      check_tree_item_expanded(Tree, Item),
			ok;
		_ ->
			%% CHECK IF FILE CAN BE OPENED AS TEXT (TO BE DONE IN IO MODULE, NOT HERE!!)
			try
				FileContents = ide_io:read_file(File),
				{Id, _Root} = wxTreeCtrl:getItemData(Tree, get_project_root(Tree, Item)),
				project_manager:open_file(File, FileContents, Id)
			catch
				throw:_ -> lib_dialog_wx:msg_error(Frame, "The file could not be loaded.")
			end
	end,
	{noreply, State}.

code_change(_, _, State) ->
	{stop, not_yet_implemented, State}.

terminate(_Reason, #state{panel=Panel}) ->
	io:format("TERMINATE PROJECTS TREE~n"),
	wxPanel:destroy(Panel).


%% =====================================================================
%% Internal functions
%% =====================================================================

%% =====================================================================
%% @doc Get the path associated to the tree item Item.

get_path(Tree, Item) ->
	case wxTreeCtrl:getItemData(Tree, Item) of
		{_Id, Path} -> Path;
		Path -> Path
	end.


%% =====================================================================
%% @doc Show the tree ctrl.

show_tree(Sz) ->
	wxSizer:hide(Sz, 0),
	wxSizer:show(Sz, 1),
	wxSizer:layout(Sz).


%% =====================================================================
%% @doc Display the placeholder.
%% This is displayed when no projects are open.

show_placeholder(Sz) ->
	wxSizer:hide(Sz, 1),
	wxSizer:show(Sz, 0),
	wxSizer:layout(Sz).


%% =====================================================================
%% @doc Hide the tree ctrl.

hide_tree(Sz, Tree) ->
	wxSizer:hide(Sz, Tree),
	wxSizer:layout(Sz).


%% =====================================================================
%% @doc Set the background colour for all visible items.
%% Includes those items that are currently scrolled out of view.

alternate_background(Tree) ->
	lists:foldl(
	fun(Item, Acc) ->
		set_item_background(Tree, Item, Acc),
		Acc+1
	end,
	0, lists:reverse(get_all_items(Tree))).


%% =====================================================================
%% @doc Set the background colour for a single item.

set_item_background(Tree, Item, Index) ->
	case Index rem 2 of
	  0 ->
			wxTreeCtrl:setItemBackgroundColour(Tree, Item, ?ROW_BG_EVEN);
	  _ ->
	 		wxTreeCtrl:setItemBackgroundColour(Tree, Item, ?ROW_BG_ODD)
	end.


%% =====================================================================
%% @doc Get every item in the tree which currently occupys a row.
%% This ignores any children of a collapsed item.

get_all_items(Tree) ->
	{FirstChild,_} = wxTreeCtrl:getFirstChild(Tree, wxTreeCtrl:getRootItem(Tree)),
	get_all_items(Tree, FirstChild, []).
get_all_items(Tree, Item, Acc) ->
	case wxTreeCtrl:isTreeItemIdOk(Item) of
		false -> Acc;
		true ->
			Res = case wxTreeCtrl:itemHasChildren(Tree, Item) and wxTreeCtrl:isExpanded(Tree, Item) of
				true ->
					{FirstChild,_} = wxTreeCtrl:getFirstChild(Tree, Item),
					get_all_items(Tree, FirstChild, [Item|Acc]);
				false -> [Item|Acc]
			end,
			get_all_items(Tree, wxTreeCtrl:getNextSibling(Tree, Item), Res)
	end.


%% =====================================================================
%% @doc Get the tree item whose data (path) is Path.

get_item_from_path(Tree, [], Path) -> ok;
get_item_from_path(Tree, [H|T], Path) ->
	case get_path(Tree, H) of
		Path -> H;
		_ -> get_item_from_path(Tree, T, Path)
	end.
  
  
  
  
  
%% =====================================================================
%% @doc Get a list of files in a given root directory then build its
%% subdirectories.

insert(Tree, Parent, Dir) ->
	Files = filelib:wildcard(Dir ++ "/*"),
	add_files(Tree, Parent, lists:reverse(Files)).


add_files(_, _, []) ->
  ok;
add_files(Tree, Item, [File|Files]) ->
  FileName = filename:basename(File),
	IsDir = filelib:is_dir(File),
  {Id, _} = wxTreeCtrl:getItemData(Tree, Item),
	case IsDir of
		true ->
			Child = wxTreeCtrl:appendItem(Tree, Item, FileName, [{data, {Id, File}}]),
			wxTreeCtrl:setItemImage(Tree, Child, 0),
      Children = filelib:wildcard(File ++ "/*"),
      check_dir_has_contents(Tree, Child, File);
		_ ->
			Child = wxTreeCtrl:prependItem(Tree, Item, FileName, [{data, {Id, File}}]),
			wxTreeCtrl:setItemImage(Tree, Child, 1)
	end,
	add_files(Tree, Item, Files).


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


%% =====================================================================
%% @doc Check if tree item has children. If so, set item has children.

check_dir_has_contents(Tree, Item, FilePath) ->
  Children = filelib:wildcard(FilePath ++ "/*"),
  case Children of
    [] ->
      ok;
    _ ->
      wxTreeCtrl:setItemHasChildren(Tree, Item, [])
  end.


%% =====================================================================
%% @doc

check_tree_item_expanded(Tree, Item) ->
  case wxTreeCtrl:isExpanded(Tree, Item) of
    true ->
      wxTreeCtrl:toggle(Tree, Item),
      wxTreeCtrl:deleteChildren(Tree, Item);
    false ->
      check_tree_item_has_children(Tree, Item)
  end. 
  
  
%% =====================================================================
%% @doc

check_tree_item_has_children(Tree, Item) ->
  case wxTreeCtrl:itemHasChildren(Tree, Item) of
    true ->
      {_, FilePath} = wxTreeCtrl:getItemData(Tree, Item),
      insert(Tree, Item, FilePath),
      wxTreeCtrl:toggle(Tree, Item);
    false ->
      ok
  end.
  
      
%% =====================================================================
%% @doc Print the tree for debugging purposes

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
