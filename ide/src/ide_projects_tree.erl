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
        add_standalone_document/1,
				remove_project/1,
        remove_standalone_document/1,
        refresh_project/1
        ]).

%% Server state
-record(state, {frame, sizer, panel, tree}).


%% =====================================================================
%% Client API
%% =====================================================================

start(Config) ->
	wx_object:start_link({local, ?MODULE}, ?MODULE, Config, []).


%% =====================================================================
%% @doc Add a project directory to the tree.

add_project(Id, Dir) ->
	wx_object:cast(?MODULE, {add_project, Id, Dir}).
add_project(Id, Dir, Pos) ->
	wx_object:cast(?MODULE, {add_project, Id, Dir, Pos}).
  

%% =====================================================================
%% @doc Delete an item from the tree

remove_project(Id) ->
	wx_object:cast(?MODULE, {remove_project, Id}).


%% =====================================================================
%% @doc Update the tree item (including children) whose data is Path.

refresh_project(Path) ->
  Tree = wx_object:call(?MODULE, tree),
	ProjectItem  = get_item_from_path(Tree, get_all_items(Tree), Path),
	{ProjectId, _} = wxTreeCtrl:getItemData(Tree, ProjectItem),
  wxTreeCtrl:delete(Tree, ProjectItem),
  wx_object:cast(?MODULE, {add_project, ProjectId, Path}),
	ok.
  

%% =====================================================================
%% @doc

add_standalone_document(Path) ->
  wx_object:cast(?MODULE, {add_standalone, Path}).


%% =====================================================================
%% @doc
 
remove_standalone_document(Path) ->
  wx_object:cast(?MODULE, {remove_standalone, Path}).


%% =====================================================================
%% Callback functions
%% =====================================================================

init(Config) ->
	Parent = proplists:get_value(parent, Config),
	Frame = proplists:get_value(frame, Config),

	Panel = wxPanel:new(Parent),
	MainSz = wxBoxSizer:new(?wxVERTICAL),
	wxPanel:setSizer(Panel, MainSz),

  Tree = wxTreeCtrl:new(Panel, [{style, ?wxTR_HAS_BUTTONS bor
                                        ?wxTR_HIDE_ROOT bor
                                        ?wxTR_FULL_ROW_HIGHLIGHT bor
                                        ?wxTR_NO_LINES}]),
	wxTreeCtrl:setIndent(Tree, 10),
	ImgList = wxImageList:new(16,16),
	wxImageList:add(ImgList, wxArtProvider:getBitmap("wxART_FOLDER", [{client,"wxART_MENU"}])),
	wxImageList:add(ImgList, wxArtProvider:getBitmap("wxART_NORMAL_FILE", [{client,"wxART_MENU"}])),
	wxImageList:add(ImgList, wxBitmap:new(wxImage:new("../icons/book.png"))),
	wxTreeCtrl:assignImageList(Tree, ImgList),

  Root = wxTreeCtrl:addRoot(Tree, "Root"),
  AddRoot = 
    fun(Name) ->
      Item = wxTreeCtrl:appendItem(Tree, Root, Name),
      % wxTreeCtrl:setItemBold(Tree, Item),
      wxTreeCtrl:setItemBackgroundColour(Tree, Item, {150,150,150}),
      wxTreeCtrl:setItemTextColour(Tree, Item, ?wxWHITE)
    end,
  AddRoot("Projects"),
  AddRoot("Standalone Files"),

	wxSizer:add(MainSz, Tree, [{proportion, 1}, {flag, ?wxEXPAND}]),

  wxTreeCtrl:connect(Tree, command_tree_item_activated, []),
	wxTreeCtrl:connect(Tree, command_tree_sel_changed, []),
	wxTreeCtrl:connect(Tree, command_tree_item_expanded, []),
	wxTreeCtrl:connect(Tree, command_tree_item_collapsed, []),

	{Panel, #state{frame=Frame, sizer=MainSz, panel=Panel, tree=Tree}}.

handle_info(Msg, State) ->
  io:format("Got Info ~p~n",[Msg]),
  {noreply,State}.
  
handle_cast({remove_project, ProjectId}, State=#state{panel=Panel, tree=Tree}) ->
  Item = get_item_from_list(Tree, ProjectId, get_projects(Tree)),
	wxPanel:freeze(Panel),
  wxTreeCtrl:delete(Tree, Item),
  % alternate_background(Tree),
  alternate_background_of_children(Tree, get_projects_root(Tree)),
	wxPanel:thaw(Panel),
  {noreply,State};
	
handle_cast({add_project, Id, Dir}, State=#state{tree=Tree}) ->
  Root = get_projects_root(Tree),
  Item = wxTreeCtrl:appendItem(Tree, Root, filename:basename(Dir), [{data, {Id, Dir}}]),
  wxTreeCtrl:setItemImage(Tree, Item, 2),
  check_dir_has_contents(Tree, Item, Dir),
  wxTreeCtrl:selectItem(Tree, Item),
  % alternate_background(Tree),
  alternate_background_of_children(Tree, get_projects_root(Tree)),
  {noreply,State};

handle_cast({add_standalone, Path}, State=#state{tree=Tree}) ->
  wxTreeCtrl:appendItem(Tree, get_standalone_root(Tree), filename:basename(Path), [{data, Path}]),
  {noreply,State};

handle_cast({remove_standalone, Path}, State=#state{tree=Tree}) ->
  Item = find_standalone(Tree, Path),
  wxTreeCtrl:delete(Tree, Item),
  % alternate_background(Tree),
  alternate_background_of_children(Tree, get_standalone_root(Tree)),
  {noreply,State}.
  
handle_call(tree, _From, State) ->
  {reply,State#state.tree,State}.

handle_event(#wx{obj=Tree, event=#wxTree{type=command_tree_item_expanded, item=Item}}, State) ->
  case is_fixed_header(Tree, Item) of
    false ->
      {_, FilePath} = wxTreeCtrl:getItemData(Tree, Item),
      insert(Tree, Item, FilePath),
      % alternate_background(Tree);
      alternate_background_of_children(Tree, get_projects_root(Tree)),
      alternate_background_of_children(Tree, get_standalone_root(Tree));
    true -> ok
  end,
	{noreply, State};
handle_event(#wx{obj=Tree, event=#wxTree{type=command_tree_item_collapsed, item=Item}}, State) ->
  case is_fixed_header(Tree, Item) of
    false ->
      wxTreeCtrl:deleteChildren(Tree, Item),
      % alternate_background(Tree);
      alternate_background_of_children(Tree, get_projects_root(Tree)),
      alternate_background_of_children(Tree, get_standalone_root(Tree));
    true -> ok
  end,
	{noreply, State};
handle_event(#wx{obj=Tree, event=#wxTree{type=command_tree_sel_changed, item=Item, itemOld=OldItem}},
						 State) ->
	case wxTreeCtrl:isTreeItemIdOk(OldItem) andalso not is_fixed_header(Tree, Item) of
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
  case is_fixed_header(Tree, Item) of
    false ->
      toggle_or_open(Tree, Item);
    true ->
      ok
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
%% @doc Set the background colour for all visible items.
%% Includes those items that are currently scrolled out of view.

% alternate_background(Tree) ->
%   lists:foldl(
%   fun(Item, Acc) ->
%     set_item_background(Tree, Item, Acc),
%     Acc+1
%   end,
%   0, lists:reverse(get_all_items(Tree))).

alternate_background_of_children(Tree, Item) ->
	lists:foldl(
	fun(Item, Acc) ->
		set_item_background(Tree, Item, Acc),
		Acc+1
	end,
	0, lists:reverse(get_children_recursively(Tree, Item))). 

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
%% @doc Get all children recursively

get_children_recursively(Tree, Item) ->
  {FirstChild, _} = wxTreeCtrl:getFirstChild(Tree, Item),
  get_children_recursively(Tree, FirstChild, []).

get_children_recursively(Tree, Item, Acc) ->
	case wxTreeCtrl:isTreeItemIdOk(Item) of
		false -> Acc;
		true ->
			Res = case wxTreeCtrl:itemHasChildren(Tree, Item) and wxTreeCtrl:isExpanded(Tree, Item) of
				true ->
					{FirstChild,_} = wxTreeCtrl:getFirstChild(Tree, Item),
					get_children_recursively(Tree, FirstChild, [Item|Acc]);
				false -> [Item|Acc]
			end,
			get_children_recursively(Tree, wxTreeCtrl:getNextSibling(Tree, Item), Res)
	end.
  
  
%% =====================================================================
%% @doc Get every item in the tree which currently occupys a row.
%% This ignores any children of a collapsed item.

get_all_items(Tree) ->
  get_children_recursively(Tree, wxTreeCtrl:getRootItem(Tree)).


%% =====================================================================
%% @doc Get the tree item whose data (path) is Path.

get_item_from_path(_Tree, [], _Path) -> ok;
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
      %Children = filelib:wildcard(File ++ "/*"),
      check_dir_has_contents(Tree, Child, File);
		_ ->
			Child = wxTreeCtrl:prependItem(Tree, Item, FileName, [{data, {Id, File}}]),
			wxTreeCtrl:setItemImage(Tree, Child, 1)
	end,
	add_files(Tree, Item, Files).


%% =====================================================================
%% @doc Get the project's root item when given any item.

get_project_root(Tree, Item) ->
	get_project_root(Tree, get_projects_root(Tree),
		wxTreeCtrl:getItemParent(Tree, Item), Item).

get_project_root(_Tree, Root, Root, Item) ->
	Item;
get_project_root(Tree, Root, Parent, Item) ->
	get_project_root(Tree, Root, wxTreeCtrl:getItemParent(Tree, Parent),
			wxTreeCtrl:getItemParent(Tree, Item)).
      

%% =====================================================================
%% @doc

get_projects(Tree) ->
  Root = get_projects_root(Tree),
  {FirstChild, _} = wxTreeCtrl:getFirstChild(Tree, Root),
  case wxTreeCtrl:isTreeItemIdOk(FirstChild) of
    true ->
      get_projects(Tree, FirstChild, [FirstChild]);
    false ->
      []
  end.
get_projects(Tree, Item, Acc) ->
  Next = wxTreeCtrl:getNextSibling(Tree, Item),
  case wxTreeCtrl:isTreeItemIdOk(Next) of
    true ->
      get_projects(Tree, Next, Acc ++ [Next]);
    false ->
      Acc
  end.
  

%% =====================================================================
%% @doc

get_item_from_list(Tree, ProjectId, [Project|Projects]) ->
  {Id, _} = wxTreeCtrl:getItemData(Tree, Project),
  case Id of
    ProjectId ->
      Project;
    _ ->
      get_item_from_list(Tree, ProjectId, Projects)
  end.


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


%% =====================================================================
%% @doc

is_projects_root(Tree, Item) ->
  case get_projects_root(Tree) of
    Item -> true;
    _ -> false
  end.
  

%% =====================================================================
%% @doc
   
is_standalone_root(Tree, Item) ->
  case get_standalone_root(Tree) of
    Item -> true;
    _ -> false
  end.


%% =====================================================================
%% @doc
 
get_projects_root(Tree) ->
  {Item, _} = wxTreeCtrl:getFirstChild(Tree, wxTreeCtrl:getRootItem(Tree)),
  Item.


%% =====================================================================
%% @doc

get_standalone_root(Tree) ->
  wxTreeCtrl:getNextSibling(Tree, get_projects_root(Tree)).
  
  
%% =====================================================================
%% @doc
  
is_fixed_header(Tree, Item) ->
  is_projects_root(Tree, Item) orelse is_standalone_root(Tree, Item).


%% =====================================================================
%% @doc
 
find_standalone(Tree, Path) ->
  Root = get_standalone_root(Tree),
  find_standalone(Tree, Root, Path).
  
find_standalone(Tree, Item, Path) ->
  case wxTreeCtrl:getItemData(Item) of
    Path -> Item;
    _ ->
      find_standalone(Tree, wxTreeCtrl:getNextSibling(Tree, Item), Path)
  end.
  
  
%% =====================================================================
%% @doc

toggle_or_open(Tree, Item) ->
  FilePath = get_path(Tree, Item),
  case filelib:is_dir(FilePath) of
    true ->
      wxTreeCtrl:toggle(Tree, Item);
    false ->
      {Id, _Root} = wxTreeCtrl:getItemData(Tree, get_project_root(Tree, Item)),
      doc_manager:create_document(FilePath, Id)
  end.
