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
        handle_event/2,
        handle_sync_event/3
        ]).

%% API
-export([
        start/1,
        add_project/2,
				add_project/3,
        add_standalone_document/1,
				remove_project/1,
        remove_standalone_document/1,
        refresh_project/1,
        insert_file/1
        ]).

%% Macros
-define(ICON_FOLDER, 0).
-define(ICON_FOLDER_OPEN, 1).
-define(ICON_PROJECT, 2).
-define(ICON_PROJECT_OPEN, 3).
-define(ICON_DOCUMENT, 4).
-define(ICON_INFO, 5).

-define(HEADER_BACKGROUND, {100,141,220}).

-define(HEADER_PROJECTS, 0).
-define(HEADER_FILES, 1).

-define(HEADER_PROJECTS_EMPTY, "No open projects").
-define(HEADER_FILES_EMPTY, "No open files").

%% Server state
-record(state, {frame, panel, tree}).


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
%  Tree = wx_object:call(?MODULE, tree),
%	ProjectItem  = get_item_from_path(Tree, get_all_items(Tree), Path),
%	{ProjectId, _} = wxTreeCtrl:getItemData(Tree, ProjectItem),
%  wxTreeCtrl:delete(Tree, ProjectItem),
%  wx_object:cast(?MODULE, {add_project, ProjectId, Path}),
	ok.
  
  
insert_file(FilePath) ->
  Tree = wx_object:call(?MODULE, tree),
  FileDir = filename:dirname(FilePath),
  case get_item_from_path(Tree, get_all_items(Tree), FileDir) of
    no_item ->
      find_root(FilePath);
    Item ->
      {Id, _} = wxTreeCtrl:getItemData(Tree, Item),
      append_item(Tree, Item, filename:basename(FilePath), [{image, ?ICON_DOCUMENT}, {data, {Id, FilePath}}])
  end.
  

  
find_root(FilePath) ->
  find_root(FilePath, []).
  
find_root(FilePath, Acc) ->
  
  Tree = wx_object:call(?MODULE, tree),
  case get_item_from_path(Tree, get_all_items(Tree), FilePath) of 
    no_item ->
      find_root(filename:dirname(FilePath), [FilePath|Acc]);
    Item ->
      wxTreeCtrl:toggle(Tree, Item),
      io:format("FILE DIRS ~p~n", [Acc]),
      toggle_items(Tree, Acc)
  end.
  
toggle_items(Tree, [FilePath]) ->
  wxTreeCtrl:selectItem(Tree, get_item_from_path(Tree, get_all_items(Tree), FilePath));
toggle_items(Tree, [Path|Paths]) ->
  io:format("PATH ~p~n", [Path]),
  io:format("ITEM FROM PATH ~p~n", [get_item_from_path(Tree, get_all_items(Tree), Path)]),
  wxTreeCtrl:toggle(Tree, get_item_from_path(Tree, get_all_items(Tree), Path)),
  print_tree_debug(Tree),
  toggle_items(Tree, Paths).

  

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
                                        ?wxTR_HAS_VARIABLE_ROW_HEIGHT bor
                                        ?wxTR_NO_LINES}]),
	wxTreeCtrl:setIndent(Tree, 10),
	ImgList = wxImageList:new(14,14),
	wxImageList:add(ImgList, wxBitmap:new(wxImage:new("../icons/14x14/blue-folder-horizontal.png"))), 
	wxImageList:add(ImgList, wxBitmap:new(wxImage:new("../icons/14x14/blue-folder-horizontal-open.png"))),
	wxImageList:add(ImgList, wxBitmap:new(wxImage:new("../icons/14x14/book.png"))),
	wxImageList:add(ImgList, wxBitmap:new(wxImage:new("../icons/14x14/book-open.png"))),
	wxImageList:add(ImgList, wxBitmap:new(wxImage:new("../icons/14x14/document.png"))),
	wxImageList:add(ImgList, wxBitmap:new(wxImage:new("../icons/14x14/information-white.png"))),
	wxTreeCtrl:assignImageList(Tree, ImgList),

  Root = wxTreeCtrl:addRoot(Tree, "Root"),
  AddRoot = 
    fun(Id, Name, Info) ->
      Item = append_item(Tree, Root, Name, [{data, Id}]),
      Placeholder = append_item(Tree, Item, Info, [{data, placeholder}]),
      wxTreeCtrl:toggle(Tree, Item),
      wxTreeCtrl:setItemImage(Tree, Placeholder, ?ICON_INFO),
      wxTreeCtrl:setItemBackgroundColour(Tree, Item, ?HEADER_BACKGROUND),
      wxTreeCtrl:setItemTextColour(Tree, Item, ?wxWHITE)
    end,
  AddRoot(?HEADER_PROJECTS, "Projects", ?HEADER_PROJECTS_EMPTY),
  AddRoot(?HEADER_FILES, "Standalone Files", ?HEADER_FILES_EMPTY),

	wxSizer:add(MainSz, Tree, [{proportion, 1}, {flag, ?wxEXPAND}]),

  wxTreeCtrl:connect(Tree, command_tree_item_activated, []),
	wxTreeCtrl:connect(Tree, command_tree_sel_changed, []),
	wxTreeCtrl:connect(Tree, command_tree_sel_changing, [callback]), %% To veto a selection
	wxTreeCtrl:connect(Tree, command_tree_item_expanded, []),
	wxTreeCtrl:connect(Tree, command_tree_item_collapsed, []),

	{Panel, #state{frame=Frame, panel=Panel, tree=Tree}}.

handle_info(Msg, State) ->
  io:format("Got Info ~p~n",[Msg]),
  {noreply,State}.
	
%% ALMOST IDENTICAL TO THE SUBSEQUENT HANDLER
handle_cast({add_project, Id, Dir}, State=#state{tree=Tree}) ->
  Root = get_projects_root(Tree),
  remove_placeholder(Tree, Root),
  Item = append_item(Tree, Root, filename:basename(Dir), [{data, {Id, Dir}}]),
  wxTreeCtrl:setItemImage(Tree, Item, ?ICON_PROJECT),
  check_dir_has_contents(Tree, Item, Dir),
  wxTreeCtrl:selectItem(Tree, Item),
  alternate_background_of_children(Tree, Root),
  {noreply,State};
  
handle_cast({remove_project, ProjectId}, State=#state{panel=Panel, tree=Tree}) ->
  Item = get_item_from_list(Tree, ProjectId, get_projects(Tree)),
	wxPanel:freeze(Panel),
  wxTreeCtrl:delete(Tree, Item),
  alternate_background_of_children(Tree, get_projects_root(Tree)),
  insert_placeholder(Tree, get_projects_root(Tree), ?HEADER_PROJECTS_EMPTY),
	wxPanel:thaw(Panel),
  {noreply,State};
  
handle_cast({add_standalone, Path}, State=#state{tree=Tree}) ->
  Root = get_standalone_root(Tree),
  remove_placeholder(Tree, Root),
  Item = append_item(Tree, Root, filename:basename(Path), [{data, Path}]),
  wxTreeCtrl:setItemImage(Tree, Item, ?ICON_DOCUMENT),
  wxTreeCtrl:selectItem(Tree, Item),
  alternate_background_of_children(Tree, Root),
  {noreply,State};

handle_cast({remove_standalone, Path}, State=#state{tree=Tree}) ->
  Item = find_standalone(Tree, Path),
  wxTreeCtrl:delete(Tree, Item),
  alternate_background_of_children(Tree, get_standalone_root(Tree)),
  insert_placeholder(Tree, get_standalone_root(Tree), ?HEADER_FILES_EMPTY),
  {noreply,State}.
  
handle_call(tree, _From, State) ->
  {reply,State#state.tree,State}.

handle_event(#wx{obj=Tree, event=#wxTree{type=command_tree_item_expanded, item=Item}}, State) ->
  % case is_fixed_header(Tree, Item) of
  case is_selectable(Tree, Item) of
    true ->
      Image = wxTreeCtrl:getItemImage(Tree, Item),
      Idx = case Image of
        ?ICON_PROJECT -> ?ICON_PROJECT_OPEN;
        ?ICON_FOLDER -> ?ICON_FOLDER_OPEN
      end,
      wxTreeCtrl:setItemImage(Tree, Item, Idx),
      
      {_, FilePath} = wxTreeCtrl:getItemData(Tree, Item),
      insert(Tree, Item, FilePath),
      alternate_background_all(Tree);
    false -> ok
  end,
	{noreply, State};
handle_event(#wx{obj=Tree, event=#wxTree{type=command_tree_item_collapsed, item=Item}}, State) ->
  % case is_fixed_header(Tree, Item) of
  case is_selectable(Tree, Item) of
    true ->
      Image = wxTreeCtrl:getItemImage(Tree, Item),
      Idx = case Image of
        ?ICON_FOLDER_OPEN -> ?ICON_FOLDER;
        ?ICON_PROJECT_OPEN -> ?ICON_PROJECT
      end,
      wxTreeCtrl:setItemImage(Tree, Item, Idx),
      wxTreeCtrl:deleteChildren(Tree, Item),
      alternate_background_all(Tree);
    false -> ok
  end,
	{noreply, State};
handle_event(#wx{obj=Tree, event=#wxTree{type=command_tree_sel_changed, item=Item, itemOld=OldItem}},
						 State) ->
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
  % case is_fixed_header(Tree, Item) of
  case is_selectable(Tree, Item) of
    true ->
      toggle_or_open(Tree, Item);
    false ->
      wxTreeCtrl:toggle(Tree, Item)
  end,
	{noreply, State}.
  
handle_sync_event(#wx{obj=Tree}, Event, _State) ->
  Item = wxTreeEvent:getItem(Event),
  % case is_fixed_header(Tree, Item) of
  case is_selectable(Tree, Item) of
    true ->
      ok;
    false ->
      wxTreeEvent:veto(Event)
  end.

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
%% @doc Alternate the background colour for each section.

alternate_background_all(Tree) ->
  alternate_background_of_children(Tree, get_projects_root(Tree)),
  alternate_background_of_children(Tree, get_standalone_root(Tree)).


%% =====================================================================
%% @doc Set the background colour for all visible children of Item.
%% Includes those items that are currently scrolled out of view.

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

get_item_from_path(_Tree, [], _Path) -> no_item;
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
			Child = append_item(Tree, Item, FileName, [{data, {Id, File}}]),
      wxTreeCtrl:setItemImage(Tree, Child, ?ICON_FOLDER),
      check_dir_has_contents(Tree, Child, File);
		_ ->
      Child = append_item(Tree, Item, FileName, [{data, {Id, File}}]),
      wxTreeCtrl:setItemImage(Tree, Child, ?ICON_DOCUMENT)
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
%% @doc Add Item to Tree.

append_item(Tree, Item, Filename) ->
  append_item(Tree, Item, Filename, []).
  
append_item(Tree, Item, Filename, Data) ->
  Itm = wxTreeCtrl:appendItem(Tree, Item, Filename, Data),
  case os:type() of
    {_,darwin} ->
      Font = wxTreeCtrl:getItemFont(Tree, Itm),
      wxFont:setPointSize(Font, 12),
      wxTreeCtrl:setItemFont(Tree, Itm, Font);
    _ -> ok
  end,
  Itm.

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
 
% get_projects_root(Tree) ->
%   {Item, _} = wxTreeCtrl:getFirstChild(Tree, wxTreeCtrl:getRootItem(Tree)),
%   Item.
get_projects_root(Tree) ->
  get_header(Tree, ?HEADER_PROJECTS).

%% get_header CAN BE CALLED DIRECTLY
%% =====================================================================
%% @doc

% get_standalone_root(Tree) ->
%   wxTreeCtrl:getNextSibling(Tree, get_projects_root(Tree)).
get_standalone_root(Tree) ->
  get_header(Tree, ?HEADER_FILES).
  

%% NEW NEW NEW NEW
%% =====================================================================
%% @doc Find the header with the id Id
%% All headers will be a first child of the (hidden) root.

get_header(Tree, HeaderId) ->
  {Item, _} = wxTreeCtrl:getFirstChild(Tree, wxTreeCtrl:getRootItem(Tree)),
  get_sibling(Tree, Item, HeaderId).
  
%% THIS IS THE SAME AS find_standalone/3
%% WILL loop forever if the item does not exist
get_sibling(Tree, Item, Data) ->
  case wxTreeCtrl:getItemData(Tree, Item) of
    Data -> Item;
    _ -> 
      get_sibling(Tree, wxTreeCtrl:getNextSibling(Tree, Item), Data)
  end.
  
is_selectable(Tree, Item) ->
  Bool = is_projects_root(Tree, Item) orelse
  is_standalone_root(Tree, Item) orelse
  is_placeholder(Tree, Item),
  not Bool.
  
%% A placeholder has unique client data
is_placeholder(Tree, Item) ->
  case wxTreeCtrl:getItemData(Tree, Item) of
    placeholder -> true;
    _ -> false
  end.
%% =====================================================================
%% @doc
  
% is_fixed_header(Tree, Item) ->
%   is_projects_root(Tree, Item) orelse 
%   is_standalone_root(Tree, Item).


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


%% =====================================================================
%% @doc The placeholder will always be the first child of the header.
%% A placeholder has a unique item data. Item should always be a header.

remove_placeholder(Tree, Item) ->
  % Sibling = wxTreeCtrl:getNextSibling(Tree, Item),
  {Child, _} = wxTreeCtrl:getFirstChild(Tree, Item),
  case wxTreeCtrl:getItemData(Tree, Child) of
    placeholder ->
      wxTreeCtrl:delete(Tree, Child);
    _ -> ok
  end.

%% If the next sibling is a header, then the branch must be empty, so
%% display a placeholder.
%% Item should always be a header.
insert_placeholder(Tree, Item, Msg) ->
  Sibling = wxTreeCtrl:getNextSibling(Tree, Item),
  case wxTreeCtrl:getItemData(Tree, Sibling) of
    N when (N =:= ?HEADER_FILES) or (N =:= ?HEADER_PROJECTS) ->
      Placeholder = append_item(Tree, Item, Msg, [{data, placeholder}]),
      wxTreeCtrl:setItemImage(Tree, Placeholder, ?ICON_INFO);
    _ -> ok
  end.
