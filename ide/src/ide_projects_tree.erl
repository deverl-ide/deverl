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
				get_open_projects/0]).

-record(state, {frame, sizer, panel, tree, placeholder}).

start(Config) ->
	wx_object:start_link({local, ?MODULE}, ?MODULE, Config, []).

init(Config) ->
	Parent = proplists:get_value(parent, Config),
	Frame = proplists:get_value(frame, Config),
	
	Panel = wxPanel:new(Parent),
	wxPanel:setBackgroundColour(Panel, {110,110,110}),
	MainSz = wxBoxSizer:new(?wxVERTICAL),
	wxPanel:setSizer(Panel, MainSz),
	
	%% Placeholder (Ph) "No Open Projects"
	Placeholder = wxPanel:new(Panel),
	
	PhSz = wxBoxSizer:new(?wxVERTICAL),
	
	HSz = wxBoxSizer:new(?wxHORIZONTAL),
	T = wxStaticText:new(Placeholder, ?wxID_ANY, "No Open Projects", []),
	wxSizer:addStretchSpacer(HSz),
	wxSizer:add(HSz, T, [{proportion, 1}, {flag, ?wxEXPAND}]),
	wxSizer:addStretchSpacer(HSz),
	% wxSizer:add(PhSz, HSz, [{proportion, 1}, {flag, ?wxEXPAND}]),
	
	wxStaticText:setForegroundColour(T, {200,200,200}),
	wxSizer:addStretchSpacer(PhSz),
	wxSizer:add(PhSz, HSz, [{proportion, 1}, {flag, ?wxEXPAND}]),
	wxSizer:addStretchSpacer(PhSz),
	wxPanel:setSizer(Placeholder, PhSz),
	wxSizer:fit(PhSz, Placeholder),
	wxSizer:layout(PhSz),
		
	wxSizer:add(MainSz, Placeholder, [{proportion, 1}, {flag, ?wxEXPAND}]),
	
		
  Tree = wxTreeCtrl:new(Panel, [{style, ?wxTR_HAS_BUTTONS bor
                                        ?wxTR_HIDE_ROOT bor
                                        ?wxTR_FULL_ROW_HIGHLIGHT}]),
	wxTreeCtrl:setBackgroundColour(Tree, {210,210,210}),
	ImgList = wxImageList:new(24,24),
	wxImageList:add(ImgList, wxArtProvider:getBitmap("wxART_FOLDER", [{client,"wxART_MENU"}])),
	wxImageList:add(ImgList, wxArtProvider:getBitmap("wxART_NORMAL_FILE", [{client,"wxART_MENU"}])),
	wxImageList:add(ImgList, wxArtProvider:getBitmap("wxART_HELP_BOOK", [{client,"wxART_MENU"}])),
	wxTreeCtrl:assignImageList(Tree, ImgList),
		
  wxTreeCtrl:addRoot(Tree, "ProjectTreeRoot"),
	wxSizer:add(MainSz, Tree, [{proportion, 1}, {flag, ?wxEXPAND}]),
	
	hide_tree(MainSz, Tree),
	
  wxTreeCtrl:connect(Tree, command_tree_item_activated, []),
	wxTreeCtrl:connect(Tree, command_tree_sel_changed, []),
	
	{Panel, #state{frame=Frame, sizer=MainSz, panel=Panel, tree=Tree, placeholder=Placeholder}}.
	

%% =====================================================================
%% OTP callbacks
%%
%% =====================================================================

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
	wxPanel:thaw(Panel),
  {noreply,State};
handle_cast({add, Dir}, State=#state{sizer=Sz, tree=Tree}) ->
	case wxWindow:isShown(Tree) of
		false -> 
			show_tree(Sz);
		true -> ok
	end,
	Root = wxTreeCtrl:getRootItem(Tree),
	Item = wxTreeCtrl:appendItem(Tree, Root, filename:basename(Dir), [{data, Dir}]),
	wxTreeCtrl:setItemImage(Tree, Item, 2),
	build_tree(Tree, Item, Dir),
  {noreply,State};
handle_cast(Msg, State) ->
  io:format("Got cast ~p~n",[Msg]),
  {noreply,State}.

handle_call(tree, _From, State) ->
  {reply,State#state.tree,State}.

handle_event(#wx{obj=Tree, event=#wxTree{type=command_tree_sel_changed, item=Item, itemOld=OldItem}}, 
						 State=#state{frame=Frame, tree=Tree}) ->
	case wxTreeCtrl:isTreeItemIdOk(OldItem) of
		false ->  %% Deleted item
			ok;
		true ->
			ProjRoot = get_project_root(Tree, Item),
			Data = wxTreeCtrl:getItemData(Tree, ProjRoot),
			ProjName = filename:basename(Data),
			ide:set_title(ProjName),
			ide_menu:update_label(wxFrame:getMenuBar(Frame), ?MENU_ID_CLOSE_PROJECT, "Close Project (" ++ ProjName ++ ")"),
			doc_manager:set_active_project({ProjRoot, Data})
	end,
			
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
					FileContents, [{project,{get_project_root(Tree, Item), 
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
%% @doc Add a project directory to the tree.

add_project(Dir) ->
	Tree = wx_object:cast(?MODULE, {add, Dir}).
	
	
%% =====================================================================
%% @doc Get a list of files in a given root directory then build its
%% subdirectories.

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
			wxTreeCtrl:setItemImage(Tree, Child, 0),
			build_tree(Tree, Child, File);
		_ ->
			Child = wxTreeCtrl:prependItem(Tree, Root, FileName, [{data, File}]),
			wxTreeCtrl:setItemImage(Tree, Child, 1)
	end,
	add_files(Tree, Root, Files).


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
%% @doc Delete an item from the tree

delete_project(Id) ->
	wx_object:cast(?MODULE, {delete, Id}).


%% =====================================================================
%% @doc Print the tree

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

show_tree(Sz) ->
	wxSizer:hide(Sz, 0),
	wxSizer:show(Sz, 1),
	wxSizer:layout(Sz).
	
show_placeholder(Sz) ->
	wxSizer:hide(Sz, 1),
	wxSizer:show(Sz, 0),
	wxSizer:layout(Sz).
	
hide_tree(Sz, Tree) ->
	wxSizer:hide(Sz, Tree),
	wxSizer:layout(Sz).
	
	
get_open_projects() ->
	Tree = wx_object:call(?MODULE, tree),
	{Fc,_} = wxTreeCtrl:getFirstChild(Tree, wxTreeCtrl:getRootItem(Tree)),
	get_open_projects(Tree, Fc, []).
	
get_open_projects(Tree, Item, Acc) ->
	case wxTreeCtrl:isTreeItemIdOk(Item) of
		true -> 
			get_open_projects(Tree, wxTreeCtrl:getNextSibling(Tree, Item), 
				[{Item,wxTreeCtrl:getItemData(Tree, Item)} | Acc]);
		false -> Acc
	end.