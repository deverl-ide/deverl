-module(ide_side_bar).

-include_lib("wx/include/wx.hrl").
-include("../include/ide.hrl").

-behaviour(wx_object).
-export([new/1,
		 init/1, 
		 terminate/2, 
		 code_change/3, 
		 handle_info/2, 
		 handle_call/3, 
		 handle_cast/2, 
		 handle_event/2]).
		 
-record(state, {win, wx_env}).

-define(FOLDER_IMAGE,  0).
-define(FILE_IMAGE, 1).
		 
new(Config) ->
	wx_object:start_link({local, ?MODULE}, ?MODULE, Config, []).
	
init(Config) ->
	Toolbook = make_toolbook(Config),
	{Toolbook, #state{win=Toolbook, wx_env=wx:get_env()}}. %% Maintained at server
	
	
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
    
handle_call(toolbook, _From, State) ->
    {reply,State#state.win,State};
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
			ide:add_editor_with_contents(File, Filename, binary_to_list(FileContents)),
			io:format(Text++"~n"++File++"~n")
	end,
	{noreply, State};
handle_event(_Event, State) ->
    io:format("SIDE BAR EVENT CA~n"),
    {noreply, State}.
    
code_change(_, _, State) ->
	{stop, not_yet_implemented, State}.
	
terminate(_Reason, #state{win=Toolbook}) ->
	io:format("TERMINATE SIDEBAR~n"),
	wxToolbook:destroy(Toolbook).





%% =====================================================================
%% @doc Construct the sidebar's wxToolbook.
	
make_toolbook(Config) ->
	ImgList = wxImageList:new(24,24),
	wxImageList:add(ImgList, wxBitmap:new(wxImage:new("../icons/document-new.png"))),
	wxImageList:add(ImgList, wxBitmap:new(wxImage:new("../icons/document-open.png"))),
	wxImageList:add(ImgList, wxBitmap:new(wxImage:new("../icons/document-new.png"))),

	Toolbook = wxToolbook:new(Config, ?wxID_ANY),
	wxToolbook:assignImageList(Toolbook, ImgList),

	ProjectsPanel = wxPanel:new(Toolbook),
	ProjectsSizer = wxBoxSizer:new(?wxVERTICAL),
	ProjectTree = make_tree(ProjectsPanel),
	wxSizer:add(ProjectsSizer, ProjectTree, [{flag, ?wxEXPAND}, {proportion, 1}]),
	wxPanel:setSizer(ProjectsPanel, ProjectsSizer),   

	wxToolbook:addPage(Toolbook, ProjectsPanel, "Projects", [{imageId, 2}]),

	TestPanel = wxPanel:new(Toolbook),
	% TestSizer = wxBoxSizer:new(?wxVERTICAL),
	% TestWindow = wxWindow:new(TestPanel, 2222),
	%wxWindow:setBackgroundColour(W1, {123,34,1}),
	% wxSizer:add(TestSizer, TestWindow, [{flag, ?wxEXPAND}, {proportion, 1}]),
	% wxPanel:setSizer(TestPanel, TestSizer),    
	wxToolbook:addPage(Toolbook, TestPanel, "Tests", [{imageId, 2}]),


	FunctionsPanel = func_list:start([{parent, Toolbook}]),
	% FunctionSizer = wxBoxSizer:new(?wxVERTICAL),
	% FunctionWindow = wxWindow:new(FunctionsPanel, 2222),
	% wxPanel:setSizer(FunctionsPanel, FunctionSizer),    
	% wxSizer:add(FunctionSizer, FunctionWindow, [{flag, ?wxEXPAND}, {proportion, 1}]),
	wxToolbook:addPage(Toolbook, FunctionsPanel, "Functions", [{imageId, 2}]),

	wxToolbook:setSelection(Toolbook, 0), %% Default to projects

	wxTreeCtrl:connect(ProjectTree, command_tree_item_activated, []),

	Toolbook.
	

%% =====================================================================
%% @doc Make a directory tree from project directory. Gets root from 
%% user preferences.

-spec make_tree(Parent) -> Result when
	Parent :: wxPanel:wxPanel(),
	Result :: wxTreeCtrl:wxTreeCtrl().

make_tree(Parent) ->
	ProjectDir = user_prefs:get_user_pref({pref, project_dir}),
    Tree = wxTreeCtrl:new(Parent, [{style, ?wxTR_HAS_BUTTONS bor 
                                           ?wxTR_HIDE_ROOT}]),                                 
    ImgList = wxImageList:new(24,24),
	wxImageList:add(ImgList, wxArtProvider:getBitmap("wxART_FOLDER")),
	wxImageList:add(ImgList, wxArtProvider:getBitmap("wxART_NORMAL_FILE")),
	wxTreeCtrl:assignImageList(Tree, ImgList),                                       
                                                                               
    Root = wxTreeCtrl:addRoot(Tree, ProjectDir),
    build_tree(Tree, Root, ProjectDir),
	Tree.

	
%% =====================================================================
%% @doc Get a list of files in a given root directory then build its
%% subdirectories.

build_tree(Tree, Root, Dir) ->
	Files = filelib:wildcard(Dir ++ "/*"),
	add_files(Tree, Root, Files).
	
%% =====================================================================
%% @doc Add files to the given directory.

add_files(_, _, []) ->
	ok;
add_files(Tree, Root, [File|Files]) ->
	FileName = filename:basename(File),
	Child = wxTreeCtrl:appendItem(Tree, Root, FileName, [{data, File}]), 
	IsDir = filelib:is_dir(File),
	case IsDir of
		true ->
			wxTreeCtrl:setItemImage(Tree, Child, ?FOLDER_IMAGE),
			build_tree(Tree, Child, File);
		_ ->
			wxTreeCtrl:setItemImage(Tree, Child, ?FILE_IMAGE)
	end,
	add_files(Tree, Root, Files).