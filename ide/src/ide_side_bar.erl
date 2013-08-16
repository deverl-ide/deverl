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
  
	P1 = wxPanel:new(Toolbook),
	Sz = wxBoxSizer:new(?wxVERTICAL),
	wxPanel:setSizer(P1, Sz),    
	Tree = wxGenericDirCtrl:new(P1, [{dir, "/usr"}, 
                                     {style, ?wxDIRCTRL_SHOW_FILTERS}]),
	wxSizer:add(Sz, Tree, [{flag, ?wxEXPAND}, {proportion, 1}]),
	wxToolbook:addPage(Toolbook, P1, "Files", [{bSelect, true}, {imageId, 1}]),
  
	P2 = wxPanel:new(Toolbook),
	Sz2 = wxBoxSizer:new(?wxVERTICAL),
	W1 = wxWindow:new(P2, 987),
	%wxWindow:setBackgroundColour(W1, {123,34,1}),
	wxPanel:setSizer(P2, Sz2),    
	wxSizer:add(Sz2, W1, [{flag, ?wxEXPAND}, {proportion, 1}]),
	wxToolbook:addPage(Toolbook, P2, "Tests", [{bSelect, true}, {imageId, 2}]),
	
	P3 = wxPanel:new(Toolbook),
	Sz3 = wxBoxSizer:new(?wxVERTICAL),
	wxPanel:setSizer(P3, Sz3),   
    ProjectTree = make_tree(P3),
	wxSizer:add(Sz3, ProjectTree, [{flag, ?wxEXPAND}, {proportion, 1}]),
	wxToolbook:addPage(Toolbook, P3, "Projects", [{bSelect, true}, {imageId, 2}]),
  
	wxToolbook:setSelection(Toolbook, 2), %% Default to projects
	
	Toolbook.
	

%% =====================================================================
%% @doc Make a directory tree from project directory. Gets root from 
%% user preferences.

-spec make_tree(Parent) -> Result when
	Parent :: wxPanel:wxPanel(),
	Result :: wxTreeCtrl:wxTreeCtrl().

make_tree(Parent) ->
	ProjectDir = user_prefs:get_user_pref({pref, project_dir}),
    Tree = wxTreeCtrl:new(Parent, [{style, ?wxTR_HAS_BUTTONS bor ?wxTR_HIDE_ROOT}]),
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
	%io:format(wxTreeCtrl:getItemData(Tree, Child) ++ "~n"),
	IsDir    = filelib:is_dir(File),
	if
		 IsDir ->
			build_tree(Tree, Child, File);
		 true ->
			ok
	end,
	add_files(Tree, Root, Files).
	

	
	
	
	
	
	
	
