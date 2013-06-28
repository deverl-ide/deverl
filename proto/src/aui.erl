%% AUI Manager
%% aui.erl

-module(aui).

-include_lib("wx/include/wx.hrl").
-behaviour(wx_object).

%% wx_objects callbacks
-export([start/0, init/1, terminate/2,  code_change/3,
         handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).

%% Client API         
-export([add_editor/0, add_editor/2, show_hide/1, show_hide/2]).

%% The record containing the State.
-record(state, {win,  
                env,                  %% The wx environment
                workspace,            %% Notebook
                workspace_manager,    %% Tabbed UI manager for editors
                perspective           %% The AUI user perspective
                }).

-define(DEFAULT_FRAME_WIDTH,  1300).
-define(DEFAULT_FRAME_HEIGHT, 731).
-define(DEFAULT_UTIL_HEIGHT,  200).
-define(DEFAULT_TEST_WIDTH,   200).

-define(DEFAULT_TAB_LABEL, "new_file").

start() ->
  start([]).
  
start(Args) ->
  wx_object:start({local, ?MODULE}, ?MODULE, Args, [{debug, [log]}]).
  %% Trap the error {error,{already_started,Pid()}} to prevent the app from 
  %% being opened twice.

init(Options) ->
  wx:new(Options),

  Frame = wxFrame:new(wx:null(), ?wxID_ANY, "Erlang IDE", [{size,{?DEFAULT_FRAME_WIDTH,?DEFAULT_FRAME_HEIGHT}}]),
  wxFrame:connect(Frame, close_window),
  
  menu_toolbar:new(Frame),
        
  UI = wxPanel:new(Frame, []),
  % UiSizer = wxBoxSizer:new(?wxVERTICAL),
  % wxPanel:setSizer(UI, UiSi zer),

  Manager = wxAuiManager:new([{managed_wnd, UI}, {flags, ?wxAUI_MGR_RECTANGLE_HINT bor 
                                                         ?wxAUI_MGR_TRANSPARENT_DRAG}]),
                                                         
  % wxSizer:add(UiSizer, Manager, [{flag, ?wxEXPAND},
  %                           {proportion, 1}]),
  
  %% PaneInfo - default (common) pane behaviour
  PaneInfo = wxAuiPaneInfo:new(),
  wxAuiPaneInfo:closeButton(PaneInfo, [{visible, false}]),
  wxAuiPaneInfo:floatable(PaneInfo, [{b, false}]),
  wxAuiPaneInfo:captionVisible(PaneInfo, [{visible, false}]),
    
  %% The centre pane/editor window
  EditorWindowPaneInfo = wxAuiPaneInfo:centrePane(PaneInfo), 
  wxAuiPaneInfo:name(EditorWindowPaneInfo, "EditorPane"),
  Workspace = create_editor(UI, Manager, EditorWindowPaneInfo, ?DEFAULT_TAB_LABEL),
  
  %% The left pane/test window
  TestWindow = wxPanel:new(UI),
  TestWindowPaneInfo = wxAuiPaneInfo:left(wxAuiPaneInfo:new(PaneInfo)),
  wxAuiPaneInfo:name(TestWindowPaneInfo, "TestPane"),
  wxAuiPaneInfo:minSize(TestWindowPaneInfo, {200,0}),
  wxAuiPaneInfo:bestSize(TestWindowPaneInfo, {200,0}),
  wxAuiManager:addPane(Manager, TestWindow, TestWindowPaneInfo),
  
  TestSizer = wxBoxSizer:new(?wxVERTICAL),
  TestT = wxTextCtrl:new(TestWindow, 8001, [{style, ?wxTE_MULTILINE}]), 
  wxSizer:add(TestSizer, TestT, [{flag, ?wxEXPAND},
                                 {proportion, 1}]),
  wxPanel:setSizer(TestWindow, TestSizer),
  
  %% The bottom pane/utility window
  BottomPaneInfo = wxAuiPaneInfo:bottom(wxAuiPaneInfo:new(PaneInfo)),
  wxAuiPaneInfo:name(BottomPaneInfo, "UtilPane"),
  wxAuiPaneInfo:minSize(BottomPaneInfo, {0,200}),
  wxAuiPaneInfo:bestSize(BottomPaneInfo, {0, 200}),
  create_utils(UI, Manager, BottomPaneInfo),
  
  % wxAuiManager:connect(Manager, aui_pane_maximize, [{skip,true}]),
  % wxAuiManager:connect(Manager, aui_pane_button, [{skip,true}]),
  % wxAuiManager:connect(Manager, aui_pane_restore, [{skip,true}]),    
  % wxAuiManager:connect(Manager, aui_render, [{skip,true}]),    
  wxAuiManager:update(Manager),
  
  %% Custom status bar
  % StatusBar = wxPanel:new(UI, []),
  % SbSizer = wxBoxSizer:new(?wxHORIZONTAL),
  % wxPanel:setSizer(StatusBar, SbSizer),
  % 
  % wxSizer:add(UiSizer, StatusBar, [{flag, ?wxEXPAND},
  %                                {proportion, 1}]),
    
  wxFrame:show(Frame),
  
  process_flag(trap_exit, true),

  State = #state{win=Frame},
  {Frame, State#state{workspace=Workspace, 
                      workspace_manager=Manager}}.

%%%%%%%%%%%%%%%%%%%%%
%%%%% Callbacks %%%%%
handle_info({'EXIT',_, wx_deleted}, State) ->
    io:format("Got Info 1~n"),
    {noreply,State};
handle_info({'EXIT',_, shutdown}, State) ->
    io:format("Got Info 2~n"),
    {noreply,State};
handle_info({'EXIT',A, normal}, State) ->
    io:format("Got Info 3~n~p~n", [A]),
    {noreply,State};
handle_info(Msg, State) ->
    io:format("Got Info ~p~n",[Msg]),
    {noreply,State}.

handle_call(shutdown, _From, State=#state{win=Panel, workspace_manager=Manager}) ->
    wxAuiManager:unInit(Manager),
    wxAuiManager:destroy(Manager),
    wxPanel:destroy(Panel),
    io:format("Right here..~n"),
    {stop, normal, ok, State};
%% @doc Return the workspace
handle_call(workspace, _From, State) ->
    {reply, {State#state.workspace}, State};
handle_call(Msg, _From, State) ->
    demo:format(State#state{}, "Got Call ~p\n", [Msg]),
    {reply,{error, nyi}, State}.

handle_cast(Msg, State) ->
    io:format("Got cast ~p~n",[Msg]),
    {noreply,State}.
    
%% Window close event
handle_event(#wx{event=#wxClose{}}, State = #state{win=Frame}) ->
    io:format("~p Closing window ~n",[self()]),
    ok = wxFrame:setStatusText(Frame, "Closing...",[]),
    {stop, normal, State};
%% AuiManager events
handle_event(_E=#wx{event = #wxAuiManager{type = aui_pane_button} = _R}, State) ->
    io:format("button"),
    io:format("E: ~p~nR:~p~n",[_E, _R]),
    Manager = _R#wxAuiManager.manager,
    Perspective = wxAuiManager:savePerspective(Manager),
    % io:format("Perspective: ~p~n", [Perspective]),
    {noreply, State#state{perspective=Perspective}};
handle_event(#wx{event = #wxAuiManager{type = aui_pane_maximize} = E}, State) ->
    io:format("maximize"),
    Manager = E#wxAuiManager.manager,
    Perspective = wxAuiManager:savePerspective(Manager),
    io:format("Perspective: ~p~n", [Perspective]),
    {noreply, State};
handle_event(#wx{event = #wxAuiManager{type = aui_pane_restore, manager = Man}}, State) ->
    io:format("minimize"),
    wxAuiManager:loadPerspective(Man, State#state.perspective, [{update, false}]),
    % wxAuiManager:loadPaneInfo(Man, State#state.perspective, [{update, false}]),
    {noreply, State};
handle_event(_W = #wx{event = #wxAuiManager{type = aui_render} = _E}, State) ->
    io:format("render:~n"),   
    {noreply, State};
%% AuiNotebook events
handle_event(#wx{obj = _Workspace,
		 event = #wxAuiNotebook{type = command_auinotebook_page_changed,
					selection = _Sel}}, State) ->
    io:format("changed page~n"),
    {noreply, State};
handle_event(#wx{event=#wxAuiNotebook{type=command_auinotebook_bg_dclick}}, State) ->
    add_editor(State#state.workspace),
    {noreply, State};
handle_event(#wx{event = #wxAuiNotebook{type = command_auinotebook_page_close}}, State) ->
    io:fwrite("page closed~n"),
    % editor:stop(),
    {noreply, State};
%% Event catchall for testing
handle_event(Ev = #wx{}, State) ->
    io:format("aui event catchall: ~p\n", [Ev]),
    {noreply, State}.

code_change(_, _, State) ->
    {stop, not_yet_implemented, State}.

terminate(_Reason, _State) ->
    io:format("aui callback: terminate~n"),
    wx:destroy().

%%%%%%%%%%%%%%%%%%%%%
%%%%% Internals %%%%%

%% @doc Create the utilities panel
%% @private      
create_utils(Parent, Manager, Pane) ->
  %% Notebook styles
  Style = (0
     bor ?wxAUI_NB_TOP
     bor ?wxAUI_NB_WINDOWLIST_BUTTON
     bor ?wxAUI_NB_TAB_MOVE
     bor ?wxAUI_NB_SCROLL_BUTTONS
    ),
  
  Utils = wxAuiNotebook:new(Parent, [{style, Style}]),

  Console = ide_shell:new([{parent, Utils}]),
  wxAuiNotebook:addPage(Utils, Console, "Console", []),

  Pman = wxPanel:new(Utils, []),
  wxAuiNotebook:addPage(Utils, Pman, "Process Manager", []),

  Dialyser = wxPanel:new(Utils, []),
  wxAuiNotebook:addPage(Utils, Dialyser, "Dialyser", []),
  
  Debugger = wxPanel:new(Utils, []),
  wxAuiNotebook:addPage(Utils, Debugger, "Debugger", []),

  wxAuiManager:addPane(Manager, Utils, Pane),
  Utils.

%% @doc Create the workspace with the initial editor
%% @private  
create_editor(Parent, Manager, Pane, Filename) ->
  Style = (0
     bor ?wxAUI_NB_TOP
     bor ?wxAUI_NB_WINDOWLIST_BUTTON
     bor ?wxAUI_NB_TAB_MOVE
     bor ?wxAUI_NB_SCROLL_BUTTONS
     bor ?wxAUI_NB_CLOSE_ON_ALL_TABS
    ),
    
  Workspace = wxAuiNotebook:new(Parent, [{style, Style}]),
  
  Editor = editor:start([{parent, Workspace}]), %% Gets the editor instance inside a wxPanel
  wxAuiNotebook:addPage(Workspace, Editor, Filename, []),
  
  wxAuiManager:addPane(Manager, Workspace, Pane),
  
  wxAuiNotebook:connect(Workspace, command_auinotebook_bg_dclick, []),
  wxAuiNotebook:connect(Workspace, command_auinotebook_page_close, [{skip, true}]),
  wxAuiNotebook:connect(Workspace, command_auinotebook_page_changed), 
  Workspace.
  
%% @doc Called internally
%% @private
add_editor(Workspace) ->
  add_editor(Workspace, ?DEFAULT_TAB_LABEL),
  Workspace.
  
%%%%%%%%%%%%%%%%%%%%%%
%%%%% Client API %%%%%
  
%% @doc Creates a new editor instance in a new tab  
%% @doc To be called from external modules, calls wx server to obtain required state
add_editor() -> 
  {Workspace} = wx_object:call(?MODULE, workspace), 
  add_editor(Workspace),
  Workspace.
%% @doc Create a new editor with specified filename
add_editor(Workspace, FileName) -> 
  Editor = editor:start([{parent, Workspace}]),
  wxAuiNotebook:addPage(Workspace, Editor, FileName, [{select, true}]),
  Workspace.
  
show_hide(PaneType) ->
	{Workspace} = wx_object:call(?MODULE, workspace),
	Manager = wxAuiManager:getManager(Workspace),
	TestPane = wxAuiManager:getPane(Manager, "TestPane"),
	UtilPane = wxAuiManager:getPane(Manager, "UtilPane"),
	case PaneType of
		"test" ->
			show_hide(TestPane, Manager);
		"util" ->
			show_hide(UtilPane, Manager);
		"editor" ->
			show_hide(TestPane, UtilPane, Manager)
	end.
show_hide(Pane, Manager) ->
	IsShown = wxAuiPaneInfo:isShown(Pane),
	case IsShown of
		true ->
			wxAuiPaneInfo:hide(Pane);	
		_    ->
			wxAuiPaneInfo:show(Pane)
	end,
	wxAuiManager:update(Manager).
show_hide(Pane1, Pane2, Manager) ->
	Pane1IsShown = wxAuiPaneInfo:isShown(Pane1),
	Pane2IsShown = wxAuiPaneInfo:isShown(Pane2),
	case Pane1IsShown or Pane2IsShown of
		true ->
			wxAuiPaneInfo:hide(Pane1),
			wxAuiPaneInfo:hide(Pane2);
		_    ->
			wxAuiPaneInfo:show(Pane1),
			wxAuiPaneInfo:show(Pane2)
	end,
	wxAuiManager:update(Manager).
	
