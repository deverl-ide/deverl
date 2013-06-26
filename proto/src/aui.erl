%% AUI Manager
%% aui.erl

-module(aui).

-include_lib("wx/include/wx.hrl").
-behaviour(wx_object).

%% wx_objects callbacks
-export([start/0, init/1, terminate/2,  code_change/3,
         handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).

%% Client API         
-export([add_editor/0]).

%% The record containing the State.
-record(state, {win,  
                env,                  %% The wx environment
                workspace,            %% Notebook
                workspace_manager,    %% Tabbed UI manager for editors
                perspective,          %% The AUI user perspective
                editors :: list()     %% The current open editors
                }).

-define(DEFAULT_FRAME_WIDTH,  1300).
-define(DEFAULT_FRAME_HEIGHT, 731).
-define(DEFAULT_UTIL_HEIGHT,  200).
-define(DEFAULT_TEST_WIDTH,   200).

start() ->
  start([]).
  
start(Args) ->
  wx_object:start({local, ?MODULE}, ?MODULE, Args, [{debug, [log]}]).
  %% Trap the error {error,{already_started,Pid()}} to prevent the app from 
  %% being opened twice.
    
start_link() ->
  start_link([]).
  
start_link(Args) ->
  wx_object:start_link(?MODULE, Args, []).

%% init(Args) should return:
%% {wxObject, State} | {wxObject, State, Timeout} | ignore | {stop, Reason}
init(Options) ->
  wx:new(Options),

  Frame = wxFrame:new(wx:null(), ?wxID_ANY, "Erlang IDE", [{size,{?DEFAULT_FRAME_WIDTH,?DEFAULT_FRAME_HEIGHT}}]),
  wxFrame:connect(Frame, close_window),
  Env = wx:get_env(), %% The wx environment
    
  ide_menubar:new(Frame),
  ide_toolbar:new(Frame),
  
  UI = wxPanel:new(Frame, []),
  Sizer = wxBoxSizer:new(?wxVERTICAL),
  
  Manager = wxAuiManager:new([{managed_wnd, UI}]),
  
  %% PaneInfo - default (common) pane behaviour
  PaneInfo = wxAuiPaneInfo:new(),
  wxAuiPaneInfo:closeButton(PaneInfo, [{visible, false}]),
  wxAuiPaneInfo:floatable(PaneInfo, [{b, false}]),
  wxAuiPaneInfo:captionVisible(PaneInfo, [{visible, false}]),
    
  %% The left pane/test window
  TestWindow = wxPanel:new(UI),
  TestWindowPaneInfo = wxAuiPaneInfo:left(wxAuiPaneInfo:new(PaneInfo)),
  wxAuiPaneInfo:minSize(TestWindowPaneInfo, {200,0}),
  wxAuiPaneInfo:bestSize(TestWindowPaneInfo, {200,0}),
  wxAuiManager:addPane(Manager, TestWindow, TestWindowPaneInfo),
  
  TestSizer = wxBoxSizer:new(?wxVERTICAL),
  TestT = wxTextCtrl:new(TestWindow, 8001, [{style, ?wxTE_MULTILINE}]), 
  wxSizer:add(TestSizer, TestT, [{flag, ?wxEXPAND},
                                 {proportion, 1}]),
  wxPanel:setSizer(TestWindow, TestSizer),
  
  %% The centre pane/editor window
  EditorWindow = wxPanel:new(UI),
  EditorWindowPaneInfo = wxAuiPaneInfo:new(PaneInfo),
  wxAuiPaneInfo:centrePane(EditorWindowPaneInfo), 
  Workspace = create_editor(UI, Manager, EditorWindowPaneInfo, Env, "new_file"),
  
  %% The bottom pane/utility window
  BottomPaneInfo = wxAuiPaneInfo:bottom(wxAuiPaneInfo:new(PaneInfo)),
  wxAuiPaneInfo:minSize(BottomPaneInfo, {0,200}),
  wxAuiPaneInfo:bestSize(BottomPaneInfo, {0, 200}),
  create_utils(UI, Manager, BottomPaneInfo),

  wxPanel:setSizer(UI, Sizer),
  
  wxAuiManager:connect(Manager, aui_pane_maximize, [{skip,true}]),
  wxAuiManager:connect(Manager, aui_pane_restore, [{skip,true}]),    
  wxAuiManager:connect(Manager, aui_render, [{skip,true}]),    
  wxAuiManager:update(Manager),
    
  wxFrame:show(Frame),
  
  process_flag(trap_exit, true),

  State = #state{win=Frame},
  {Frame, State#state{workspace=Workspace, 
                      workspace_manager=Manager,
                      env=Env}}.

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
    {reply, {State#state.workspace, 
             State#state.workspace_manager,
             State#state.env}, State};
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
handle_event(#wx{event = #wxAuiManager{type = aui_pane_maximize} = E}, State) ->
    io:format("maximize"),
    Manager = E#wxAuiManager.manager,
    Perspective = wxAuiManager:savePerspective(Manager),
    io:format("Perspective: ~p~n", [Perspective]),
    {noreply, State#state{perspective = Perspective}};
handle_event(#wx{event = #wxAuiManager{type = aui_pane_restore, manager = Man}}, State) ->
    io:format("minimize"),
    wxAuiManager:loadPerspective(Man, State#state.perspective),
    {noreply, State};
handle_event(W = #wx{event = #wxAuiManager{type = aui_render} = E}, State) ->
    % io:format("render:~n event:~p~n", [E]),   
    {noreply, State};
%% AuiNotebook events
handle_event(#wx{obj = Workspace,
		 event = #wxAuiNotebook{type = command_auinotebook_page_changed,
					selection = Sel}}, State) ->
    io:format("changed page~n"),
    {noreply, State};
handle_event(#wx{event=#wxAuiNotebook{type=command_auinotebook_bg_dclick}}, State) ->
    add_editor(State#state.workspace, State#state.workspace_manager, State#state.env),
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
    io:format("aui terminate~n"),
    wx:destroy().
    
create_utils(Parent, Manager, Pane) ->
  %% Notebook styles
  Style = (0
     bor ?wxAUI_NB_TOP
     bor ?wxAUI_NB_WINDOWLIST_BUTTON
     bor ?wxAUI_NB_TAB_MOVE
     bor ?wxAUI_NB_SCROLL_BUTTONS
     bor ?wxAUI_NB_CLOSE_ON_ALL_TABS
    ),
  
  Utils = wxAuiNotebook:new(Parent, [{style, Style}]),

  Console = wxPanel:new(Utils, []),
  wxAuiNotebook:addPage(Utils, Console, "Console", []),

  Pman = wxPanel:new(Utils, []),
  wxAuiNotebook:addPage(Utils, Pman, "Process Manager", []),

  Dialyser = wxPanel:new(Utils, []),
  wxAuiNotebook:addPage(Utils, Dialyser, "Dialyser", []),
  
  Debugger = wxPanel:new(Utils, []),
  wxAuiNotebook:addPage(Utils, Debugger, "Debugger", []),

  wxAuiManager:addPane(Manager, Utils, Pane),
  Utils.
  
create_editor(Parent, Manager, Pane, Env, Filename) ->
  Style = (0
     bor ?wxAUI_NB_TOP
     bor ?wxAUI_NB_WINDOWLIST_BUTTON
     bor ?wxAUI_NB_TAB_MOVE
     bor ?wxAUI_NB_SCROLL_BUTTONS
     bor ?wxAUI_NB_CLOSE_ON_ALL_TABS
    ),
    
  Workspace = wxAuiNotebook:new(Parent, [{style, Style}]),
  Editor = editor:start([{parent, Workspace}, {env, Env}]),
  wxAuiNotebook:addPage(Workspace, Editor, Filename, []),
  wxAuiManager:addPane(Manager, Workspace, Pane),
  
  wxAuiNotebook:connect(Workspace, command_auinotebook_bg_dclick, []),
  wxAuiNotebook:connect(Workspace, command_auinotebook_page_close, [{skip, true}]),
  wxAuiNotebook:connect(Workspace, command_auinotebook_page_changed), 
  Workspace.
  
%% @doc To be called from external modules, calls wx server to obtain required state
add_editor() -> 
  {Workspace, Manager, Env} = wx_object:call(?MODULE, workspace),
  
  Editor = editor:start([{parent, Workspace}, {env, Env}]),
  % Editor = wxPanel:new(Workspace), %% Trying to discover why it crashes
  
  wx:set_env(Env),
  add_editor(Workspace, Manager, Env),
  Workspace.
%% @doc Called internally
add_editor(Workspace, Manager, Env) ->
  
  Editor = editor:start([{parent, Workspace}, {env, Env}]),
  % Editor = wxPanel:new(Workspace), %% Trying to discover why it crashes
  
  wxAuiNotebook:addPage(Workspace, Editor, "new_file", [{select, true}]),
  Pane = wxAuiPaneInfo:new(),
  wxAuiPaneInfo:defaultPane(Pane),
  wxAuiManager:addPane(Manager, Workspace, Pane),
  Workspace.
add_editor(FileName) -> ok.
