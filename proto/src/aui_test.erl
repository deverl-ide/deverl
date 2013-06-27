%% AUI Manager
%% aui.erl

-module(aui_test).

-include_lib("wx/include/wx.hrl").

-export([init/0]).


%% Client API         

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


init() ->
  wx:new(),
  Env = wx:get_env(), %% The wx environment

  Frame = wxFrame:new(wx:null(), ?wxID_ANY, "Erlang IDE", [{size,{?DEFAULT_FRAME_WIDTH,?DEFAULT_FRAME_HEIGHT}}]),
  wxFrame:connect(Frame, close_window),
    
  menu_toolbar:new(Frame),
  
  UI = wxPanel:new(Frame, []),
  

  Manager = wxAuiManager:new([{managed_wnd, UI}, {flags, ?wxAUI_MGR_RECTANGLE_HINT bor 
                                                         ?wxAUI_MGR_TRANSPARENT_DRAG}]),
  
  %% PaneInfo - default (common) pane behaviour
  PaneInfo = wxAuiPaneInfo:new(),
  wxAuiPaneInfo:closeButton(PaneInfo, [{visible, false}]),
  wxAuiPaneInfo:floatable(PaneInfo, [{b, false}]),
  wxAuiPaneInfo:captionVisible(PaneInfo, [{visible, false}]),
    
  %% The centre pane/editor window
  EditorWindow = wxPanel:new(UI),
  EditorWindowPaneInfo = wxAuiPaneInfo:new(PaneInfo),
  wxAuiPaneInfo:centrePane(EditorWindowPaneInfo), 
  Workspace = create_editor(UI, Manager, EditorWindowPaneInfo, Env, "new_file"),
  
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
  
  %% The bottom pane/utility window
  BottomPaneInfo = wxAuiPaneInfo:bottom(wxAuiPaneInfo:new(PaneInfo)),
  wxAuiPaneInfo:minSize(BottomPaneInfo, {0,200}),
  wxAuiPaneInfo:bestSize(BottomPaneInfo, {0, 200}),
  create_utils(UI, Manager, BottomPaneInfo),
  
  wxAuiManager:connect(Manager, aui_pane_maximize, [{skip,true}]),
  wxAuiManager:connect(Manager, aui_pane_restore, [{skip,true}]),    
  wxAuiManager:connect(Manager, aui_render, [{skip,true}]),    
  wxAuiManager:update(Manager),
    
  wxFrame:show(Frame),
  
  process_flag(trap_exit, true),

  State = #state{win=Frame},
  {Frame, State#state{workspace=Workspace, workspace_manager=Manager}}.


    
create_utils(Parent, Manager, Pane) ->
  %% Notebook styles
  Style = (0
     bor ?wxAUI_NB_TOP
     bor ?wxAUI_NB_WINDOWLIST_BUTTON
     bor ?wxAUI_NB_TAB_MOVE
     bor ?wxAUI_NB_SCROLL_BUTTONS
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

