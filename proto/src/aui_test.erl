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
  S1 = wxBoxSizer:new(?wxVERTICAL),
  TestT = wxTextCtrl:new(EditorWindow, 8001, [{style, ?wxTE_MULTILINE}]), 
  wxSizer:add(S1, TestT, [{flag, ?wxEXPAND},
                                 {proportion, 1}]),
  wxPanel:setSizer(EditorWindow, S1),
  
  %% The left pane/test window
  TestWindow = wxPanel:new(UI),
  TestWindowPaneInfo = wxAuiPaneInfo:left(wxAuiPaneInfo:new(PaneInfo)),
  wxAuiPaneInfo:minSize(TestWindowPaneInfo, {200,0}),
  wxAuiPaneInfo:bestSize(TestWindowPaneInfo, {200,0}),
  wxAuiManager:addPane(Manager, TestWindow, TestWindowPaneInfo),
  
  TestSizer = wxBoxSizer:new(?wxVERTICAL),
  TestT2 = wxTextCtrl:new(TestWindow, 8001, [{style, ?wxTE_MULTILINE}]), 
  wxSizer:add(TestSizer, TestT2, [{flag, ?wxEXPAND},
                                 {proportion, 1}]),
  wxPanel:setSizer(TestWindow, TestSizer),
  
  %% The bottom pane/utility window
  BottomWindow = wxPanel:new(UI),
  BottomPaneInfo = wxAuiPaneInfo:bottom(wxAuiPaneInfo:new(PaneInfo)),
  wxAuiPaneInfo:minSize(BottomPaneInfo, {0,200}),
  wxAuiPaneInfo:bestSize(BottomPaneInfo, {0, 200}),
  wxAuiManager:addPane(Manager, BottomWindow, BottomPaneInfo),
  
  S2 = wxBoxSizer:new(?wxVERTICAL),
  TestT3 = wxTextCtrl:new(BottomWindow, 8001, [{style, ?wxTE_MULTILINE}]), 
  wxSizer:add(S1, TestT3, [{flag, ?wxEXPAND},
                                 {proportion, 1}]),
  wxPanel:setSizer(BottomWindow, S2),
  
  
  
  
  
  
  wxAuiManager:connect(Manager, aui_pane_maximize, [{skip,true}]),
  wxAuiManager:connect(Manager, aui_pane_restore, [{skip,true}]),    
  wxAuiManager:connect(Manager, aui_render, [{skip,true}]),    
  wxAuiManager:update(Manager),
    
  wxFrame:show(Frame),
  
  process_flag(trap_exit, true).
  