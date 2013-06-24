%% AUI Manager
%% aui.erl

-module(aui).

-include_lib("wx/include/wx.hrl").
-behaviour(wx_object).

-export([start/0, init/1, terminate/2,  code_change/3,
         handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).

%% The record containing the State.
-record(state, {win, other}).

start() ->
  start([]).
  
start(Args) ->
  wx_object:start(?MODULE, Args, []).
  
start_link() ->
  start_link([]).
  
start_link(Args) ->
  wx_object:start_link(?MODULE, Args, []).

%% init(Args) should return:
%% {wxObject, State} | {wxObject, State, Timeout} | ignore | {stop, Reason}
init(Options) ->
  wx:new(Options),
  process_flag(trap_exit, true),
  
  Frame = wxFrame:new(wx:null(), ?wxID_ANY, "Erlang IDE", [{size,{800,600}}]),
  
  ide_menubar:new(Frame),
  
  UI = wxPanel:new(Frame, []),
  Sizer = wxBoxSizer:new(?wxVERTICAL),
  
  Manager = wxAuiManager:new([{managed_wnd, UI}]),
  
  PaneInfo = wxAuiPaneInfo:new(),
  wxAuiPaneInfo:closeButton(PaneInfo, [{visible, false}]),
  % wxAuiPaneInfo:right(PaneInfo),
  % wxAuiPaneInfo:dockable(PaneInfo, [{b, false}]),
  wxAuiPaneInfo:floatingSize(PaneInfo, 300,200),
  wxAuiPaneInfo:minSize(PaneInfo, {50,50}),
  % wxAuiPaneInfo:paneBorder(PaneInfo),
  % wxAuiPaneInfo:floatable(PaneInfo, [{b, true}]),
  wxAuiPaneInfo:minimizeButton(PaneInfo, [{visible, true}]),
  wxAuiPaneInfo:bottomDockable(PaneInfo, [{b, true}]),
  
  io:format("DOCKABLE: ~p~n", [wxAuiPaneInfo:isBottomDockable(PaneInfo)]),
  
  %% The left pane/test window
  TestWindow = wxPanel:new(UI), 
  wxAuiManager:addPane(Manager, TestWindow, 
    wxAuiPaneInfo:caption(wxAuiPaneInfo:left(wxAuiPaneInfo:new(PaneInfo)), "Test Cases")),
  
  %% The centre pane/editor window
  EditorWindow = wxPanel:new(UI),
  Pane2 = wxAuiPaneInfo:new(PaneInfo),
  wxAuiPaneInfo:centrePane(Pane2),
  wxAuiManager:addPane(Manager, EditorWindow, Pane2),
  
  %% The bottom pane/utility window
  UtilityWindow = wxPanel:new(UI),
  wxAuiManager:addPane(Manager, UtilityWindow, wxAuiPaneInfo:bottom(wxAuiPaneInfo:new(PaneInfo))),
  
  
  wxPanel:setSizer(UI, Sizer),
  
  wxAuiManager:update(Manager),
    
  wxFrame:connect(Frame, command_menu_selected),
  wxFrame:connect(Frame, close_window),
    
  wxFrame:show(Frame),
  
  State = #state{win=Frame},
  {Frame, State#state{other=o}}.

%%%%% Callbacks %%%%%
handle_info({'EXIT',_, wx_deleted}, State) ->
    {noreply,State};
handle_info({'EXIT',_, shutdown}, State) ->
    {noreply,State};
handle_info({'EXIT',_, normal}, State) ->
    {noreply,State};
handle_info(Msg, State) ->
    io:format("Got Info ~p~n",[Msg]),
    {noreply,State}.

handle_call(Msg, _From, State) ->
    io:format("Got Call ~p~n",[Msg]),
    {reply,ok,State}.

handle_cast(Msg, State) ->
    io:format("Got cast ~p~n",[Msg]),
    {noreply,State}.

handle_event(#wx{event=#wxClose{}}, State = #state{win=Frame}) ->
    io:format("~p Closing window ~n",[self()]),
    ok = wxFrame:setStatusText(Frame, "Closing...",[]),
    {stop, normal, State};
handle_event(A,B) ->
    io:format("~p~n~p~n", [A,B]).

code_change(_, _, State) ->
    {stop, not_yet_implemented, State}.

terminate(_Reason, _State) ->
    wx:destroy().
