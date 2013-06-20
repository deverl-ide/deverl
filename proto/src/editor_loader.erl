%% editor_loader.erl
%% Simple frame that loads an editor object into the GUI

-module(editor_loader).
-export([start/0, start/1, start_link/0, start_link/1, 
	 init/1, terminate/2,  code_change/3,
	 handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).

-include_lib("wx/include/wx.hrl").

-behaviour(wx_object).

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

%% init(Args) should return 
%% {wxObject, State} | {wxObject, State, Timeout} | ignore | {stop, Reason}
init(Options) ->
  wx:new(Options),
  process_flag(trap_exit, true),
  
  Frame = wxFrame:new(wx:null(), ?wxID_ANY, "Main Frame", [{size,{600,400}}]),
  
  % MenuBar = wxMeuBar:new([]),  
  
  wxFrame:connect(Frame, command_menu_selected),
  wxFrame:connect(Frame, close_window),
  wxFrame:connect(Frame, key_down),
  
  load_editor([{parent, Frame}]),
  
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
handle_event(_,State) ->
    io:format("Event got in Frame.~n", []),
    {noreply, State}.

code_change(_, _, State) ->
    {stop, not_yet_implemented, State}.

terminate(_Reason, _State) ->
    wx:destroy().

load_editor(Parent) ->
  %% Start an editor
  editor:start(Parent),
  %% Add it to a subwindow
  
  %% Add it to a tab
  ok.