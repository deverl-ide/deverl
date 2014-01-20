-module(ide_pref_debugger_wx).

-include_lib("wx/include/wx.hrl").

-behaviour(wx_object).
-export([start/1, init/1, terminate/2,  code_change/3,
	       handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).

-record(state, {parent,
            	  config
            	 }).


%% =====================================================================
%% Client API
%% =====================================================================

%% =====================================================================
%% @doc

-spec start(Config) -> wxWindow:wxWindow() when
  Config :: list().

start(Config) ->
  wx_object:start_link(?MODULE, Config, []).


%% =====================================================================
%% Callback functions
%% =====================================================================

init(Config) ->
  wx:batch(fun() -> do_init(Config) end).

do_init(Config) ->
  Parent = proplists:get_value(parent, Config),
  %   Panel = wxWindow:new(Parent, ?wxID_ANY),
  % 
  %   LRSizer = wxBoxSizer:new(?wxHORIZONTAL),
  %   wxWindow:setSizer(Panel, LRSizer),
  %   wxSizer:addSpacer(LRSizer, 20),
  % 
  %   VertSizer = wxBoxSizer:new(?wxVERTICAL),
  %   wxSizer:addSpacer(VertSizer, 20),
  %   wxSizer:add(VertSizer, wxStaticText:new(Panel, ?wxID_ANY, "Nothing here yet.")),
  % 
  %   wxSizer:addSpacer(VertSizer, 20),
  % 
  %   wxSizer:add(LRSizer, VertSizer),
  %   wxSizer:addSpacer(LRSizer, 20),
  % 
  % wxSizer:layout(LRSizer),
  
  Xrc = wxXmlResource:get(),
  Panel0 = wxPanel:new(),
  % ide_lib_dlg_wx:win_var(Dlg),

  %% Load XRC (Assumes all XRC handlers init previously)
  wxXmlResource:loadPanel(Xrc, Panel0, Parent, "prefs"),
  Sz = wxWindow:getSizer(Panel0),
  
  %% Load first pref
  Panel1 = wxXmlResource:loadPanel(Xrc, Parent, "pref_compiler"),
  wxSizer:add(Sz, Panel1),
  
  {Panel0, #state{parent=Panel0}}.


handle_event(Ev = #wx{}, State = #state{}) ->
  io:format("Got Event ~p~n",[Ev]),
  {noreply,State}.

handle_info(Msg, State) ->
  io:format( "Got Info ~p~n",[Msg]),
  {noreply,State}.

handle_call(shutdown, _From, State=#state{parent=Panel}) ->
  wxWindow:destroy(Panel),
  {stop, normal, ok, State};

handle_call(Msg, _From, State) ->
  io:format("Got Call ~p~n",[Msg]),
  {reply,ok,State}.

handle_cast(Msg, State) ->
  io:format("Got cast ~p~n",[Msg]),
  {noreply,State}.

code_change(_, _, State) ->
  {ok, State}.

terminate(_Reason, _) ->
  ok.
