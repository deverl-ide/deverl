-module(pref_editor).

-include_lib("wx/include/wx.hrl").

-behaviour(wx_object).
-export([start/1, init/1, terminate/2,  code_change/3,
	       handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).

-record(state, {parent,
            	  config
            	 }).

start(Config) ->
  wx_object:start_link(?MODULE, Config, []).

init(Config) ->
  wx:batch(fun() -> do_init(Config) end).

do_init(Config) ->
  Parent = proplists:get_value(parent, Config),
  Panel = wxWindow:new(Parent, ?wxID_ANY),
     
  LRSizer = wxBoxSizer:new(?wxHORIZONTAL),
  wxWindow:setSizer(Panel, LRSizer),
  wxSizer:addSpacer(LRSizer, 20),
  
  VertSizer = wxBoxSizer:new(?wxVERTICAL),
  wxSizer:addSpacer(VertSizer, 20),
  wxSizer:add(VertSizer, wxStaticText:new(Panel, ?wxID_ANY, "Nothing here yet.")), 
  
  wxSizer:addSpacer(VertSizer, 20),   
  
  wxSizer:add(LRSizer, VertSizer),
  wxSizer:addSpacer(LRSizer, 20),
                      
	wxSizer:layout(LRSizer),
  
  {Panel, #state{parent=Panel}}.

  
    
%% =====================================================================
%% @doc OTP behaviour callbacks
handle_event(Ev = #wx{}, State = #state{}) ->
    demo:format(State#state.config,"Got Event ~p~n",[Ev]),
    {noreply,State}.

%% Callbacks handled as normal gen_server callbacks
handle_info(Msg, State) ->
    demo:format(State#state.config, "Got Info ~p~n",[Msg]),
    {noreply,State}.

handle_call(shutdown, _From, State=#state{parent=Panel}) ->
    wxWindow:destroy(Panel),
    {stop, normal, ok, State};

handle_call(Msg, _From, State) ->
    demo:format(State#state.config,"Got Call ~p~n",[Msg]),
    {reply,ok,State}.

handle_cast(Msg, State) ->
    io:format("Got cast ~p~n",[Msg]),
    {noreply,State}.

code_change(_, _, State) ->
    {stop, ignore, State}.

terminate(_Reason, _) ->
    ok.