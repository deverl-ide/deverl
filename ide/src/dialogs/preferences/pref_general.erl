-module(pref_general).

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
  
  
  ConfigSizer = wxBoxSizer:new(?wxHORIZONTAL),

  Choices = ["Default Config"],
  Choice = wxChoice:new(Panel, ?wxID_ANY, [{choices, Choices}]),
  wxChoice:setToolTip(Choice, "Select a configuration"),
  
  wxSizer:add(ConfigSizer, wxStaticText:new(Panel, ?wxID_ANY, "Configuration:")),
  wxSizer:add(ConfigSizer, 10, 0),
  wxSizer:add(ConfigSizer, Choice, [{proportion, 1}, {flag, ?wxEXPAND}]),
  wxSizer:add(ConfigSizer, 10, 0),
  wxSizer:add(ConfigSizer, wxButton:new(Panel, ?wxID_ANY, [{label, "New..."}]), [{proportion, 0}, {flag, ?wxEXPAND}]),
  wxSizer:add(ConfigSizer, 10, 0),
  wxSizer:add(ConfigSizer, wxButton:new(Panel, ?wxID_ANY, [{label, "Delete..."}]), [{proportion, 0}, {flag, ?wxEXPAND}]),

  wxSizer:add(VertSizer, ConfigSizer),
  
  
  wxSizer:addSpacer(VertSizer, 20),
  wxSizer:add(VertSizer, wxStaticLine:new(Panel, [{style, ?wxLI_HORIZONTAL}]), 
              [{flag, ?wxEXPAND}]),
  wxSizer:addSpacer(VertSizer, 20),
  
              
  FlexGridSizer = wxFlexGridSizer:new(3, 3, 10, 10),
  wxSizer:add(FlexGridSizer, wxStaticText:new(Panel, ?wxID_ANY, "Module:"), []),
  wxSizer:add(FlexGridSizer, wxTextCtrl:new(Panel, 22222221, []), [{proportion, 1}, {flag, ?wxEXPAND}]),
  wxSizer:add(FlexGridSizer, wxButton:new(Panel, ?wxID_ANY, [{label, "Browse..."}]), [{proportion, 0}]),
 
  wxSizer:add(FlexGridSizer, wxStaticText:new(Panel, ?wxID_ANY, "Main Function:"), []),
  wxSizer:add(FlexGridSizer, wxTextCtrl:new(Panel, 22222222, []), [{proportion, 1}, {flag, ?wxEXPAND}]),
  wxSizer:add(FlexGridSizer, 0, 0, []),
 
  wxSizer:add(FlexGridSizer, wxStaticText:new(Panel, ?wxID_ANY, "Arguments:"), []),
  wxSizer:add(FlexGridSizer, wxTextCtrl:new(Panel, 22222223, []), [{proportion, 1}, {flag, ?wxEXPAND}]),
  wxSizer:add(FlexGridSizer, 0, 0, []),
 
  wxSizer:add(FlexGridSizer, wxStaticText:new(Panel, ?wxID_ANY, "VM Options:"), []),
  wxSizer:add(FlexGridSizer, wxTextCtrl:new(Panel, 22222224, []), [{proportion, 1}, {flag, ?wxEXPAND}]),
  wxSizer:add(FlexGridSizer, 0, 0, []),
  
  wxFlexGridSizer:addGrowableCol(FlexGridSizer, 1),
              
                         
  wxSizer:add(VertSizer, FlexGridSizer, [{flag, ?wxEXPAND}, {proportion, 1}]),      
  wxSizer:addSpacer(VertSizer, 20),   
  
  wxSizer:add(LRSizer, VertSizer),
  wxSizer:addSpacer(LRSizer, 20),
  
  wxSizer:layout(LRSizer),
  {Panel, #state{parent=Panel}}.
  
    
%% =====================================================================
%% @doc OTP behaviour callbacks
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
  {stop, ignore, State}.

terminate(_Reason, _) ->
  ok.