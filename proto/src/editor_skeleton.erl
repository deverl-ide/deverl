%% editor_skeleton.erl
%% skeleton for the editor.

-module(editor_skeleton).

-export([start/1, 
	 init/1, terminate/2,  code_change/3,
	 handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).

-compile(export_all).


-include_lib("wx/include/wx.hrl").
-behaviour(wx_object).

%% The record containing the State.
-record(state, {win}).

start(Config) ->
    wx_object:start_link(?MODULE, Config, []).

%% init(Args) should return 
%% {wxObject, State} | {wxObject, State, Timeout} | ignore | {stop, Reason}
init(Config) ->
  Parent = proplists:get_value(parent, Config),
  Panel = wxPanel:new(Parent),
  Sizer = wxBoxSizer:new(?wxVERTICAL),
  wxPanel:setSizer(Panel, Sizer),
  Editor = wxTextCtrl:new(Panel, 3, [{value, "This is a\nmultiline\nwxTextCtrl"},
					  {style, ?wxDEFAULT bor ?wxTE_MULTILINE}]),
  
  wxSizer:add(Sizer, Editor, [{border, 8}, 
                              {flag, ?wxALL bor ?wxEXPAND},
                              {proportion, 1}]),
  
  {Panel, #state{win=Panel}}.

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