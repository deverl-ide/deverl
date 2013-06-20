%% editor.erl
%% Simple frame that loads an editor object

-module(editor).

-export([start/1, init/1, terminate/2,  code_change/3,
	       handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).

-include_lib("wx/include/wx.hrl").
-behaviour(wx_object).

-define(DEFAULT_FONT_SIZE, 11).

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
  Editor = wxStyledTextCtrl:new(Panel),
  
  wxSizer:add(Sizer, Editor, [{flag, ?wxEXPAND},
                              {proportion, 1}]),
                              
                              
  %% Editor styles
  Font = wxFont:new(?DEFAULT_FONT_SIZE, ?wxFONTFAMILY_TELETYPE, ?wxNORMAL, ?wxNORMAL,[]),
  wxWindow:setFont(Editor, Font),
  wxStyledTextCtrl:styleClearAll(Editor),
  wxStyledTextCtrl:styleSetFont(Editor, ?wxSTC_STYLE_DEFAULT, Font),
  wxStyledTextCtrl:setLexer(Editor, ?wxSTC_LEX_ERLANG),
  %% Margins
  wxStyledTextCtrl:setMarginType(Editor, 0, ?wxSTC_MARGIN_NUMBER),
  MW = wxStyledTextCtrl:textWidth(Editor, ?wxSTC_STYLE_LINENUMBER, "9"),
  wxStyledTextCtrl:setMarginWidth(Editor, 0, MW*2),
  wxStyledTextCtrl:setMarginWidth(Editor, 1, 0),
  
  wxStyledTextCtrl:styleSetForeground (Editor, ?wxSTC_STYLE_LINENUMBER, {75, 75, 75}),
  wxStyledTextCtrl:styleSetBackground (Editor, ?wxSTC_STYLE_LINENUMBER, {220, 220, 220}),
  %% Markers
  
  
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
    
%%%%% Editor Functions %%%%%
keywords() ->
  KWS = ["after" "and" "andalso" "band" "begin" "bnot" 
  "bor" "bsl" "bsr" "bxor" "case" "catch" "cond" "div" 
  "end" "fun" "if" "let" "not" "of" "or" "orelse" 
  "receive" "rem" "try" "when" "xor"],
  lists:flatten([KW ++ " " || KW <- KWS]).