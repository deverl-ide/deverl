%% =====================================================================
%% @author
%% @copyright
%% @title
%% @version
%% @doc The log used for displaying info/errors to the user.
%% Uses a read-only styledTextCtrl.
%% @end
%% =====================================================================

-module(log).

-include_lib("wx/include/wx.hrl").
-include("ide.hrl").

%% wx_object
-behaviour(wx_object).
-export([init/1, 
		 		 terminate/2, 
				 code_change/3, 
				 handle_info/2, 
				 handle_call/3, 
				 handle_cast/2, 
				 handle_event/2]).

%% API     
-export([new/1,
         message/1,
         error/1]).

%% Macros
-define(MARKER_EVEN, 0).
-define(MARKER_ODD, 1).
-define(stc, wxStyledTextCtrl).


%% Server state
-record(state, {win, 
								log}).


%% =====================================================================
%% Client API
%% =====================================================================	

%% =====================================================================
%% @doc

new(Config) ->
	wx_object:start_link({local, ?MODULE}, ?MODULE, Config, []).
	

%% =====================================================================
%% @doc

message(Msg) ->
  wx_object:cast(?MODULE, Msg).


%% =====================================================================
%% @doc

error(Msg) ->
  ok.


%% =====================================================================
%% Callback functions
%% =====================================================================

init(Config) ->
	Parent = proplists:get_value(parent, Config),
	Panel = wxPanel:new(Parent, []),
	MainSizer = wxBoxSizer:new(?wxVERTICAL),
	wxWindow:setSizer(Panel, MainSizer),

	Log = ?stc:new(Panel, []),
  ?stc:setMarginWidth(Log, 0, 0),
  ?stc:setMarginWidth(Log, 1, 0),
  ?stc:setMarginWidth(Log, 2, 0),
	?stc:setMarginLeft(Log, 3),
  ?stc:setReadOnly(Log, true),
	?stc:setLexer(Log, ?wxSTC_LEX_NULL),
  
  ?stc:setCaretWidth(Log, 0),
  ?stc:styleSetSize(Log, ?wxSTC_STYLE_DEFAULT, 12),
  ?stc:styleSetSize(Log, ?wxSTC_STYLE_BRACEBAD, 16),
  ?stc:styleClearAll(Log),
  
  %% Markers for alternate row line colours
  ?stc:markerDefine(Log, ?MARKER_EVEN, ?wxSTC_MARK_BACKGROUND),
  ?stc:markerSetBackground(Log, ?MARKER_EVEN, ?ROW_BG_EVEN),
  ?stc:markerDefine(Log, ?MARKER_ODD, ?wxSTC_MARK_BACKGROUND),
  ?stc:markerSetBackground(Log, ?MARKER_ODD, ?ROW_BG_ODD),
                                                	
	wxSizer:add(MainSizer, Log, [{flag, ?wxEXPAND}, {proportion, 1}]),
                                        

	State=#state{win=Panel, 
				       log=Log},
  
  {Panel, State}.

handle_info(Msg, State) ->
  io:format("Got cast ~p~n",[Msg]),
  {noreply, State}.
  
handle_cast(Msg, State=#state{log=Log}) ->
  ?stc:setReadOnly(Log, false),
  Line = ?stc:getCurrentLine(Log),
  case Line rem 2 of
      0 ->
        ?stc:markerAdd(Log, Line, ?MARKER_EVEN);
      _ ->
        ?stc:markerAdd(Log, Line, ?MARKER_ODD)
  end,
  ?stc:addText(Log, Msg),
  ?stc:newLine(Log),
  ?stc:setReadOnly(Log, true),
  {noreply, State}.

handle_call(Msg, _From, State) ->
  io:format("Got Call ~p~n",[Msg]),
  {reply,ok, State}.
    
handle_event(#wx{}, State) ->
	{noreply, State}.
    
code_change(_, _, State) ->
	{stop, not_yet_implemented, State}.

terminate(_Reason, #state{win=Frame}) ->
	io:format("TERMINATE SHELL~n"),
	wxPanel:destroy(Frame).

	
%% =====================================================================
%% Internal functions
%% =====================================================================
	
%% =====================================================================
%% @doc 