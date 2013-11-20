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
-define(STYLE_DUMMY, 1).
-define(STYLE_ERROR, 2).

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
  wx_object:cast(?MODULE, {error, Msg}).


%% =====================================================================
%% Callback functions
%% =====================================================================

init(Config) ->
	Parent = proplists:get_value(parent, Config),
	Panel = wxPanel:new(Parent),
	MainSizer = wxBoxSizer:new(?wxVERTICAL),
	wxWindow:setSizer(Panel, MainSizer),

	Log = ?stc:new(Panel, [{style, ?wxBORDER_NONE}]),
  ?stc:setMarginWidth(Log, 0, 0),
  ?stc:setMarginWidth(Log, 1, 0),
  ?stc:setMarginWidth(Log, 2, 0),
  ?stc:setReadOnly(Log, true),
	?stc:setLexer(Log, ?wxSTC_LEX_NULL),
  ?stc:setCaretWidth(Log, 0),
  
  %% Default "normal" style
  ?stc:styleSetFont(Log, ?wxSTC_STYLE_DEFAULT, wxFont:new(11, ?wxFONTFAMILY_TELETYPE, ?wxNORMAL, ?wxNORMAL,[])),
  ?stc:styleSetSize(Log, ?wxSTC_STYLE_DEFAULT, 11),
  ?stc:styleClearAll(Log),
  
  %% Dummy style to add vertical spacing (increase line height)
  ?stc:styleSetSpec(Log, ?STYLE_DUMMY, "fore:#234567,size:13"),
  %% Error style
  ?stc:styleSetSpec(Log, ?STYLE_ERROR, "fore:#D8203E"),
  
  %% Markers for alternate row line colours
  ?stc:markerDefine(Log, ?MARKER_EVEN, ?wxSTC_MARK_BACKGROUND),
  ?stc:markerSetBackground(Log, ?MARKER_EVEN, ?ROW_BG_EVEN),
  ?stc:markerDefine(Log, ?MARKER_ODD, ?wxSTC_MARK_BACKGROUND),
  ?stc:markerSetBackground(Log, ?MARKER_ODD, ?ROW_BG_ODD),
                                                	
	wxSizer:add(MainSizer, Log, [{flag, ?wxEXPAND}, {proportion, 1}]),
                                        
	State=#state{win=Panel, 
				       log=Log},
               
  %% Note this stops the samll square artifact from appearing in top left corner.
  wxSizer:layout(MainSizer),
  
  {Panel, State}.

handle_info(Msg, State) ->
  io:format("Got cast ~p~n",[Msg]),
  {noreply, State}.

handle_cast({error, Msg}, State=#state{log=Log}) ->
  {ok, Length} = append(Log, Msg),
  Start = ?stc:positionFromLine(Log, ?stc:getCurrentLine(Log) - 1),
  ?stc:startStyling(Log, Start, 31),
  ?stc:setStyling(Log, Length, ?STYLE_ERROR),
  {noreply, State};
handle_cast(Msg, State=#state{log=Log}) ->
  append(Log, Msg),
  {noreply, State}.

handle_call(Msg, _From, State) ->
  io:format("Got Call ~p~n",[Msg]),
  {reply,ok, State}.
    
handle_event(#wx{}, State) ->
	{noreply, State}.
    
code_change(_, _, State) ->
	{stop, not_yet_implemented, State}.

terminate(_Reason, #state{win=Frame}) ->
	io:format("TERMINATE LOG~n"),
	wxPanel:destroy(Frame).

	
%% =====================================================================
%% Internal functions
%% =====================================================================
	
%% =====================================================================
%% @doc Get the current time formatted as a string HH:MM:SS.

get_time() ->
  {_, {H, M, S}} = calendar:local_time(),
  Args = [H, M, S],
  Str = io_lib:format("~2.10.0B:~2.10.0B:~2.10.0B", Args),
  lists:flatten(Str).
<<<<<<< HEAD
=======
  

%% =====================================================================
%% @doc Append a line (record) to the log.

append(Log, Msg) ->
  ?stc:gotoPos(Log, ?stc:getLength(Log)),
  ?stc:setReadOnly(Log, false),
  Line = ?stc:getCurrentLine(Log),
  case Line rem 2 of
      0 ->
        ?stc:markerAdd(Log, Line, ?MARKER_EVEN);
      _ ->
        ?stc:markerAdd(Log, Line, ?MARKER_ODD)
  end,
  Message = lists:flatten([get_time(), "  ", Msg]),
  ?stc:addText(Log, Message),
  ?stc:newLine(Log),
  ?stc:setReadOnly(Log, true),
  {ok, length(Message)}.
>>>>>>> da3c22c9b4321e9daf2195a15a1564cdef9d84b3
