%% =====================================================================
%% @author
%% @copyright
%% @title
%% @version
%% @doc The log used for displaying info/errors to the user.
%% Uses a read-only styledTextCtrl.
%% @end
%% =====================================================================

-module(ide_log_out_wx).

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
         message/2,
         error/1,
         error/2]).

%% Macros
-define(MARKER_EVEN, 0).
-define(MARKER_ODD, 1).
-define(stc, wxStyledTextCtrl).
-define(STYLE_DUMMY, 1).
-define(STYLE_ERROR, 2).
-define(STYLE_HOTSPOT, 3).

%% Server state
-record(state, {win,
								log}).


%% =====================================================================
%% Client API
%% =====================================================================

%% =====================================================================
%% @doc

-spec new(list()) -> wxWindow:wxWindow().

new(Config) ->
	wx_object:start_link({local, ?MODULE}, ?MODULE, Config, []).


%% =====================================================================
%% @doc
%%

-spec message(string()) -> ok.

message(Msg) ->
  message(Msg, []).


%% =====================================================================
%% @doc Append a message to the log.
%% The hotspot will be a clickable link and must be a substring of Msg.

-spec message(string(), [Option]) -> ok when
  Option :: {hotspot, string()}.

message(Msg, Options) ->
  wx_object:cast(?MODULE, {Msg, Options}).


%% =====================================================================
%% @doc
%% @see ide_log_out_wx:message/2 for a description of options.

-spec error(string()) -> no_return().

error(Msg) ->
  ?MODULE:error(Msg, []).

-spec error(string(), list()) -> ok.

error(Msg, Options) ->
  wx_object:cast(?MODULE, {error, Msg, Options}).


%% =====================================================================
%% Callback functions
%% =====================================================================

init(Config) ->
	Parent = proplists:get_value(parent, Config),
	Panel = wxPanel:new(Parent),
	MainSizer = wxBoxSizer:new(?wxVERTICAL),
	wxWindow:setSizer(Panel, MainSizer),

	Log = ?stc:new(Panel, [{style, ?wxBORDER_NONE}, {id, ?WINDOW_LOG}]),
  ?stc:setMarginWidth(Log, 0, 0),
  ?stc:setMarginWidth(Log, 1, 0),
  ?stc:setMarginWidth(Log, 2, 0),
  ?stc:setReadOnly(Log, true),
	?stc:setLexer(Log, ?wxSTC_LEX_NULL),
  ?stc:setCaretWidth(Log, 0),

  %% Default "normal" style
  ?stc:styleSetFont(Log, ?wxSTC_STYLE_DEFAULT, ide_sys_pref_gen:get_font(log_font)),
  ?stc:styleSetSize(Log, ?wxSTC_STYLE_DEFAULT, 11),
  ?stc:styleClearAll(Log),

  %% Dummy style to add vertical spacing (increase line height)
  ?stc:styleSetSpec(Log, ?STYLE_DUMMY, "fore:#234567,size:13"),
  %% Error style
  ?stc:styleSetSpec(Log, ?STYLE_ERROR, "fore:#D8203E"),
  %% Hotspot style
  ?stc:styleSetSpec(Log, ?STYLE_HOTSPOT, "fore:#0000FF,underline"),
  ?stc:styleSetHotSpot(Log, ?STYLE_HOTSPOT, true),

  %% Markers for alternate row line colours
  ?stc:markerDefine(Log, ?MARKER_EVEN, ?wxSTC_MARK_BACKGROUND),
  ?stc:markerSetBackground(Log, ?MARKER_EVEN, ?ROW_BG_EVEN),
  ?stc:markerDefine(Log, ?MARKER_ODD, ?wxSTC_MARK_BACKGROUND),
  ?stc:markerSetBackground(Log, ?MARKER_ODD, ?ROW_BG_ODD),
  
  %% Events
  ?stc:connect(Log, set_focus, [{skip, true}]),
  ?stc:connect(Log, kill_focus, [{skip, true}]),
  ?stc:connect(Log, stc_updateui, []),

	wxSizer:add(MainSizer, Log, [{flag, ?wxEXPAND}, {proportion, 1}]),

	State=#state{win=Panel,
				       log=Log},

  %% Note this stops the small square artifact from appearing in top left corner.
  wxSizer:layout(MainSizer),

  %% Connect handlers
  ?stc:connect(Log, stc_hotspot_click),

  {Panel, State}.

handle_info(Msg, State) ->
  io:format("Got cast ~p~n",[Msg]),
  {noreply, State}.

handle_cast({error, Msg, Options}, State=#state{log=Log}) ->
  {ok, Length} = append(Log, Msg),
  Start = ?stc:positionFromLine(Log, ?stc:getCurrentLine(Log) - 1),
  ?stc:startStyling(Log, Start, 31),
  ?stc:setStyling(Log, Length, ?STYLE_ERROR),
  case proplists:get_value(hotspot, Options) of
    undefined -> ok;
    Hotspot ->
      L = string:str(Msg, Hotspot),
      ?stc:startStyling(Log, Start + L + 9, 31), % (+9) allows for width of timestamp
      ?stc:setStyling(Log, length(Hotspot), ?STYLE_HOTSPOT)
  end,
  {noreply, State};
handle_cast({Msg, Options}, State=#state{log=Log}) ->
  case proplists:get_value(hotspot, Options) of
    undefined -> ok;
    _Hotspot -> ok
  end,
  append(Log, Msg),
  {noreply, State}.

handle_call(Msg, _From, State) ->
  io:format("Got Call ~p~n",[Msg]),
  {reply,ok, State}.

handle_event(#wx{event=#wxStyledText{type=stc_hotspot_click, position=Pos}},
             State=#state{log=Log}) ->
  Style = ?stc:getStyleAt(Log, Pos),
  Max = ?stc:getLength(Log),
  %% Get the leftmost position where the style starts
  Leftmost = fun(_F, 0) -> 0;
    (F, P) ->
      case ?stc:getStyleAt(Log, P) of
        Style ->
          F(F, P - 1);
        _ ->
          P + 1
      end
  end,
  %% and the rightmost
  Rightmost = fun(_F, Limit) when Limit =:= Max -> Max;
    (F, P) ->
      case ?stc:getStyleAt(Log, P) of
        Style ->
          F(F, P + 1);
        _ ->
          P
      end
  end,
  L = Leftmost(Leftmost, Pos - 1),
  R = Rightmost(Rightmost, Pos + 1),
  Range = ?stc:getTextRange(Log, L, R),
  hotspot_action(Range),
	{noreply, State};
handle_event(#wx{event=#wxFocus{type=set_focus}}, State) ->
  %% Enable undo/redo
  ide:toggle_menu_items([?wxID_COPY, ?wxID_SELECTALL], true),
  {noreply, State};
handle_event(#wx{event=#wxFocus{type=kill_focus}}, State) ->
  %% Disable undo
  ide:toggle_menu_items([?wxID_COPY, ?wxID_SELECTALL], false),
  {noreply, State};
handle_event(#wx{event=#wxStyledText{type=stc_updateui}},State=#state{log=Log}) ->
  case ?stc:getSelection(Log) of
    {N,N} -> 
      ide:toggle_menu_items([?wxID_COPY], false);
    _ -> 
      ide:toggle_menu_items([?wxID_COPY], true)
  end,
  {noreply, State}.

code_change(_, _, State) ->
	{ok, State}.

terminate(_Reason, #state{win=Frame}) ->
	wxPanel:destroy(Frame).


%% =====================================================================
%% Internal functions
%% =====================================================================

%% =====================================================================
%% @doc Get the current time formatted as a string HH:MM:SS.

-spec get_time() -> [term()].

get_time() ->
  {_, {H, M, S}} = calendar:local_time(),
  Args = [H, M, S],
  Str = io_lib:format("~2.10.0B:~2.10.0B:~2.10.0B", Args),
  lists:flatten(Str).


%% =====================================================================
%% @doc Append a line (record) to the log.

-spec append(wxStyledTextCtrl:wxStyledTextCtrl(), string()) -> {ok, integer()}.

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


%% =====================================================================
%% @doc

-spec hotspot_action(wxStyledTextCtrl:charlist()) -> ok.

hotspot_action(Range) ->
  case Range of
    "output" ->
      ide:display_output_window(?WINDOW_OUTPUT);
    _ ->
      ok
  end.