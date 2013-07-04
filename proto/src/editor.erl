%% editor.erl
%% Simple frame that loads an editor object

-module(editor).

-export([word_wrap/1, stop/0, update_style/2]).

-export([start/1, init/1, terminate/2,  code_change/3,
         handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).

-include_lib("wx/include/wx.hrl").
-behaviour(wx_object).

-define(GREY, {25,0,0}).
-define(SELECTION_COLOUR, {200,255,255}).

-define(LEFT_MARGIN_WIDTH, 6).
-define(RIGHT_MARGIN_WIDTH, 6).
-define(MARGIN_LINE_NUMBER_PADDING, " ").
-define(MARGIN_LINE_NUMBER_WIDTH, "  ").
-define(MARGIN_LINE_NUMBER_POINT_REDUCTION, 2). %% The size (pts) to reduce margin text by

%% The record containing the state maintained by the server
-record(state, {win, 
                editor
               }).

%% @doc Create and return a new editor instance
start(Config) ->
  wx_object:start(?MODULE, Config, []).
  % wx_object:start_link(?MODULE, Config, []).

init(Config) ->
  Parent = proplists:get_value(parent, Config),
  Sb = proplists:get_value(status_bar, Config),
  Font = proplists:get_value(font, Config),
  
  Contents = proplists:get_value(contents, Config, false),
  
  Panel = wxPanel:new(Parent),

  Sizer = wxBoxSizer:new(?wxVERTICAL),
  wxPanel:setSizer(Panel, Sizer),
  Editor = wxStyledTextCtrl:new(Panel), 
  wxSizer:add(Sizer, Editor, [{flag, ?wxEXPAND},
                              {proportion, 1}]),              
                                                                                             
  %% Editor styles  
  wxStyledTextCtrl:styleSetFont(Editor, ?wxSTC_STYLE_DEFAULT, Font),
  
  % wxStyledTextCtrl:styleClearAll(Editor),
  
  wxStyledTextCtrl:setLexer(Editor, ?wxSTC_LEX_ERLANG),
  wxStyledTextCtrl:setSelBackground(Editor, true, ?SELECTION_COLOUR),
  wxStyledTextCtrl:setSelectionMode(Editor, ?wxSTC_SEL_LINES),
  wxStyledTextCtrl:setMargins(Editor, ?LEFT_MARGIN_WIDTH, ?RIGHT_MARGIN_WIDTH), %% Left and right of text

  wxStyledTextCtrl:setMarginType(Editor, 0, ?wxSTC_MARGIN_NUMBER),   
  Mw = wxStyledTextCtrl:textWidth(Editor, ?wxSTC_STYLE_LINENUMBER, 
        ?MARGIN_LINE_NUMBER_PADDING ++ ?MARGIN_LINE_NUMBER_WIDTH),
  wxStyledTextCtrl:setMarginWidth(Editor, 0, Mw),
  wxStyledTextCtrl:styleSetSize(Editor, ?wxSTC_STYLE_LINENUMBER,
        (wxFont:getPointSize(Font) - ?MARGIN_LINE_NUMBER_POINT_REDUCTION)),
  
  %% Add the keywords
  wxStyledTextCtrl:setKeyWords(Editor, 0, keywords()),
  
  update_styles(Editor),
  
  %% Folding
  wxStyledTextCtrl:setMarginType (Editor, 1, ?wxSTC_MARGIN_SYMBOL),
  wxStyledTextCtrl:setMarginWidth(Editor, 1, 4),
  wxStyledTextCtrl:setMarginMask (Editor, 1, ?wxSTC_MASK_FOLDERS),
  wxStyledTextCtrl:setMarginSensitive(Editor, 1, true), %% Makes margin sensitive to mouse clicks
  
  wxStyledTextCtrl:setProperty(Editor, "fold", "1"),
  
  wxStyledTextCtrl:markerDefine (Editor, ?wxSTC_MARKNUM_FOLDER, ?wxSTC_MARK_ARROW, 
    [{foreground, ?GREY}, {background, ?GREY}]),
  wxStyledTextCtrl:markerDefine (Editor, ?wxSTC_MARKNUM_FOLDEROPEN, ?wxSTC_MARK_ARROWDOWN, 
    [{foreground, ?GREY}, {background, ?GREY}]),
  wxStyledTextCtrl:markerDefine (Editor, ?wxSTC_MARKNUM_FOLDERSUB, ?wxSTC_MARK_EMPTY, 
    [{foreground, ?GREY}, {background, ?GREY}]),
  wxStyledTextCtrl:markerDefine (Editor, ?wxSTC_MARKNUM_FOLDEREND, ?wxSTC_MARK_ARROW, 
    [{foreground, ?GREY}, {background, ?GREY}]),
  wxStyledTextCtrl:markerDefine (Editor, ?wxSTC_MARKNUM_FOLDEROPENMID, ?wxSTC_MARK_ARROWDOWN, 
    [{foreground, ?GREY}, {background, ?GREY}]),        
  wxStyledTextCtrl:markerDefine (Editor, ?wxSTC_MARKNUM_FOLDERMIDTAIL, ?wxSTC_MARK_EMPTY, 
    [{foreground, ?GREY}, {background, ?GREY}]),
  wxStyledTextCtrl:markerDefine (Editor, ?wxSTC_MARKNUM_FOLDERTAIL, ?wxSTC_MARK_EMPTY, 
    [{foreground, ?GREY}, {background, ?GREY}]),
  
  %% Attach events
  wxStyledTextCtrl:connect(Editor, stc_marginclick, []),
  wxStyledTextCtrl:connect(Editor, stc_modified, [{userData, Sb}]),
  
  %% Load contents if any
  if
    Contents /= false ->
      wxStyledTextCtrl:setText(Editor, Contents);
    true -> ok
  end,
    
  % process_flag(trap_exit, true),
  {Panel, #state{win=Panel, editor=Editor}}.

%%%%%%%%%%%%%%%%%%%%%
%%%%% Callbacks %%%%%
handle_info({'EXIT',_, wx_deleted}, State) ->
    io:format("Got Info~n"),
    {noreply,State};
handle_info({'EXIT',_, shutdown}, State) ->
    io:format("Got Info~n"),
    {noreply,State};
handle_info({'EXIT',_, normal}, State) ->
    io:format("Got Info~n"),
    {noreply,State};
handle_info(Msg, State) ->
    io:format("Got Info ~p~n",[Msg]),
    {noreply,State}.

handle_call(Msg, _From, State) ->
    io:format("Got Call ~p~n",[Msg]),
    {reply,State#state.editor,State}.

handle_cast(stop, State)->
    io:format("Handle cast: stop~n"),
    {stop, normal, State};
handle_cast(Msg, State) ->
    io:format("Got cast ~p~n",[Msg]),
    {noreply,State}.

handle_event(_A=#wx{event=#wxStyledText{type=stc_change}=_E}, State = #state{editor=Editor}) ->
    io:format("Change event: ~p~n", [_E]),
    {noreply, State};
handle_event(_A=#wx{event=#wxStyledText{type=stc_modified}=_E, userData=Sb}, 
             State=#state{editor=Editor}) ->
    %% Update status bar line/col position
    {X,Y} = get_x_y(Editor),
    customStatusBar:set_text(Sb,{field,line}, io_lib:format("~w:~w",[X, Y])),
    %% Update margin width if required
    adjust_margin_width(Editor),  
    {noreply, State};
handle_event(#wx{event=#wxStyledText{type=stc_marginclick, position = Pos, margin = Margin} = _E},
             State = #state{editor=Editor}) ->
    Ln = wxStyledTextCtrl:lineFromPosition(Editor, Pos),
    Fl = wxStyledTextCtrl:getFoldLevel(Editor, Ln),
    case Margin of
      1 when Ln > 0, Fl > 0 ->
        wxStyledTextCtrl:toggleFold(Editor, Ln);
      _ -> ok
    end,
    {noreply, State};
handle_event(E,O) ->
  io:format("editor catchall Event: ~p~nObject: ~p~n", [E,O]),
  {noreply, O}.
    
code_change(_, _, State) ->
    {stop, not_yet_implemented, State}.

terminate(_Reason, _State) ->
  io:format("editor callback: terminate: stop~n").
    
%%%%%%%%%%%%%%%%%%%%%
%%%%% Internals %%%%%

%% @doc Defines the keywords in the Erlang language
%% @private
keywords() ->
  KWS = ["after" "and" "andalso" "band" "begin" "bnot" 
  "bor" "bsl" "bsr" "bxor" "case" "catch" "cond" "div" 
  "end" "fun" "if" "let" "not" "of" "or" "orelse" 
  "receive" "rem" "try" "when" "xor"],
  lists:flatten([KW ++ " " || KW <- KWS]).

%% @doc Updates the styles for individual elements in the lexer
-spec update_styles(Editor) -> 'ok' when
  Editor :: wxStyledTextCtrl:wxStyledTextCtrl().
update_styles(Editor) ->
  %% {Style, Colour}
  Styles =  [{?wxSTC_ERLANG_DEFAULT,  {0,0,0}},
       {?wxSTC_ERLANG_COMMENT,  {160,53,35}},
       {?wxSTC_ERLANG_VARIABLE, {150,100,40}},
       {?wxSTC_ERLANG_NUMBER,   {5,5,100}},
       {?wxSTC_ERLANG_KEYWORD,  {130,40,172}},
       {?wxSTC_ERLANG_STRING,   {170,45,132}},
       {?wxSTC_ERLANG_OPERATOR, {30,0,0}},
       {?wxSTC_ERLANG_ATOM,     {0,0,0}},
       {?wxSTC_ERLANG_FUNCTION_NAME, {64,102,244}},
       {?wxSTC_ERLANG_CHARACTER,{236,155,172}},
       {?wxSTC_ERLANG_MACRO,    {40,144,170}},
       {?wxSTC_ERLANG_RECORD,   {40,100,20}},
       {?wxSTC_ERLANG_SEPARATOR,{0,0,0}},
       {?wxSTC_ERLANG_NODE_NAME,{0,0,0}},
       {?wxSTC_STYLE_LINENUMBER,{100,100,100}}
      ],
    
  %% Set the font and style
  SetStyle = fun({Style, Color}) ->
	       wxStyledTextCtrl:styleSetForeground(Editor, Style, Color)
       end,

  [SetStyle(Style) || Style <- Styles],
  ok.
  
%% @doc Get the cursor position in the editor
%% @doc x=line no, y=col no.
-spec editor:get_x_y(Editor) -> Result when
  Editor :: wxStyledTextCtrl:wxStyledTextCtrl(),
  Result :: {integer(), integer()}.
get_x_y(Editor) ->
  LineNo = wxStyledTextCtrl:getCurrentLine(Editor),
  Pos = wxStyledTextCtrl:getCurrentPos(Editor) + 1,
  ColNo = Pos - wxStyledTextCtrl:positionFromLine(Editor, LineNo),
  {LineNo+1, ColNo}.
  
%% @doc Adjust the width of the line_number margin if necessary,
%% @doc dynamically (it doesn't currently decrease).
%% @private
-spec adjust_margin_width(Editor) -> Result when
  Editor :: wxStyledTextCtrl:wxStyledTextCtrl(),
  Result :: 'ok'.
adjust_margin_width(Editor) ->
  Lns = wxStyledTextCtrl:getLineCount(Editor),
  NewWidth = wxStyledTextCtrl:textWidth(Editor, ?wxSTC_STYLE_LINENUMBER, integer_to_list(Lns)),
  Cw = wxStyledTextCtrl:getMarginWidth(Editor, 0),
  if
    NewWidth >= Cw ->
      %% Add an extra characters width
      W = wxStyledTextCtrl:textWidth(Editor, ?wxSTC_STYLE_LINENUMBER, 
               integer_to_list(Lns) ++ " "),
      wxStyledTextCtrl:setMarginWidth(Editor, 0, W);
    true -> ok
  end,
  ok.  
  
%%%%%%%%%%%%%%%%%%%%%%
%%%%% Client API %%%%%

-spec update_style(Editor, Font) -> ok when
  Editor :: wxStyledTextCtrl:wxStyledTextCtrl(),
  Font :: wxFont:wxFont().
update_style(Editor, Font) ->
  wxStyledTextCtrl:styleSetFont(Editor, ?wxSTC_STYLE_DEFAULT, Font),
  %% Update the margin size
  adjust_margin_width(Editor),
  ok.

%% @doc Toggles wordwrap in the editor  
word_wrap(Server) -> %% Param is the wx_object handle from the original call to start
  io:format("WORDWRAP~n", []),
  io:format("PID: ~p~n", [wx_object:get_pid(Server)]),
  %% Make a call to the wx server, which call handle_call()
  State = wx_object:call(Server, editor),
  io:format("State: ~p~n", [State]).
  
%% Set the zoom

%% @doc Get the outer panels associated editor

  
stop() ->
  io:format("Terminate~n"),
  wx_object:cast(?MODULE, stop).
  