%% editor.erl
%% Simple frame that loads an editor object

-module(editor).

-export([word_wrap/1]).

-export([start/1, init/1, terminate/2,  code_change/3,
         handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).

-include_lib("wx/include/wx.hrl").
-behaviour(wx_object).

-define(DEFAULT_FONT_SIZE, 11).

%% Colours
-define(GREY, {25,0,0}).
-define(SELECTION, {200,255,255}).

-define(LEFT_MARGIN_WIDTH, 6).
-define(RIGHT_MARGIN_WIDTH, 6).
-define(MARGIN_NUMBER_PADDING, "   ").
-define(MARGIN_NUMBER_TEXT_REDUCTION, 2). %% The size (pts) to reduce margin text by

%% The record containing the state maintained by the server
-record(state, {win, editor}).

start(Config) ->
  wx_object:start_link(?MODULE, Config, []).

%% init(Args) should return 
%% {wxObject, State} | {wxObject, State, Timeout} | ignore | {stop, Reason}
init(Config) ->
  
  %% Testing events
  % wx:new(),
  % Frame = wxFrame:new(wx:null(), ?wxID_ANY, "Main Frame", [{size,{600,400}}]),
  % Panel = wxPanel:new(Frame),
  %% End tests
  
  Parent = proplists:get_value(parent, Config),
  Panel = wxPanel:new(Parent),
  
  Sizer = wxBoxSizer:new(?wxVERTICAL),
  wxPanel:setSizer(Panel, Sizer),
  Editor = wxStyledTextCtrl:new(Panel), 
  wxSizer:add(Sizer, Editor, [{flag, ?wxEXPAND},
                              {proportion, 1}]),
  
  %% Testing events
  % wxFrame:show(Frame),                          
  %% End tests                
                              
  %% Editor styles
  Font = wxFont:new(?DEFAULT_FONT_SIZE, ?wxFONTFAMILY_TELETYPE, ?wxNORMAL, ?wxNORMAL,[]),
  wxWindow:setFont(Editor, Font),
  
  wxStyledTextCtrl:styleClearAll(Editor),
  wxStyledTextCtrl:styleSetFont(Editor, ?wxSTC_STYLE_DEFAULT, Font),
  wxStyledTextCtrl:setLexer(Editor, ?wxSTC_LEX_ERLANG),
  
  wxStyledTextCtrl:setSelBackground(Editor, true, ?SELECTION),
  wxStyledTextCtrl:setSelectionMode(Editor, ?wxSTC_SEL_LINES),
  
  %% Margins !!!!! DEFINE THESE MARGINS AS MACROS !!!!!!
  wxStyledTextCtrl:setMargins(Editor, ?LEFT_MARGIN_WIDTH, ?RIGHT_MARGIN_WIDTH), %% Left and right of text
  wxStyledTextCtrl:setMarginType(Editor, 0, ?wxSTC_MARGIN_NUMBER),
  wxStyledTextCtrl:styleSetSize(Editor, ?wxSTC_STYLE_LINENUMBER, (?DEFAULT_FONT_SIZE - ?MARGIN_NUMBER_TEXT_REDUCTION)),
  MW = wxStyledTextCtrl:textWidth(Editor, ?wxSTC_STYLE_LINENUMBER, ?MARGIN_NUMBER_PADDING ++ "9"),
  wxStyledTextCtrl:setMarginWidth(Editor, 0, MW),
  wxStyledTextCtrl:styleSetForeground (Editor, ?wxSTC_STYLE_LINENUMBER, {75, 75, 75}),
  wxStyledTextCtrl:styleSetBackground (Editor, ?wxSTC_STYLE_LINENUMBER, {220, 220, 220}),
  
  update_styles(Editor, Font),
  
  %% Add the keywords
  wxStyledTextCtrl:setKeyWords(Editor, 0, keywords()),
  
  %% Folding
  wxStyledTextCtrl:setMarginType (Editor, 1, ?wxSTC_MARGIN_SYMBOL),
  wxStyledTextCtrl:setMarginWidth(Editor, 1, 8),
  wxStyledTextCtrl:setMarginMask (Editor, 1, ?wxSTC_MASK_FOLDERS),
  wxStyledTextCtrl:setMarginSensitive(Editor, 1, true), %% Makes margin sensitive to mouse clicks
  
  wxStyledTextCtrl:setProperty(Editor, "fold", "7"),
  wxStyledTextCtrl:setProperty(Editor, "fold.comment", "2"),
  wxStyledTextCtrl:setProperty(Editor, "fold.compact", "2"),
  
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
  
  wxStyledTextCtrl:connect(Editor, stc_marginclick, []),
  wxStyledTextCtrl:connect(Editor, stc_modified, []),
  
  {Panel, #state{win=Panel, editor=Editor}}.

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
    {reply,State#state.editor,State}.

handle_cast(Msg, State) ->
    io:format("Got cast ~p~n",[Msg]),
    {noreply,State}.

handle_event(#wx{event=#wxStyledText{type=stc_modified}}, State = #state{editor=Editor}) ->
    %% Update margin width dynamically
    %% Using the correct event?
    Lns = wxStyledTextCtrl:getLineCount(Editor),
    Nw = wxStyledTextCtrl:textWidth(Editor, ?wxSTC_STYLE_LINENUMBER, ?MARGIN_NUMBER_PADDING ++ integer_to_list(Lns)),
    Cw = wxStyledTextCtrl:getMarginWidth(Editor, 0),
    if
      Nw /= Cw ->
        wxStyledTextCtrl:setMarginWidth(Editor, 0, Nw);
      true -> ok
    end,
    {noreply, State};
handle_event(#wx{event=#wxStyledText{type=stc_marginclick, position = Pos, margin = Margin} = E},
             State = #state{editor=Editor}) ->
    Ln = wxStyledTextCtrl:lineFromPosition(Editor, Pos),
    Fl = wxStyledTextCtrl:getFoldLevel(Editor, Ln),
    % io:format("Margin ~p clicked at position ~p on line number ~p.~n", [Margin, Pos, Ln]),
    % io:format("~p~n", [E]),
    case Margin of
      1 when Ln > 0, Fl > 0 ->
        wxStyledTextCtrl:toggleFold(Editor, Ln);
      _ -> ok
    end,
    {noreply, State}.
    
code_change(_, _, State) ->
    {stop, not_yet_implemented, State}.

terminate(_Reason, _State) ->
    wx:destroy().
    
%%%%% Editor Functions (Internal) %%%%%
keywords() ->
  KWS = ["after" "and" "andalso" "band" "begin" "bnot" 
  "bor" "bsl" "bsr" "bxor" "case" "catch" "cond" "div" 
  "end" "fun" "if" "let" "not" "of" "or" "orelse" 
  "receive" "rem" "try" "when" "xor"],
  lists:flatten([KW ++ " " || KW <- KWS]).

update_styles(Editor, Font) ->
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
       {?wxSTC_ERLANG_NODE_NAME,{0,0,0}}
      ],
    
  %% Set the font and style
  SetStyle = fun({Style, Color}) ->
	       wxStyledTextCtrl:styleSetFont(Editor, Style, Font),
	       wxStyledTextCtrl:styleSetForeground(Editor, Style, Color)
       end,

  [SetStyle(Style) || Style <- Styles],
  ok.
  
%%%%% Interface (Exported) %%%%%
update_style() ->
  %% Change styles
  
  %% Update the margin size (2pts smaller than text)
  ok.

%% @doc Toggles wordwrap in the editor  
word_wrap(Server) -> %% Param is the wx_object handle from the original call to start
  io:format("WORDWRAP~n", []),
  io:format("PID: ~p~n", [wx_object:get_pid(Server)]),
  %% Make a call to the wx server, which call handle_call()
  State = wx_object:call(Server, editor),
  io:format("State: ~p~n", [State]).