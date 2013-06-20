%% editor.erl
%% Simple frame that loads an editor object

-module(editor).

-export([start/1, init/1, terminate/2,  code_change/3,
	       handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).

-include_lib("wx/include/wx.hrl").
-behaviour(wx_object).

-define(DEFAULT_FONT_SIZE, 11).
-define(GREY, {100,100,100}).

%% The record containing the State.
-record(state, {win, editor}).

start(Config) ->
    wx_object:start_link(?MODULE, Config, []).

%% init(Args) should return 
%% {wxObject, State} | {wxObject, State, Timeout} | ignore | {stop, Reason}
init(Config) ->
  
  %% Testing events
  wx:new(),
  Frame = wxFrame:new(wx:null(), ?wxID_ANY, "Main Frame", [{size,{600,400}}]),
  Panel = wxPanel:new(Frame),
  %% End tests
  
  % Parent = proplists:get_value(parent, Config),
  % Panel = wxPanel:new(Parent),
  
  Sizer = wxBoxSizer:new(?wxVERTICAL),
  wxPanel:setSizer(Panel, Sizer),
  Editor = wxStyledTextCtrl:new(Panel), 
  wxSizer:add(Sizer, Editor, [{flag, ?wxEXPAND},
                              {proportion, 1}]),
  
  %% Testing events
  wxFrame:show(Frame),                          
  %% End tests                 
                              
  %% Editor styles
  Font = wxFont:new(?DEFAULT_FONT_SIZE, ?wxFONTFAMILY_TELETYPE, ?wxNORMAL, ?wxNORMAL,[]),
  wxWindow:setFont(Editor, Font),
  wxStyledTextCtrl:styleClearAll(Editor),
  wxStyledTextCtrl:styleSetFont(Editor, ?wxSTC_STYLE_DEFAULT, Font),
  wxStyledTextCtrl:setLexer(Editor, ?wxSTC_LEX_ERLANG),
  wxStyledTextCtrl:setMargins(Editor, 4, 4),
  %% Margins !!!!! DEFINE THESE MARGINS AS MACROS !!!!!!
  wxStyledTextCtrl:setMarginType(Editor, 0, ?wxSTC_MARGIN_NUMBER),
  MW = wxStyledTextCtrl:textWidth(Editor, ?wxSTC_STYLE_LINENUMBER, "9"),
  wxStyledTextCtrl:setMarginWidth(Editor, 0, MW*2),
  % wxStyledTextCtrl:setMarginWidth(Editor, 1, 0),
  
  wxStyledTextCtrl:styleSetForeground (Editor, ?wxSTC_STYLE_LINENUMBER, {75, 75, 75}),
  wxStyledTextCtrl:styleSetBackground (Editor, ?wxSTC_STYLE_LINENUMBER, {220, 220, 220}),
  %% Folding
  wxStyledTextCtrl:setMarginType (Editor, 1, ?wxSTC_MARGIN_SYMBOL),
  wxStyledTextCtrl:setMarginWidth(Editor, 1, 15),
  wxStyledTextCtrl:setMarginMask (Editor, 1, ?wxSTC_MASK_FOLDERS),
  wxStyledTextCtrl:styleSetBackground(Editor, 1, {200, 200, 200} ),
  wxStyledTextCtrl:setMarginSensitive(Editor, 1, true), %% Makes margin sensitive to mouse clicks
  
  
  wxStyledTextCtrl:markerDefine (Editor, ?wxSTC_MARKNUM_FOLDER, ?wxSTC_MARK_ARROW ),
  wxStyledTextCtrl:markerSetForeground (Editor, ?wxSTC_MARKNUM_FOLDER, ?GREY),
  wxStyledTextCtrl:markerSetBackground (Editor, ?wxSTC_MARKNUM_FOLDER, ?GREY),
        
  wxStyledTextCtrl:markerDefine (Editor, ?wxSTC_MARKNUM_FOLDEROPEN, ?wxSTC_MARK_ARROWDOWN),
  wxStyledTextCtrl:markerSetForeground (Editor, ?wxSTC_MARKNUM_FOLDEROPEN, ?GREY),
  wxStyledTextCtrl:markerSetBackground (Editor, ?wxSTC_MARKNUM_FOLDEROPEN, ?GREY),
        
  wxStyledTextCtrl:markerDefine (Editor, ?wxSTC_MARKNUM_FOLDERSUB, ?wxSTC_MARK_EMPTY),
  wxStyledTextCtrl:markerSetForeground (Editor, ?wxSTC_MARKNUM_FOLDERSUB, ?GREY),
  wxStyledTextCtrl:markerSetBackground (Editor, ?wxSTC_MARKNUM_FOLDERSUB, ?GREY),
        
  wxStyledTextCtrl:markerDefine (Editor, ?wxSTC_MARKNUM_FOLDEREND, ?wxSTC_MARK_ARROW),
  wxStyledTextCtrl:markerSetForeground (Editor, ?wxSTC_MARKNUM_FOLDEREND, ?GREY),
  wxStyledTextCtrl:markerSetBackground (Editor, ?wxSTC_MARKNUM_FOLDEREND, {0,0,0}),
        
  wxStyledTextCtrl:markerDefine (Editor, ?wxSTC_MARKNUM_FOLDEROPENMID, ?wxSTC_MARK_ARROWDOWN),
  wxStyledTextCtrl:markerSetForeground (Editor, ?wxSTC_MARKNUM_FOLDEROPENMID, ?GREY),
  wxStyledTextCtrl:markerSetBackground (Editor, ?wxSTC_MARKNUM_FOLDEROPENMID, {255,255,255}),
        
  wxStyledTextCtrl:markerDefine (Editor, ?wxSTC_MARKNUM_FOLDERMIDTAIL, ?wxSTC_MARK_EMPTY),
  wxStyledTextCtrl:markerSetForeground (Editor, ?wxSTC_MARKNUM_FOLDERMIDTAIL, ?GREY),
  wxStyledTextCtrl:markerSetBackground (Editor, ?wxSTC_MARKNUM_FOLDERMIDTAIL, ?GREY),
        
  wxStyledTextCtrl:markerDefine (Editor, ?wxSTC_MARKNUM_FOLDERTAIL, ?wxSTC_MARK_EMPTY),
  wxStyledTextCtrl:markerSetForeground (Editor, ?wxSTC_MARKNUM_FOLDERTAIL, ?GREY),
  wxStyledTextCtrl:markerSetBackground (Editor, ?wxSTC_MARKNUM_FOLDERTAIL, ?GREY),
  
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
    {reply,ok,State}.

handle_cast(Msg, State) ->
    io:format("Got cast ~p~n",[Msg]),
    {noreply,State}.

handle_event(#wx{event=#wxStyledText{type=stc_modified}}, State = #state{editor=Editor}) ->
    io:format("Text modified. ~n", []),
    {noreply, State};
handle_event(#wx{event=#wxStyledText{type=stc_marginclick, position = Pos, margin = Margin} = E},
             State = #state{editor=Editor}) ->
    Ln = wxStyledTextCtrl:lineFromPosition(Editor, Pos),
    Fl = wxStyledTextCtrl:getFoldLevel(Editor, Ln),
    io:format("Margin ~p clicked at position ~p on line number ~p.~n", [Margin, Pos, Ln]),
    % io:format("~p~n", [E]),
    case Margin of
      1 when Ln > 0, Fl > 0 ->
        io:format("Now fold:~n", [])    
    end,
    {noreply, State}.
    
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