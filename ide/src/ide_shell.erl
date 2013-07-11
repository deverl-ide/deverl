-module(ide_shell).
-include_lib("wx/include/wx.hrl").

-export([new/1, init/1, terminate/2, code_change/3, 
		 handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).

-behaviour(wx_object).

-define(SHELL_TEXT_BOX, 001).
-define(PROMPT, "> ").

%% The record containing the State.
-record(state, {win, textctrl, input, lastchar, promptcount}).

new(Config) ->
	wx_object:start({local, ?MODULE}, ?MODULE, Config, []).
	
%% Initialise the server's state
init(Config) ->
	Parent = proplists:get_value(parent, Config),
	Panel  = wxPanel:new(Parent),
	MainSizer = wxBoxSizer:new(?wxVERTICAL),
	wxPanel:setSizer(Panel, MainSizer),
	
  % The style of the text box
	ShellTextBox = wxTextCtrl:new(Panel, ?SHELL_TEXT_BOX, [{style, ?wxDEFAULT bor ?wxTE_MULTILINE}]),
	wxTextCtrl:writeText(ShellTextBox, "1" ?PROMPT),
	wxTextCtrl:setInsertionPoint(ShellTextBox, wxTextCtrl:getLastPosition(ShellTextBox)),
  % wxWindow:setForegroundColour(ShellTextBox, ?wxWHITE),
  % wxWindow:setBackgroundColour(ShellTextBox, ?wxBLACK),
	wxWindow:setFont(ShellTextBox, wxFont:new(12, ?wxFONTFAMILY_TELETYPE, ?wxNORMAL, ?wxNORMAL,[])),
	
	wxSizer:add(MainSizer, ShellTextBox, [{flag, ?wxEXPAND},
                                          {proportion, 1}]),
		
  % Connect listener to text box	
	wxTextCtrl:connect(ShellTextBox, char),
	
	
	{Panel, #state{win=Panel, textctrl=ShellTextBox, input=[], promptcount=1}}. %% Maintained at server
	
%%%%% Callbacks %%%%%
%% These are all called from the server %%
handle_info({'EXIT',_, wx_deleted}, State) ->
    {noreply,State};
handle_info({'EXIT',_, shutdown}, State) ->
    {noreply,State};
handle_info({'EXIT',_, normal}, State) ->
    {noreply,State};
handle_info(Msg, State) ->
    io:format("Got Info ~p~n",[Msg]),
    {noreply,State}.

handle_call(text_ctrl, _From, State) ->
    {reply,State#state.textctrl,State};
handle_call(Msg, _From, State) ->
    io:format("Got Call ~p~n",[Msg]),
    {reply,ok,State}.

handle_cast(Msg, State) ->
    io:format("Got cast ~p~n",[Msg]),
    {noreply,State}.

%% This is where events are handled %%
handle_event(#wx{event=#wxClose{}}, State = #state{win=Frame, input=Input}) ->
    ok = wxFrame:setStatusText(Frame, "Closing...",[]),
    {stop, normal, State};

%% Deal with an ENTER keypress immediately following a period (.)
handle_event(#wx{event=#wxKey{type=char, keyCode=13}}, 
             State = #state{win=Frame, textctrl = TextCtrl, input = Input, lastchar = 46}) ->  %% Enter & Full stop
    PromptCount = State#state.promptcount + 1,
    wxTextCtrl:writeText(TextCtrl, "\n" ++ Input), %% Input contains all unparsed input
    wxTextCtrl:writeText(TextCtrl, get_prompt(PromptCount)),
    call_parser(Input),
    {noreply, State#state{input=[], lastchar=13, promptcount=PromptCount}};
    
%% Deal with ENTER
handle_event(#wx{event=#wxKey{type=char, keyCode=13}}, State = #state{win=Frame, textctrl = TextCtrl, input = Input}) -> 
    wxTextCtrl:writeText(TextCtrl, get_prompt(State#state.promptcount)),
    {noreply, State#state{input=Input++"\n"}};
    
%% Now just deal with any char
handle_event(#wx{event=#wxKey{type=char, keyCode=KeyCode}}, State = #state{win=Frame, textctrl = TextCtrl, input = Input}) ->
  % Update the state
    NewInput = Input ++ [KeyCode],
    wxTextCtrl:writeText(TextCtrl, [KeyCode]),
    wxTextCtrl:setInsertionPoint(TextCtrl, wxTextCtrl:getLastPosition(TextCtrl)),
    {noreply, State#state{input=NewInput, lastchar=KeyCode}}.
    
code_change(_, _, State) ->
    {stop, not_yet_implemented, State}.

terminate(_Reason, _State) ->
    wx:destroy().
    
call_parser(Message) ->
  io:format("MESSAGE: ~p~n", [Message]),
  %% Convert the user input to an erlang term
  {ok, T, _} = erl_scan:string(Message),
  {ok, Var} = erl_parse:parse_term(T),
  
  % parser:parse(Var).
  port:call_port(Var).
  
load_response(Response) ->
  Tc = wx_object:call(?MODULE, text_ctrl),
  wxTextCtrl:writeText(Tc, Response).
  % wxTextCtrl:writeText(Tc, get_prompt(PromptCount)).
    
get_prompt(Count) ->
    I = integer_to_list(Count),
    "\n"++I++?PROMPT.
