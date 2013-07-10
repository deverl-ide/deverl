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
	wx_object:start(?MODULE, Config, []).
	
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

%% This is executed where char events are handled.
%% Deal with an ENTER keypress immediately following a period (.)
handle_event(#wx{event=#wxKey{type=char, keyCode=13}}, 
             State = #state{win=Frame, textctrl = TextCtrl, input = Input, lastchar = 46}) ->  %% Enter & Full stop
    PromptCount = State#state.promptcount + 1,
    wxTextCtrl:writeText(TextCtrl, "\nDone."),
    
  % The collected input is now available in Input, ready to be sent to ERTS
    wxTextCtrl:writeText(TextCtrl, get_prompt(PromptCount)),
    {noreply, State#state{input=[], lastchar=13, promptcount=PromptCount}};
    
%% Deal with ENTER
handle_event(#wx{event=#wxKey{type=char, keyCode=13}}, State = #state{win=Frame, textctrl = TextCtrl, input = Input}) -> 
    wxTextCtrl:writeText(TextCtrl, get_prompt(State#state.promptcount)),
    {noreply, State#state{input=Input++"\n"}};
    
%% Now just deal with any char
handle_event(#wx{event=#wxKey{type=char, keyCode=KeyCode}}, State = #state{win=Frame, textctrl = TextCtrl, input = Input}) ->
  % This currently converts everything as an ascii character (ascii need to be filtered out)
    Key = io_lib:format("~c", [KeyCode]),
  % Update the state
    NewInput = Input ++ Key,
    wxTextCtrl:writeText(TextCtrl, Key),
    wxTextCtrl:setInsertionPoint(TextCtrl, wxTextCtrl:getLastPosition(TextCtrl)),
    {noreply, State#state{input=NewInput, lastchar=KeyCode}}.
    
code_change(_, _, State) ->
    {stop, not_yet_implemented, State}.

terminate(_Reason, _State) ->
    wx:destroy().
    
get_prompt(Count) ->
    I = integer_to_list(Count),
    "\n"++I++?PROMPT.