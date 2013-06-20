-module(ide_shell).
-include_lib("wx/include/wx.hrl").

-export([start/0, start/1, start_link/0, start_link/1, 
	 init/1, terminate/2,  code_change/3,
	 handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).

-behaviour(wx_object).

-define(SHELL_TEXT_BOX, 001).

%% The record containing the State.
-record(state, {win, textctrl, input}).

start() ->
  start([]).
  
start(Args) ->
  wx_object:start(?MODULE, Args, []).
  
start_link() ->
  start_link([]).
  
start_link(Args) ->
  wx_object:start_link(?MODULE, Args, []).
	
%% Initialise the server's state
init(Args) ->
	Server = wx:new(),
	Frame  = wxFrame:new(Server, -1, "Erlang Shell", [{size, {800, 300}}]),
	Panel  = wxPanel:new(Frame),
  
  Input = "> ",
	
  % The style of the text box
	ShellTextBox = wxTextCtrl:new(Panel, ?SHELL_TEXT_BOX, [{style, ?wxTE_MULTILINE}, 
											   {size, {800, 300}}, {value, Input}]),
	wxWindow:setForegroundColour(ShellTextBox, ?wxWHITE),
	wxWindow:setBackgroundColour(ShellTextBox, ?wxBLACK),
	wxWindow:setFont(ShellTextBox, wxFont:new(12, ?wxFONTFAMILY_TELETYPE, ?wxNORMAL, ?wxNORMAL,[])),
	
  % Set sizers
	MainSizer = wxBoxSizer:new(?wxVERTICAL),
	wxSizer:add(MainSizer, ShellTextBox, []),
	wxPanel:setSizer(Panel, MainSizer),
	
	wxFrame:show(Frame),
	
	wxTextCtrl:connect(ShellTextBox, char),
	
	{Frame, #state{win=Frame, textctrl=ShellTextBox, input=Input}}. %% Maintained at server
	
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
%% This is executed where char events are handled
%% First, deal with the the case where the user enters a full stop
handle_event(#wx{event=#wxKey{type=char, keyCode=46}}, State = #state{win=Frame, textctrl = TextCtrl, input = Input}) ->    %% Full stop
    NewInput = Input ++ ".",
    wxTextCtrl:writeText(TextCtrl, "\nYou have entered:\n"),
    wxTextCtrl:writeText(TextCtrl, NewInput),
    {noreply, State#state{input="> "}};
%% We can deal with any key as we have above (i.e. the enter key)
%% Now just deal with any char
handle_event(#wx{event=#wxKey{type=char, keyCode=KeyCode}}, State = #state{win=Frame, textctrl = TextCtrl, input = Input}) ->
    %% This currently treats everything as an ascii character
    Key = io_lib:format("~c", [KeyCode]),
    %% Update the state
    NewInput = Input ++ Key,
    %% Prompt again
    wxTextCtrl:setValue(TextCtrl, NewInput),
    {noreply, State#state{input=NewInput}}.

code_change(_, _, State) ->
    {stop, not_yet_implemented, State}.

terminate(_Reason, _State) ->
    wx:destroy().