-module(ide_shell).
-include_lib("wx/include/wx.hrl").

-export([load_response/1]).

-export([new/1,
		 init/1, 
		 terminate/2, 
		 code_change/3, 
		 handle_info/2, 
		 handle_call/3, 
		 handle_cast/2, 
		 handle_event/2]).

-behaviour(wx_object).

-define(SHELL_TEXT_BOX, 001).
-define(PROMPT, "> ").

%% The record containing the State.
-record(state, {win, textctrl, input, lastchar, promptcount, wx_env}).

new(Config) ->
	wx_object:start_link({local, ?MODULE}, ?MODULE, Config, []).
	
%% Initialise the server's state
init(Config) ->
	Parent = proplists:get_value(parent, Config),
	Panel  = wxPanel:new(Parent),
	MainSizer = wxBoxSizer:new(?wxVERTICAL),
	wxPanel:setSizer(Panel, MainSizer),
	
  % The style of the text box
	ShellTextBox = wxTextCtrl:new(Panel, ?SHELL_TEXT_BOX, [{style, ?wxDEFAULT bor ?wxTE_MULTILINE}]),
	wxTextCtrl:setInsertionPoint(ShellTextBox, wxTextCtrl:getLastPosition(ShellTextBox)),
	wxWindow:setFont(ShellTextBox, wxFont:new(12, ?wxFONTFAMILY_TELETYPE, ?wxNORMAL, ?wxNORMAL,[])),
	
	wxSizer:add(MainSizer, ShellTextBox, [{flag, ?wxEXPAND},
                                          {proportion, 1}]),
		
  % Connect listener to text box	
	wxTextCtrl:connect(ShellTextBox, char, [{callback, fun(E,O) ->
                                                       handle_char_event(E,O)
                                                     end
                                                     }]),
	
	
	{Panel, #state{win=Panel, 
                 textctrl=ShellTextBox, 
                 input=[], 
                 promptcount=1,
                 wx_env=wx:get_env()}}. %% Maintained at server
	
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
    {reply,{State#state.wx_env,State#state.textctrl},State};
handle_call(Msg, _From, State) ->
    io:format("Got Call ~p~n",[Msg]),
    {reply,ok,State}.

handle_cast(Msg, State) ->
    io:format("Got cast ~p~n",[Msg]),
    {noreply,State}.

%% This is where events are handled %%
handle_event(#wx{event=#wxClose{}}, State=#state{win=Frame, input=Input}) ->
    ok = wxFrame:setStatusText(Frame, "Closing...",[]),
    {stop, normal, State};

%% Deal with an ENTER keypress immediately following a period (.)
handle_event(#wx{event=#wxKey{type=char, keyCode=13}}, 
             State = #state{win=Frame, textctrl = TextCtrl, input = Input, lastchar = 46}) ->  %% Enter & Full stop
  PromptCount = State#state.promptcount + 1,
  call_parser(Input), %% Input contains all unparsed input
  {noreply, State#state{input=[], lastchar=13, promptcount=PromptCount}};
    
%% Deal with ENTER
handle_event(#wx{event=#wxKey{type=char, keyCode=13}}, State = #state{win=Frame, textctrl = TextCtrl, input = Input}) -> 
  wxTextCtrl:writeText(TextCtrl, get_prompt(State#state.promptcount)),
  {noreply, State#state{input=Input++"\n"}};

    
%% Deal with BACKSPACE
handle_event(#wx{event=#wxKey{type=char, keyCode=8}}, State=#state{textctrl = TextCtrl, input = Input, promptcount = PromptCount}) ->
	Prompt = integer_to_list(PromptCount),
	{_, X, Y} = wxTextCtrl:positionToXY(TextCtrl, wxTextCtrl:getLastPosition(TextCtrl)),
	case length(Prompt)+2 =:= X of
		true ->
			{noreply, State};
		false ->
			LastPos = wxTextCtrl:getLastPosition(TextCtrl),
			wxTextCtrl:remove(TextCtrl, LastPos-1, LastPos),
			wxTextCtrl:setInsertionPointEnd(TextCtrl),
			LastChar = wxTextCtrl:getLastPosition(TextCtrl),
			
			NewInput = lists:delete(lists:last(Input), Input),
			{noreply, State#state{input=NewInput ,lastchar=LastChar}}
	end;
    
handle_event(#wx{event=#wxKey{type=char, keyCode=127}}, State=#state{win=Frame, textctrl = TextCtrl, input = Input}) -> 
    {noreply, State};
    

%% Now just deal with any char
handle_event(#wx{event=#wxKey{type=char, keyCode=KeyCode}}, State=#state{win=Frame, textctrl = TextCtrl, input = Input}) ->
  % Update the state
  NewInput = Input ++ [KeyCode], %% !!!! REVERSE THIS LIST !!!!!!
  wxTextCtrl:setInsertionPoint(TextCtrl, wxTextCtrl:getLastPosition(TextCtrl)),
  wxTextCtrl:writeText(TextCtrl, [KeyCode]),  
  {noreply, State#state{input=NewInput, lastchar=KeyCode}}.
    
code_change(_, _, State) ->
  {stop, not_yet_implemented, State}.

terminate(_Reason, _State) ->
  io:format("TERMINATION SHELL~n"),
  ok.


%% =====================================================================
%% Asynchronous event callbacks

%% Arrow keys
handle_char_event(#wx{obj=Console, event=#wxKey{type=char, keyCode=?WXK_UP}},O) ->
  io:format("Char Event: ~p~n",[O]);
handle_char_event(#wx{event=#wxKey{type=char, keyCode=?WXK_DOWN}},O) ->
  io:format("Char Event: ~p~n",[O]);
handle_char_event(#wx{obj=Console, event=#wxKey{type=char, keyCode=?WXK_LEFT}},O) ->
  prompt_limit(O, Console);
handle_char_event(#wx{event=#wxKey{type=char, keyCode=?WXK_RIGHT}},O) ->
  io:format("Char Event: ~p~n",[O]),
  wxEvent:skip(O);
  
%% Deal with DELETE
handle_char_event(#wx{obj=Console, event=#wxKey{type=char, keyCode=8}},O) -> 
  prompt_limit(O, Console);
  
%% Deal with CHAR
handle_char_event(#wx{event=#wxKey{type=char, keyCode=KeyCode}},O) -> 
  io:format("Char Event: ~p~n",[O]),
  wxEvent:skip(O);
  
%% Catchall  
handle_char_event(E,O) ->
  io:format("Event: ~p~n Object: ~p~n", [E,O]).

prompt_limit(Event, Console) ->
  {_,X,Y} = wxTextCtrl:positionToXY(Console, wxTextCtrl:getInsertionPoint(Console)),
  Limit = X-1,
  case get_prompt_length(wxTextCtrl:getLineText(Console, Y)) of
    Limit ->
      wxEvent:stopPropagation(Event);
    _ ->
      wxEvent:skip(Event)
  end.

%% =====================================================================
%% @doc

call_parser(Message) ->
  parser:parse_input(Message).


%% =====================================================================
%% @doc
  
load_response(Response) ->
  {Env, Tc} = wx_object:call(?MODULE, text_ctrl),
  wx:set_env(Env),
  wxTextCtrl:writeText(Tc, Response).


%% =====================================================================
%% @doc
    
get_prompt(Count) ->
    I = integer_to_list(Count),
    "\n"++I++?PROMPT.


%% =====================================================================
%% @doc
    
wget_prompt_length(Count) ->
    I = integer_to_list(Count),
    length(I) + 2.


%% =====================================================================
%% @doc

get_prompt_length(Line) ->
	prompt_length(Line, 0).
prompt_length([Char|String], Count) ->
	case Char of
		62 -> % the prompt char
			Count + 1;
		_ -> 
			prompt_length(String, Count + 1)
	end.









