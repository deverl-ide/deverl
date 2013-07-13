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
-record(state, {win, textctrl, wx_env}).

new(Config) ->
	wx_object:start_link({local, ?MODULE}, ?MODULE, Config, []).
	
%% Initialise the server's state
init(Config) ->
	Parent = proplists:get_value(parent, Config),
  
  ScrollWin = wxScrolledWindow:new(Parent, []),
	MainSizer = wxBoxSizer:new(?wxVERTICAL),
	wxWindow:setSizer(ScrollWin, MainSizer),
	
	ShellTextBox = wxTextCtrl:new(ScrollWin, ?SHELL_TEXT_BOX, [{style, ?wxTE_MULTILINE}]),
	
  wxWindow:setFont(ShellTextBox, wxFont:new(12, ?wxFONTFAMILY_TELETYPE, 
                                                ?wxNORMAL, 
                                                ?wxNORMAL,[])),
	
	wxSizer:add(MainSizer, ShellTextBox, [{flag, ?wxEXPAND},
                                          {proportion, 1}]),
                                          
		
  % Connect listener to text box	
	wxTextCtrl:connect(ShellTextBox, char, [{callback, fun(E,O) ->
                                                       handle_char_event(E,O)
                                                     end
                                                     }]),
		
	{ScrollWin, #state{win=ScrollWin, 
                 textctrl=ShellTextBox, 
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
handle_event(#wx{event=#wxClose{}}, State=#state{win=Frame}) ->
    ok = wxFrame:setStatusText(Frame, "Closing...",[]),
    {stop, normal, State}.
    
code_change(_, _, State) ->
  {stop, not_yet_implemented, State}.

terminate(_Reason, _State) ->
  io:format("TERMINATE SHELL~n"),
  ok.


%% =====================================================================
%% Asynchronous event callbacks

%% ENTER
handle_char_event(#wx{obj=Console, event=#wxKey{type=char, keyCode=13}}, O) -> 
  LineText = wxTextCtrl:getLineText(Console, wxTextCtrl:getNumberOfLines(Console) - 1),
  Input = string:substr(LineText, get_prompt_length(LineText)+2),
  case length(Input) of
    0 -> %% single enter key pressed, nothing else
      %% Note we have manually insert the prompt because sending a single newline '\n'
      %% to the port results in no response. It is the terminal that redraws the prompt
      %% and not the ERTS. Same goes with history (up arrow).
      %% The port will only respond through stdout when a '.' is received.
      prompt_2_console(Console, LineText),
      call_parser(Input), %% send anyway, so any error contains the correct position integer
      wxEvent:stopPropagation(O);
    _ ->
      Last = fun(46) -> %% keycode 46 = '.'
                  %% Deal with the case where several '.'s are entered, '...'
                   prompt_or_not(length(Input), Input, Console, LineText, O);
                 (_) -> %% write the newline and prompt to the console
                   prompt_2_console(Console, LineText)
              end,
      Last(lists:last(Input)),     
      call_parser(Input)
  end;

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
  
%% Backspace
handle_char_event(#wx{obj=Console, event=#wxKey{type=char, keyCode=8}},O) -> 
  prompt_limit(O, Console);
  
%% CHAR
handle_char_event(#wx{obj=Console, event=#wxKey{type=char, keyCode=KeyCode}},O) -> 
  {_,X,Y} = wxTextCtrl:positionToXY(Console, wxTextCtrl:getInsertionPoint(Console)),
  LastLine = wxTextCtrl:getNumberOfLines(Console) - 1,
  PromptLen = get_prompt_length(wxTextCtrl:getLineText(Console, LastLine)),
  case (X > PromptLen) and (Y =:= LastLine) of
    true -> ok;
    false ->
      wxTextCtrl:setInsertionPointEnd(Console)
  end,
  wxEvent:skip(O);
  
%% Catchall  
handle_char_event(E,O) ->
  io:format("Event: ~p~n Object: ~p~n", [E,O]).


%% =====================================================================
%% @doc Write a newline plus the repeated prompt to the console.
  
prompt_2_console(Console, LineText) ->
  wxTextCtrl:writeText(Console, io_lib:nl()),
  wxTextCtrl:writeText(Console, get_current_prompt(LineText)).
  
  
%% =====================================================================
%% @doc Determine whether we need to manually prompt_2_console()
%%
%% @private

prompt_or_not(N,Input,Console,LineText,Ev) when N>1 ->
  Penult = lists:nth(length(Input)-1, Input),
  if 
    Penult =:= 46 ->
      prompt_2_console(Console, LineText),
      wxEvent:stopPropagation(Ev);
    true -> 
      wxEvent:skip(Ev)
  end;
prompt_or_not(_,Input,_,_,Ev) ->
  wxEvent:skip(Ev).

%% =====================================================================
%% @doc


prompt_limit(Event, Console) ->
  {_,X,_} = wxTextCtrl:positionToXY(Console, wxTextCtrl:getInsertionPoint(Console)),
  Lines = wxTextCtrl:getNumberOfLines(Console),
  Limit = X-1,
  case get_prompt_length(wxTextCtrl:getLineText(Console, Lines-1)) of
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
%% @doc Get the prompt i.e. '2> ' from the given line.

get_current_prompt(Line) ->
  string:substr(Line, 1, get_prompt_length(Line) + 1).


%% =====================================================================
%% @doc Get the length of the prompt.

get_prompt_length(Line) ->
	get_prompt_length(Line, 1).
  
get_prompt_length([Char|String], Count) ->
	case Char of
		62 -> % the prompt char '>'
			Count;
		_ -> 
			get_prompt_length(String, Count + 1)
	end.