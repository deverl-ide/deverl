%% =====================================================================
%% @author
%% @copyright
%% @title
%% @version
%% @doc This module initalises and manages the port for the console.
%% @end
%% =====================================================================

-module(console_port).

%% gen_server
-export([init/1, 
         handle_call/3,
         handle_cast/2, 
         handle_info/2,
         code_change/3,
         terminate/2]).

%% API
-export([start/0, 
				 eval/1, 
				 close_port/0]).

%% Server state
-record(state, {port :: port(),
                respond :: boolean()
                }).


%% =====================================================================
%% Client API
%% =====================================================================

%% =====================================================================
%% @doc 

start()->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% =====================================================================
%% @doc

eval(Message) ->
	eval(Message, true).
eval(Message, Respond) ->
	gen_server:call(?MODULE, {call, Message, Respond}).
  
% flush_buffer() ->
%   gen_server:call(?MODULE, flush_buffer).
%   
% buffer_responses(Bool) ->
%   gen_server:call(?MODULE, {buffer_responses, Bool}).
	
%% =====================================================================
%% @doc Close the port.
%% Don't attempt to write/read from the port after this!

close_port() ->
  ?MODULE ! {self(), close}.
	
	
%% =====================================================================
%% Callback functions
%% =====================================================================
   
init(Args) ->
  %% io:format("STARTING PORT~n"),
	% process_flag(trap_exit, true), %% Die when the parent process dies
	{Path, Options} = case os:type() of
		{win32,_} ->
			{"C:\\Program Files\\erl5.10.3\\erts-5.10.3\\bin\\erl", [use_stdio]};
    {_,darwin} ->
      {"/usr/local/lib/erlang/erts-5.10.1/bin/erl", [use_stdio, exit_status]}; %% For Tom, for now.
		_Other ->
			{"/usr/local/lib/erlang/erts-5.10.2/bin/erl", [use_stdio, exit_status]}
	end,
	try open(Path, Options) of
		Port -> 
      %% io:format("OPENING PORT: ~p~n", [Port]),
			{ok, #state{port=Port, respond=false}}
	catch
		_:_ ->
      %% io:format("COULD NOT OPEN PORT~n"),
			{stop, no_port}
	end.

handle_call({call, Msg, Respond}, _From, #state{port=Port}=State) ->
  %% io:format("CALLING PORT: ~p~n", [Port]),
  %% io:format("SERVER STATE: ~p~n", [State]),
  port_command(Port, Msg),
	{reply, ok, State#state{respond=Respond}}.
% handle_call(flush_buffer, _From, #state{queue=Queue}=State) ->
%   lists:map(fun console_parser:parse_response/1, Queue),
%   {reply, ok, State#state{queue=[]}};
% handle_call({buffer_responses, Bool}, _From, #state{port=Port}=State) ->
%   {reply, response, State#state{buffer_responses=Bool}}.
	
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({_From, close}, State) ->
	%% io:format("PORT CLOSED INFO 1~n"),
  {stop, {port_closed, quit}, State};
handle_info({Port,{exit_status,Status}}, State) ->
	%% io:format("PORT CLOSED INFO 2~n"),
  % {stop, {port_terminated, ok}, State};
  {stop, {port_terminated, quit}, State};
handle_info({'EXIT', Port, Reason}, #state{port=Port}=State) ->
	%% io:format("PORT CLOSED~n"),
  {stop, {port_terminated, Reason}, State};
handle_info({_Port, {data, Response}}, State=#state{respond=Respond}) ->
	case Respond of
		false -> 
      %% io:format("Buffered response: ~p~n", [Response]),
      % State#state{queue=[Response | Queue]}
      ok;
		true -> 
      %% io:format("Non-buffered response: ~p~n", [Response]),
			console_parser:parse_response(Response)
	end,
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate({port_terminated, _Reason}, _State) ->
	io:format("TERMINATE CONSOLE YEAHHHH~n"),
  ok;
terminate(_Reason, #state{port=Port}) ->
	io:format("TERMINATE CONSOLE~n"),
  port_close(Port),
	ok.
		

%% =====================================================================
%% Internal functions
%% =====================================================================		

%% =====================================================================
%% @doc Open the port.
%% @throws 
%% @private

open(Path, Options) ->
	try open_port({spawn_executable, Path}, Options) of
		Port -> Port
	catch
		_:_ ->
			throw(no_port)
	end.