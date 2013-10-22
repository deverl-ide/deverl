%% =====================================================================
%% @author
%% @copyright
%% @title
%% @version
%% @doc This module initalises and manages the port for the console.
%% It will buffer any messages received from the port when 
%% buffer_responses is set to true. These can be received when ready
%% using flush_buffer/0. 
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
				 call_port/1, 
				 buffer_responses/1,
				 flush_buffer/0,
				 close_port/0]).

%% Server state
-record(state, {port :: port(),
								buffer_responses :: boolean(),
								queue :: list()}).


%% =====================================================================
%% Client API
%% =====================================================================

%% =====================================================================
%% @doc 

start()->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% =====================================================================
%% @doc

call_port(Message) ->
	gen_server:call(?MODULE, {call, Message}).

flush_buffer() ->
	gen_server:call(?MODULE, flush_buffer).
	
buffer_responses(Bool) ->
	gen_server:call(?MODULE, {buffer_responses, Bool}).
	
%% =====================================================================
%% @doc Close the port.
%% Don't attempt to write/read from the port after this!

close_port() ->
  ?MODULE ! {self(), close}.
	
	
%% =====================================================================
%% Callback functions
%% =====================================================================
   
init(Args) ->
	process_flag(trap_exit, true), %% Die when the parent process dies
	{Path, Options} = case os:type() of
		{win32,_} ->
			{"C:\\Program Files\\erl5.10.2\\erts-5.10.2\\bin\\erl", [use_stdio]};
		_Other ->
			{"/usr/local/lib/erlang/erts-5.10.2/bin/erl", [use_stdio]}
	end,
	try open(Path, Options) of
		Port -> 
			{ok, #state{port=Port, buffer_responses=true, queue=[]}}
	catch
		_:_ ->
			{stop, no_port}
	end.

handle_call({call, Msg}, _From, #state{port=Port}=State) ->
  port_command(Port, Msg),
	{reply, ok, State};
handle_call(flush_buffer, _From, #state{queue=Queue}=State) ->
	lists:map(fun console_parser:parse_response/1, Queue),
	{reply, ok, State#state{queue=[]}};
handle_call({buffer_responses, Bool}, _From, #state{port=Port}=State) ->
	{reply, response, State#state{buffer_responses=Bool}}.
	
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({'EXIT', Port, Reason}, #state{port = Port} = State) ->
  {stop, {port_terminated, Reason}, State};
handle_info({_Port, {data, Response}}, State=#state{buffer_responses=Buffer, queue=Queue}) ->
	NewState = case Buffer of
		true -> 
			Q = [Response | Queue],
			State#state{queue=Q};
		false -> 
			console_parser:parse_response(Response), State
	end,
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate({port_terminated, _Reason}, _State) ->
  ok;
terminate(_Reason, #state{port=Port}) ->
	io:format("TERMINATE CONSOLE~n"),
  port_close(Port).
		

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