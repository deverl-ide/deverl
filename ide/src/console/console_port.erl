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
         eval/2,
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
  
	
%% =====================================================================
%% @doc Close the port.
%% Don't attempt to write/read from the port after this!

close_port() ->
  ?MODULE ! {self(), close}.
	
	
%% =====================================================================
%% Callback functions
%% =====================================================================
   
init(Args) ->
	{Path, Options} = case os:type() of
		{win32,_} ->
			{"C:\\Program Files\\erl5.10.3\\erts-5.10.3\\bin\\erl", [use_stdio]};
		_Other ->
      {string:strip(os:cmd("which erl"), both, $\n), [use_stdio, exit_status]}
	end,
	try open(Path, Options) of
		Port -> 
			{ok, #state{port=Port, respond=false}}
	catch
		_:_ ->
			{stop, no_port}
	end.

handle_call({call, Msg, Respond}, _From, #state{port=Port}=State) ->
  port_command(Port, Msg),
	{reply, ok, State#state{respond=Respond}}.
	
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({_From, close}, State) ->
  {stop, normal, State};
handle_info({'EXIT', Port, Reason}, #state{port=Port}=State) ->
  {stop, {port_terminated, Reason}, State};
handle_info({_Port, {data, Response}}, State=#state{respond=Respond}) ->
	case Respond of
		false -> 
      ok;
		true -> 
			console_parser:parse_response(Response)
	end,
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate({port_terminated, _Reason}, _State) ->
  ok;
terminate(_Reason, #state{port=Port}) ->
  % port_close(Port),
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