-module(shell_svr).

%% gen_server
-export([init/1, 
         handle_call/3,
         handle_cast/2, 
         handle_info/2,
         code_change/3,
         terminate/2]).

%% API
-export([start/0]).

%% Server state
-record(state, {count :: integer(),
								bindings,
								records_ets, %% ETS table of records
                history}).


%% =====================================================================
%% Client API
%% =====================================================================

%% =====================================================================
%% @doc 

start() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% =====================================================================
%% @doc

eval(Cmd) ->
  gen_server:call(?MODULE, {eval, Cmd}).
	
	
%% =====================================================================
%% Callback functions
%% =====================================================================
   
init() ->
  %% Init bindings
  Bs = erl_eval:new_bindings(),
  
  %% Use an Ets table for record definitions. It takes too long to
  %% send a huge term to and from the evaluator. Ets makes it
  %% possible to have thousands of record definitions.
  RT = ets:new(?RECORDS, [public,ordered_set]),
  _ = initiate_records(Bs, RT),

  process_flag(trap_exit, true),

  State = #state{count = 0,
                 bindings = Bs,
                 records_ets = RT,
                 history = []
                 }
	end.

handle_call({eval, Cmd}, _From, State=#state{count=N, bindings=Bs, records_ets=RT, history=History}) ->
  
	{reply, ok, State};
	
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({_From, close}, State) ->
  {stop, {port_closed, quit}, State}; 

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
%% @doc