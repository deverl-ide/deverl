-module(ide_eunit_listener).

-behaviour(eunit_listener).

-include_lib("eunit/include/eunit.hrl").

-export([
  start/0,
  start/1
]).

-export([init/1, handle_begin/3, handle_end/3, handle_cancel/3,
	 terminate/2]).
   
-record(state, {
  results :: [{atom(), atom()}], %% {module, pass | fail}
  wx_env
}).
   
start() ->
    start([]).

start(Options) ->
    eunit_listener:start(?MODULE, Options).
    
init(Options) ->
  Env = proplists:get_value(wx_env, Options),
  receive
  	{start, _Reference} ->
	    ok
  end,
  #state{results=[], wx_env=Env}.

handle_begin(test, _Data, State) ->
  State;
handle_begin(_Kind, _Data, State) ->
  State.
    
handle_end(test, Data, State) ->
  {_Mod, Func, _Arity} = proplists:get_value(source, Data),
  Result = case proplists:get_value(status, Data) of
    ok ->
      true;
    _ -> %% error
      false
  end,
  Results = [{Func, Result} | State#state.results],
  State#state{results=Results};
handle_end(group, Data, State) ->
  case proplists:get_value(id, Data) of
    [] -> ide_testpane:show_test_results(State#state.results, State#state.wx_env);
    _ -> ok
  end,
  State;
handle_end(Kind, Data, State) ->
  % io:format("handle end: ~p~n~p~n", [Kind, Data]),
  State.
  
handle_cancel(Kind, Data, State) ->
  % io:format("handle cancel: ~p~n~p~n", [Kind, Data]),
  State.
     
terminate({ok, Data}, _State) ->
  ok;
terminate({error, Reason}, _State) ->
  % fwrite("Internal error: ~P.\n", [Reason, 25]),
  ok.