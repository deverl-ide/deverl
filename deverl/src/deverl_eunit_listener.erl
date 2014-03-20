%% =====================================================================
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%% 
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%% 
%% You should have received a copy of the GNU General Public License
%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%
%% @author Tom Richmond <tr201@kent.ac.uk>
%% @author Mike Quested <mdq3@kent.ac.uk>
%% @copyright Tom Richmond, Mike Quested 2014
%%
%% @doc Listener receives callbacks whilst Eunit is running.
%% @end
%% =====================================================================

-module(deverl_eunit_listener).

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
  passed :: integer(),
  failed :: integer(),
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
  #state{results=[], wx_env=Env, passed=0, failed=0}.

handle_begin(test, _Data, State) ->
  State;
handle_begin(_Kind, _Data, State) ->
  State.
    
handle_end(test, Data, State=#state{results=Results, passed=Passed, failed=Failed}) ->
  {_Mod, Func, _Arity} = proplists:get_value(source, Data),
  case proplists:get_value(status, Data) of
    ok -> %% passed test
      State#state{results=[{Func, true} | Results], passed=Passed+1};
    _ -> %% failed test
      State#state{results=[{Func, false} | Results], failed=Failed+1}
  end;
handle_end(group, Data, State) ->
  case proplists:get_value(id, Data) of
    [] -> 
      deverl_testpane:show_test_results(State#state.results, State#state.wx_env),
      Msg = io_lib:format("Tests complete: ~p passed, ~p failed.", [State#state.passed, State#state.failed]),
      deverl_log_out_wx:message(Msg);
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
