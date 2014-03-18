-module(deverl_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).
		
init([]) ->
  % Args = [{debug, [verbose]}],
  Silent = [{silent_start, true}],
  Server = {deverl, {deverl, start, [Silent]},
            temporary, 2000, worker, [deverl]},
  Children = [Server],
  RestartStrategy = {one_for_one, 0, 1},
  {ok, {RestartStrategy, Children}}.