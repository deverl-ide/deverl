-module(file_sup).

-behaviour(supervisor).
-export([init/1]).

%% API
-export([start_link/1]).

start_link(Config) ->
	supervisor:start_link(?MODULE, Config).
	
init(Config) ->
	Server = {file_poller, {file_poller, start, [Config]},
						transient, 2000, worker, [file_poller]},
	Children = [Server],
	RestartStategy = {one_for_one, 0, 1},
	{ok, {RestartStategy, Children}}.