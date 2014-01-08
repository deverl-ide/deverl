-module(ide_file_poll_sup).

-behaviour(supervisor).
-export([init/1]).

%% API
-export([start_link/1]).

start_link(Config) ->
	supervisor:start_link(?MODULE, Config).
	
init(Config) ->
	Server = {ide_file_poll_gen, {ide_file_poll_gen, start, [Config]},
						transient, 2000, worker, [ide_file_poll_gen]},
	Children = [Server],
	RestartStategy = {one_for_one, 0, 1},
	{ok, {RestartStategy, Children}}.