-module(console_sup).

-behaviour(supervisor).
-export([init/1]).

%% API
-export([start_link/1]).

start_link(Config) ->
	supervisor:start_link(?MODULE, Config).
	
init(Config) ->
  Parser = {console_parser, {console_parser, start, []},
            permanent, brutal_kill, worker, [console_parser]},
	Port = {console_port, {console_port, start, Config},
						permanent, 2000, worker, [console_port]},
	Children = [Parser, Port],
	RestartStategy = {one_for_one, 5, 4},
	{ok, {RestartStategy, Children}}.