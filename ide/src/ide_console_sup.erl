-module(ide_console_sup).

-behaviour(supervisor).
-export([init/1]).

%% API
-export([start_link/1]).


%% =====================================================================
%% Client API
%% =====================================================================

%% =====================================================================
%% @doc

-spec start_link(Config) -> supervisor:startlink_ret() when
  Config :: list().

start_link(Config) ->
	supervisor:start_link(?MODULE, Config).


%% =====================================================================
%% Callback functions
%% =====================================================================
	
init(Config) ->
  Parser = {ide_console_parser, {ide_console_parser, start, []},
            permanent, brutal_kill, worker, [ide_console_parser]},
	Port = {ide_console_port_gen, {ide_console_port_gen, start, Config},
						permanent, 2000, worker, [ide_console_port_gen]},
	Children = [Parser, Port],
	RestartStategy = {one_for_one, 5, 4},
	{ok, {RestartStategy, Children}}.
