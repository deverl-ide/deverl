-module(deverl_console_sup).

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
  Parser = {deverl_console_parser, {deverl_console_parser, start, []},
            permanent, brutal_kill, worker, [deverl_console_parser]},
	Port = {deverl_console_port_gen, {deverl_console_port_gen, start, Config},
						permanent, 2000, worker, [deverl_console_port_gen]},
	Children = [Parser, Port],
	RestartStategy = {one_for_one, 5, 4},
	{ok, {RestartStategy, Children}}.