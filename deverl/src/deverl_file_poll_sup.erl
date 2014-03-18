-module(deverl_file_poll_sup).

-behaviour(supervisor).
-export([init/1]).

%% API
-export([start_link/1]).


%% =====================================================================
%% Client API
%% =====================================================================

%% =====================================================================
%% @doc

-spec start_link(list()) -> supervisor:startlink_ret().

start_link(Config) ->
	supervisor:start_link(?MODULE, Config).


%% =====================================================================
%% Callback functions
%% =====================================================================


init(Config) ->
	Server = {deverl_file_poll_gen, {deverl_file_poll_gen, start, [Config]},
						transient, 2000, worker, [deverl_file_poll_gen]},
	Children = [Server],
	RestartStategy = {one_for_one, 0, 1},
	{ok, {RestartStategy, Children}}.
