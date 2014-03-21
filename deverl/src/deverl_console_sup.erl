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
%% @doc Supervises deverl_console_parser and deverl_console_port_gen,
%% restarting them if they fail. If they cannot be restarted then this
%% process will shutdown (will be trapped in deverl).
%% @end
%% =====================================================================

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
%% @hidden
init(Config) ->
  Parser = {deverl_console_parser, {deverl_console_parser, start, []},
            permanent, brutal_kill, worker, [deverl_console_parser]},
	Port = {deverl_console_port_gen, {deverl_console_port_gen, start, Config},
						permanent, 2000, worker, [deverl_console_port_gen]},
	Children = [Parser, Port],
	RestartStategy = {one_for_one, 5, 4},
	{ok, {RestartStategy, Children}}.