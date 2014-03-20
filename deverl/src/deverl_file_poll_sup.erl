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
%% @doc Supervises a file poller process.
%% @end
%% =====================================================================

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
%% @hidden
init(Config) ->
	Server = {deverl_file_poll_gen, {deverl_file_poll_gen, start, [Config]},
						transient, 2000, worker, [deverl_file_poll_gen]},
	Children = [Server],
	RestartStategy = {one_for_one, 0, 1},
	{ok, {RestartStategy, Children}}.
