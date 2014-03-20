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
%% @doc This module monitors a file for external updates.%% @end
%% =====================================================================

-module(deverl_file_poll_gen).

%% gen_server
-behaviour(gen_server).
-export([init/1, handle_info/2, handle_call/3, handle_cast/2,
				 handle_event/2, code_change/3, terminate/2]).

%% API
-export([start/1,
         stop/0]).

%% Macros
-define(INTERVAL, 500).

%% Server state
-record(state, {type, editor_pid, root_path, root_lm}).


%% =====================================================================
%% Client API
%% =====================================================================

%% =====================================================================
%% @doc

-spec start(list()) -> {ok, pid()} | ignore | {error, {already_started, pid()} | term()}.

start(Config) ->
	gen_server:start(?MODULE, Config, []).


%% =====================================================================
%% @doc

-spec stop() -> ok.

stop() ->
	gen_server:cast(?MODULE, stop).


%% =====================================================================
%% Callback functions
%% =====================================================================
%% @hidden
init(Config) ->
	Path = proplists:get_value(path, Config),
	State = case file_type(Path) of
		file ->
			DateTime = filelib:last_modified(Path),
			#state{type=file, root_path=Path, root_lm=DateTime};
		directory -> ok %% undefined behaviour
	end,

	% Start timer
	erlang:send_after(?INTERVAL, self(), trap),
	{ok, State#state{editor_pid=proplists:get_value(editor_pid, Config)}}.
%% @hidden
handle_info(trap, State=#state{type=file, root_path=Path, root_lm=Lm}) ->
	Mod = filelib:last_modified(Path),
	case Mod of
		0 ->
			%% Prompt user to save or close the file
			file_not_found();
			%% Returns the new path, update the state
		Lm ->
			ok;
		_ ->
			%% save the changes to the file
			%% DONE THROUGH DOC_MANAGER
      ok
	end,
	erlang:send_after(?INTERVAL, self(), trap),
	{noreply, State#state{root_lm=Mod}}.

file_not_found() ->
	ok.
%% @hidden
handle_call(_, _From, State) ->
	{noreply, State}.
%% @hidden
handle_cast(stop, State) ->
	{stop, normal, State}.
%% @hidden
handle_event(_, State) ->
	{noreply, State}.
%% @hidden
code_change(_, _, State) ->
	{ok, State}.
%% @hidden
terminate(_Reason, _) ->
  ok.


%% =====================================================================
%% Internal functions
%% =====================================================================

%% =====================================================================
%% @doc

-spec file_type(string()) -> file | directory.

file_type(Path) ->
	IsRegular = filelib:is_regular(Path),
	case IsRegular of
		true ->
			file;
		false ->
			directory
	end.
