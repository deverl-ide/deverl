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
%% @doc Prepares user input for passing to the console port. Prepares
%% responses from the port for display to the user.
%% @end
%% =====================================================================

-module(deverl_console_parser).

-export([start/0,
		     parse_input/1,
		     parse_response/1]).

-export([init/0]).

%% =====================================================================
%% Client API
%% =====================================================================

%% =====================================================================
%% @doc Start an instance of the parser. 

-spec start() -> {ok, pid()}.

start()->
    Pid =  spawn_link(?MODULE, init, []),
    register(?MODULE, Pid),
    {ok, Pid}.


%% =====================================================================
%% @doc Prepares user input for sending to the console port, and sends it.

-spec parse_input(string()) -> ok.

parse_input(Message) ->
	M = Message ++ io_lib:nl(),
	deverl_console_port_gen:eval(M).


%% =====================================================================
%% @doc Formats the response from the port, before sending it to the
%% console.

-spec parse_response(string()) -> {data, string()}.

parse_response(Response) ->
  ?MODULE ! {data, Response}.


%% =====================================================================
%% Internal functions
%% =====================================================================

init() ->
  process_flag(trap_exit, true), %% Die when the parent process dies
  loop().

loop() ->
  receive
    {data, R1} ->
      %% Split data at newline
      Split = re:split(R1, "(\\R)", [{newline, any}, {return, list}, trim]),
      lists:foreach(fun eval_response/1, Split),
      loop()
  end.

eval_response(R0) ->
  case is_prompt(R0) of
    true ->
      deverl_console_wx:append_command({response, complete});
    false ->
      deverl_console_wx:append_command({response, R0})
  end.

is_prompt(Cmd) ->
  case re:run(Cmd, "^\\d+> $", []) of
    nomatch ->
      false;
    {match, _} ->
      true
  end.
