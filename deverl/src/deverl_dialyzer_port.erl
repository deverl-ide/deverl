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
%% @doc Opens and manages a port for Dialyzer.
%% @end
%% =====================================================================

-module(deverl_dialyzer_port).

-include("deverl.hrl").

%% API
-export([run/2]).


%% =====================================================================
%% Client API
%% =====================================================================

%% =====================================================================
%% @doc Run Dialyzer. Passes message back to caller.

run(From, Config) ->
  SetFlags = fun
    ({files, Files}, Acc) -> 
      ["--quiet", "--src"] ++ Files ++ Acc;
    (build_plt, Acc) -> 
      ["--build_plt", "--apps", "erts", "kernel", "stdlib", "mnesia"] ++ Acc;
    (BadFlag, Acc) -> 
      erlang:error({badflag, BadFlag})
  end,
  Flags = lists:foldl(SetFlags, [], Config),
  deverl_stdout_wx:clear(),
  
  #general_prefs{path_to_dialyzer=Dlzr} = deverl_sys_pref_gen:get_preference(general_prefs),
  
  %% Attempt to open the port
  try
    open_port({spawn_executable, Dlzr}, [use_stdio,
                                         exit_status,
                                         {args, Flags}]),                                          
    deverl_stdout_wx:append_header("Dialyzer Output"),
    loop(From)
  catch
    error:_ ->
      deverl_stdout_wx:append("Couldn't start Dialyzer.\nCheck the path to Dialyzer is correct in Preferences -> General -> Path to Dialyzer."),
      deverl_log_out_wx:error("Error: Couldn't start Dialyzer, see output.", [{hotspot, "output"}]),
      From ! {self(),{ error, could_not_start}} %% Inform the caller we couldn't continue
  end.

%% =====================================================================
%% Internal functions
%% =====================================================================

%% =====================================================================
%% @doc Looping receive block to receive all output from the port until
%% an exit_status is received.

loop(From) ->
  receive
    {_Port, {data, Data}} ->
      deverl_stdout_wx:append(Data),
      loop(From);
    {_Port, {exit_status, 0}} ->
      deverl_log_out_wx:message("Dialyzer finished."),
      deverl_stdout_wx:append_footer(),
      From ! {self(), ok};
    {_Port, {exit_status, _}} ->
      deverl_log_out_wx:error("ERROR: Dialyzer failed. See output.", [{hotspot, "output"}]),
      deverl_stdout_wx:append_footer(),
      From ! {self(), {error, runtime_error}}
  end.