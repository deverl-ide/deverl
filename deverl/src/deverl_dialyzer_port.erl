%% =====================================================================
%% @author
%% @copyright
%% @title
%% @version
%% @doc 
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
%% @doc

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
    deverl_stdout_wx:append("========================= Dialyzer Output ========================\n"),  
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
      From ! {self(), ok};
    {_Port, {exit_status, _}} ->
      deverl_log_out_wx:error("ERROR: Dialyzer failed. See output.", [{hotspot, "output"}]),
      From ! {self(), {error, runtime_error}}
  end.