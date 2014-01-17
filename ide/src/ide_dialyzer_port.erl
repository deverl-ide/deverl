%% =====================================================================
%% @author
%% @copyright
%% @title
%% @version
%% @doc 
%% @end
%% =====================================================================

-module(ide_dialyzer_port).

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
  ide_stdout_wx:clear(),
  open_port({spawn_executable, dialyzer()}, [use_stdio,
                                             exit_status,
                                             {args, Flags}]),
                                             
  ide_stdout_wx:append("========================= Dialyzer Output ========================\n"),  
  loop(From).


%% =====================================================================
%% Internal functions
%% =====================================================================

%% =====================================================================
%% @doc Looping receive block to receive all output from the port until
%% an exit_status is received.

loop(From) ->
  receive
    {_Port, {data, Data}} ->
      ide_stdout_wx:append(Data),
      loop(From);
    {_Port, {exit_status, 0}} ->
      ide_log_out_wx:message("Dialyzer finished."),
      From ! {self(), ok};
    {_Port, {exit_status, _}} ->
      ide_log_out_wx:error("ERROR: Dialyzer failed. See output.", [{hotspot, "output"}]),
      From ! {self(), error}
  after
    50000 ->
      io:format("TIMEOUT~n"),
      error
  end.


%% =====================================================================
%% @doc Get the path to erlc.

-spec dialyzer() -> file:filename().

dialyzer() ->
  case os:type() of
		{win32,_} ->
			"C:\\Program Files\\erl5.10.3\\erts-5.10.3\\bin\\dialyzer";
    _ ->
      string:strip(os:cmd("which dialyzer"), both, $\n)
  end.