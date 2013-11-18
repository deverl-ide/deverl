%% =====================================================================
%% @author
%% @copyright
%% @title
%% @version
%% @doc
%% @end
%% =====================================================================

-module(port).

%% API
-export([start/2, call/2]).


%% =====================================================================
%% Client API
%% =====================================================================

%% =====================================================================
%% @doc 
%% @throws

start(Args, Cwd)->
  io:format("~p~n", [self()]),
  spawn(?MODULE, call, [Args, Cwd]).
  %call(Args, Cwd).   
  

%% =====================================================================
%% Internal functions
%% =====================================================================	

%% =====================================================================
%% @doc

call(Args, Cwd) ->
  ErlPath = case os:type() of
		{win32,_} ->
			"C:\\Program Files\\erl5.10.3\\erts-5.10.3\\bin\\erl";
    {_,darwin} ->
      "/usr/local/lib/erlang/erts-5.10.1/bin/erl";
		_Other ->
			"/usr/local/lib/erlang/erts-5.10.2/bin/erl"
  end,
  open_port({spawn_executable, ErlPath}, [use_stdio, exit_status, {cd, Cwd}, {args, [Args]}]),
  io:format("~p~n", [self()]),
  loop().
  
  
loop() ->
  receive 
    {data, Data} ->
      io:format("Data Received: ~p~n", [Data]),
      loop();
    {exit_status, 0} ->
      io:format("Exited~n")
  end.



		



