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
  spawn(?MODULE, call, [Args, Cwd]). 
  

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
  open_port({spawn_executable, ErlPath}, [stderr_to_stdout, exit_status, {cd, Cwd}, {args, [Args]}]),
  loop().
  
  
loop() ->
  receive    
    {_Port, {data, Data}} ->
      io:format("Data Received: ~p~n", [Data]),
      loop();
    {_Port, {exit_status, Status}} ->
      io:format("Exited with code: ~p~n", [Status]);
    {'EXIT', _Port, Reason} ->
      io:format("Exited: ~p~n", [Reason]);
    Thing ->
      io:format("~p~n", [Thing])
  end.



		



