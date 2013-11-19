%% =====================================================================
%% @author
%% @copyright
%% @title compiler_port
%% @version
%% @doc This module connects to erlc through a port and prints the
%% response to an output window unitl an exit signal is reached.
%% @end
%% =====================================================================

-module(compiler_port).

%% API
-export([start/1, compile/1]).


%% =====================================================================
%% Client API
%% =====================================================================

%% =====================================================================
%% @doc 
%% @throws

% start(Args, Cwd)->
%   spawn(?MODULE, call, [Args, Cwd]).
start(Config)->
  spawn(?MODULE, compile, [Config]). 

%% =====================================================================
%% Internal functions
%% =====================================================================	

compile(Config) ->
  case proplists:get_value(file, Config, undefined) of
    undefined -> %% Assume directory (project)
      call(Config);  
    File ->
      file(File, Config)
  end.

%% =====================================================================
%% @doc

file(File, Config) ->
  ErlC = case os:type() of
		{win32,_} ->
			"C:\\Program Files\\erl5.10.3\\erts-5.10.3\\bin\\erlc";
    {_,darwin} ->
      "/usr/local/lib/erlang/erts-5.10.1/bin/erlc";
		_ ->
			"/usr/local/lib/erlang/erts-5.10.2/bin/erlc"
  end,
  Cwd = filename:dirname(File),
  ErlCArgs = filename:basename(File),
  open_port({spawn_executable, ErlC}, [use_stdio, 
                                       exit_status, 
                                       {cd, Cwd}, 
                                       {args, [ErlCArgs]}]),
  loop(filename:basename(File)).


%% =====================================================================
%% @doc

call(Config) ->
  
  Cwd = proplists:get_value(cwd, Config),
  
  ErlC = case os:type() of
		{win32,_} ->
			"C:\\Program Files\\erl5.10.3\\erts-5.10.3\\bin\\erlc";
    {_,darwin} ->
      "/usr/local/lib/erlang/erts-5.10.1/bin/erlc";
		_ ->
			"/usr/local/lib/erlang/erts-5.10.2/bin/erlc"
  end,
  
  %% erlc flags
  IncludeDir = ["-I", lists:append([Cwd, "/include"])],
  OutputDir = ["-o", lists:append([Cwd, "/ebin"])],
  Flags = lists:append(IncludeDir, OutputDir),
           
  %% Get a list of all erl,hrl,yrl,mib,rel files in any subdirectory of Cwd
  Files = lists:filter(fun(X) -> not filelib:is_dir(X) end,
    filelib:wildcard([Cwd | "/src/**/*.{erl,hrl,yrl,mib,rel}"])),
  
  %% build the arguement list to pass to erlc
  ErlCArgs = lists:append(Flags, Files),

  open_port({spawn_executable, ErlC}, [use_stdio, 
                                       exit_status, 
                                       {cd, Cwd}, 
                                       {args, ErlCArgs}]),
  loop(filename:basename(Cwd)).
  

%% =====================================================================
%% @doc
  
loop(Name) ->
  receive 
    {_Port, {data, Data}} ->
      compiler_output:append(Data),
      loop(Name);
    {_Port, {exit_status, 0}} ->
      log:message("Compiled " ++ Name ++ " successfully.");
    {_Port, {exit_status, _}} ->
      log:error("ERROR: Compilation failed " ++ Name ++ ". See compiler output.")
  after
    10000 ->
      io:format("TIMEOUT~n")
  end.