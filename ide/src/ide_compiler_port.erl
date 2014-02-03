%% =====================================================================
%% @author
%% @copyright
%% @title ide_compiler_port
%% @version
%% @doc This module connects to erlc through a port and prints the
%% response to an output window until an exit signal is received. An
%% acknowledgment of success or failure is then sent to the calling
%% process.
%% @end
%% =====================================================================

-module(ide_compiler_port).

%% API
-export([start/1, start/2]).

%% Spawned process
-export([compile/3]).

%% Type
-type path() :: string().


%% =====================================================================
%% Client API
%% =====================================================================

%% =====================================================================
%% @doc Compile the resource located at Cwd. If 'file' is set in Config
%% then Cwd is the absolute path of the file to compile, otherwise it
%% is the root directory that contains all erlang source files to be
%% compiled.
%% A message of the format 'ok' or 'error' will be sent the calling
%% process on completion.

-spec start(path()) -> pid().

start(Cwd)->
  start(Cwd, []).

-spec start(Cwd, [Config])  -> pid() when
  Cwd :: path(),
  Config :: file %% Single file, Cwd is absolute path of file
          | Flag,
  Flag :: {cflag, supress_warnings  %% Supress warnings
          | verbose                 %% Verbose warnings
          | smp                     %% Native compilation
          | makefile                %% Create a makefile
          | all_errors}.            %% Convert warninf to errors

start(Cwd, Config)->
  spawn(?MODULE, compile, [self(), Cwd, Config]).



%% =====================================================================
%% Internal functions
%% =====================================================================

%% =====================================================================
%% @doc
%% @private
%% @see start/2

-spec compile(pid(), path(), list()) -> {pid(), 'ok' | 'error'}.

compile(From, Path, Config) ->
  CFlags = fun({cflag, supress_warnings}, Acc) -> ["-W0"|Acc];
           ({cflag, verbose}, Acc) -> ["-v"|Acc];
           ({cflag, smp}, Acc) -> ["-smp"|Acc];
           ({cflag, makefile}, Acc) -> ["-M"|Acc];
           ({cflag, all_errors}, Acc) -> ["-Werror"|Acc];
           ({cflag, BadFlag}, _) -> erlang:error({badflag, BadFlag});
           (_, Acc) -> Acc end,

  Flags = lists:foldl(CFlags, [], Config),

  {Cwd, Args} = case proplists:get_value(file, Config) of
    true ->
      %% Single file
      A0 = [filename:basename(Path)],
      A1 = case ide_proj_man:is_known_project(Path) of %% beam goes to /ebin
        {true, P} ->
          OutputDir = ["-o", lists:append([P, "/ebin"])],
          lists:append(OutputDir, A0);
        _ -> A0
      end,
      {filename:dirname(Path), A1};
    _ ->
      %% Directory
      IncludeDir = ["-I", lists:append([Path, "/include"])],
      OutputDir = ["-o", lists:append([Path, "/ebin"])],
      DirFlags = lists:append(IncludeDir, OutputDir),

      %% Get a list of all erl,hrl,yrl,mib,rel files in any subdirectory of Cwd
      Files = lists:filter(fun(X) -> not filelib:is_dir(X) end,
        filelib:wildcard([Path | "/src/**/*.{erl,hrl,yrl,mib,rel}"])),

      {Path, lists:append(DirFlags, Files)}
  end,

  ide_stdout_wx:clear(),
  open_port({spawn_executable, erlc()}, [use_stdio,
                                         exit_status,
                                         {cd, Cwd},
                                         {args, lists:append(Flags, Args)}]),
                                         
  % ide_stdout_wx:append("========================= Compiler Output ========================\n"),
  ide_stdout_wx:append_header("Compiler Output"),
  loop(From, filename:basename(Path)).


%% =====================================================================
%% @doc Looping receive block to receive all output from the port until
%% an exit_status is received.

-spec loop(pid(), string()) -> {pid(), 'ok' | 'error'}.

loop(From, Name) ->
  receive
    {_Port, {data, Data}} ->
      ide_stdout_wx:append(Data),
      loop(From, Name);
    {_Port, {exit_status, 0}} ->
      ide_log_out_wx:message("Compiled " ++ Name ++ " successfully."),
      ide_stdout_wx:append_footer(),
      From ! {self(), ok};
    {_Port, {exit_status, _}} ->
      ide_log_out_wx:error("ERROR: Compilation failed " ++ Name ++ ". See output.", [{hotspot, "output"}]),
      ide_stdout_wx:append_footer(),
      From ! {self(), error}
  after
    10000 ->
      io:format("TIMEOUT~n"),
      error
  end.


%% =====================================================================
%% @doc Get the path to erlc.

-spec erlc() -> path().

erlc() ->
  case os:type() of
		{win32,_} ->
			"C:\\Program Files\\erl5.10.3\\erts-5.10.3\\bin\\erlc";
    _ ->
      string:strip(os:cmd("which erlc"), both, $\n)
  end.
