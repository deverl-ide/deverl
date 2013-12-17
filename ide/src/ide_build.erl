%% =====================================================================
%% @author
%% @copyright
%% @title
%% @version
%% @doc 
%% @end
%% =====================================================================

-module(ide_build).
-include_lib("wx/include/wx.hrl").

%% API
-export([
        compile_file/0,
        make_project/0,
        run_project/1
        ]).


%% =====================================================================
%% Client API
%% =====================================================================

compile_file() ->
  DocId = doc_manager:get_active_document(),
  case doc_manager:save_document(DocId) of
    ok ->
      Path = doc_manager:get_path(DocId),
      compile_file(Path);
    cancelled ->
      ok
  end.

make_project() ->
  make_project(true).
make_project(PrintMsg) ->
  case doc_manager:save_active_project() of
    ok ->
      ProjectId = project_manager:get_active_project(),
      Path = project_manager:get_root(ProjectId),
      compiler_port:start(Path),
      receive
        {From, ok} ->
          ide_projects_tree:set_has_children(Path ++ "/ebin"),
          case PrintMsg of
            true ->
              console_wx:append_message("Project " ++ filename:basename(Path) ++ " ready");
            _ -> ok
          end,
          change_dir(Path ++ "/ebin"),
          {ok, ProjectId, Path};
        {From, error} ->
          {error, compile}
      end;
    cancelled ->
      {error, not_saved}
  end.
  
run_project(Parent) ->
  case make_project(false) of
    {ok, ProjectId, ProjectPath} ->
      build_project(Parent, ProjectId);
    {error, _} ->
      ok
  end.


%% =====================================================================
%% Internal functions
%% =====================================================================

compile_file(Path) ->
  %% Remember to send the output directory to load_file/1 if the flag is set
  %% for the compiler (-o). 
  compiler_port:start(Path, [file]),
  receive
    {From, ok} ->
      case project_manager:is_known_project(Path) of
        {true, P0} ->
          P1 = filename:join([P0, "ebin", filename:basename(Path)]),
          ide_projects_tree:set_has_children(filename:dirname(P1)),
          load_file(P1, []);
        _ ->
          load_file(Path, [])
      end;
    {From, error} ->
      ok
  end.
  
  
load_file(Path, _Options) ->
  Mod = filename:basename(Path, ".erl"),
  Beam = filename:join([filename:dirname(Path), Mod]),
  console_port:eval("code:load_abs(\"" ++ Beam ++ "\")." ++ io_lib:nl(), false),
  console_wx:append_message("Loaded module: " ++ Mod).
  
  
build_project(Parent, ProjectId) ->
  case project_manager:get_build_config(ProjectId) of
    undefined -> 
      notify_missing_config(Parent);
    Config ->
      try
        ParsedConfig = parse_config(Config),
        receive after 50 -> ok end, %% NASTY pause so we dont print the chang_dir response WTF
        console_wx:append_message("Running project:", false),
        execute_function(ParsedConfig)
      catch
        error:_ ->
          error("ERROR")
      end
  end.
  
notify_missing_config(Parent) ->
  Dialog = lib_dialog_wx:notify_missing_config(Parent),
  case wxDialog:showModal(Dialog) of
		?wxID_CANCEL ->
			cancelled;
		?wxID_OK ->
      project_manager:set_project_configuration(Parent)
	end.

parse_config([{module, M}, {function, F}]) -> {M,F};
parse_config([{module, M}, {function, F}, {args, Args}]) -> {M,F,Args}.

execute_function({M, F}) ->
  console_port:eval(M ++ ":" ++ F ++ "()." ++ io_lib:nl());
execute_function({M, F, Args}) ->
  console_port:eval("erlang:apply(" ++ M ++ "," ++ F ++ ",[" ++ Args ++ "])." ++ io_lib:nl()).

change_dir(Path) ->
  console_port:eval("cd(\"" ++ Path ++ "\")." ++ io_lib:nl(), false).