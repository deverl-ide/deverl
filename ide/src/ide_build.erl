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
  case doc_manager:save_active_project() of
    ok ->
      ProjectId = project_manager:get_active_project(),
      Path = project_manager:get_root(ProjectId),
      compiler_port:start(Path),
      receive
        {From, ok} ->
          ide_projects_tree:set_has_children(Path ++ "/ebin"),
          console_wx:append_message("Project " ++ filename:basename(Path) ++ " ready"),
          change_dir(Path ++ "/ebin"),
          {ok, ProjectId, Path};
        {From, error} ->
          {error, compile}
      end;
    cancelled ->
      {error, not_saved}
  end.
  
run_project(Parent) ->
  case make_project() of
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
      load_file(Path, []);
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
  
parse_config(Config) ->
  ok.

execute_function({M, F, Args}) ->
  ok.

change_dir(Path) ->
  console_port:eval("cd(\"" ++ Path ++ "\")." ++ io_lib:nl(), false).
