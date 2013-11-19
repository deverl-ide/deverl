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
<<<<<<< HEAD
      %console_wx:load_response("Compiling module.. " ++ filename:basename(Path) ++ io_lib:nl()),
=======
      % console_wx:append_to_console("Compiling module.. " ++ filename:basename(Path) ++ io_lib:nl()),
>>>>>>> f37a2f40aed319e5bc438f22edc7d165a8ad7fa4
      compile_file(Path);
    cancelled ->
      ok
  end.

  
make_project() ->
  case doc_manager:save_active_project() of
    ok ->
      ProjectId = project_manager:get_active_project(),
      Path = project_manager:get_root(ProjectId),
      console_port:call_port("cd(\"" ++ Path ++ "\"), make:all()." ++ io_lib:nl()),
      ide_projects_tree:set_has_children(Path ++ "/ebin"),
      {ok, ProjectId, Path};
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
<<<<<<< HEAD
	%console_port:call_port("c(\"" ++ Path ++ "\")." ++ io_lib:nl()).
=======
  % console_port:call_port("c(\"" ++ Path ++ "\")." ++ io_lib:nl()).
>>>>>>> f37a2f40aed319e5bc438f22edc7d165a8ad7fa4
  compiler_port:start([{file, Path}]).
  
build_project(Parent, ProjectId) ->
  case project_manager:get_build_config(ProjectId) of
    undefined -> 
      notify_missing_config(Parent);
    Config ->
      io:format("CONFIG: ~p~n", [Config]),
      try
        ParsedConfig = parse_config(Config),
        execute_function(ParsedConfig)
      catch
        error:_ ->
          io:format("ERROR PARSING CONFIG")
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
