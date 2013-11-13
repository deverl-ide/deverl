%% =====================================================================
%% @author
%% @copyright
%% @title
%% @version
%% @doc 
%% @end
%% =====================================================================

-module(ide_build).

%% API
-export([
        compile_file/0,
        make_project/0,
        run_project/0
        ]).


%% =====================================================================
%% Client API
%% =====================================================================

compile_file() ->
  DocId = doc_manager:get_active_document(),
  case doc_manager:save_document(DocId) of
    ok ->
      Path = doc_manager:get_path(DocId),
      console_wx:load_response("Compiling module.. " ++ filename:basename(Path) ++ io_lib:nl()),
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
      ide_projects_tree:set_has_children(Path ++ "/ebin");
    cancelled ->
      ok
  end.
  
  
run_project() ->
  ok.


%% =====================================================================
%% Internal functions
%% =====================================================================

compile_file(Path) ->
	console_port:call_port("c(\"" ++ Path ++ "\")." ++ io_lib:nl()).
