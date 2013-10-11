-module(ide_build).

-export([
	compile/0
]).


compile() ->
  case doc_manager:get_active_document() of
    {error, _} ->
      ok;
     Index ->
      case doc_manager:save_document(Index) of
				undefined -> ok;
				Path ->			
					ide_shell:load_response("Compiling module.. " ++ filename:basename(Path) ++ io_lib:nl()),
		      compile_file(Path)
			end
  end.
  

compile_file(Path) ->
	port:call_port("c(\"" ++ Path ++ "\")." ++ io_lib:nl()).

