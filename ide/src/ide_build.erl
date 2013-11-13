-module(ide_build).

-export([
        compile/0
        ]).


compile() ->
  DocId = doc_manager:get_active_document(),
  case doc_manager:save_document(DocId) of
    ok ->
      Path = doc_manager:get_path(DocId),
      console_wx:load_response("Compiling module.. " ++ filename:basename(Path) ++ io_lib:nl()),
      compile_file(Path);
    cancelled ->
      ok
  end.


compile_file(Path) ->
	console_port:call_port("c(\"" ++ Path ++ "\")." ++ io_lib:nl()).
