-module(ide_build).


compile() ->
  case doc_manager:get_selected_editor() of
    {error, _} ->
      ok;
    {ok, {Index, Pid}} ->
      doc_manager:save_file(Index, Pid),
      Path = filename:rootname(editor:get_editor_path(Pid)),
      compile_file(Path)
  end.
  

compile_file(Path) ->
	port:call_port("c(" ++ Path ++ ")." ++ io_lib:nl()).
	
