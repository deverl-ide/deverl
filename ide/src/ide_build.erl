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
  DocId = ide_doc_man_wx:get_active_document(),
  case ide_doc_man_wx:save_document(DocId) of
    ok ->
      Path = ide_doc_man_wx:get_path(DocId),
      compile_file(Path);
    cancelled ->
      ok
  end.

make_project() ->
  make_project(true).
make_project(PrintMsg) ->
  case ide_doc_man_wx:save_active_project() of
    ok ->
      ProjectId = ide_proj_man:get_active_project(),
      Path = ide_proj_man:get_root(ProjectId),
      ide_compiler_port:start(Path),
      receive
        {_From, ok} ->
          ide_proj_tree_wx:set_has_children(Path ++ "/ebin"),
          case PrintMsg of
            true ->
              ide_console_wx:append_message("Project ready: " ++ filename:basename(Path));
            _ -> ok
          end,
          change_dir(Path ++ "/ebin"),
          {ok, ProjectId, Path};
        {_From, error} ->
          {error, compile}
      end;
    cancelled ->
      {error, not_saved}
  end.

run_project(Parent) ->
  case make_project(false) of
    {ok, ProjectId, _ProjectPath} ->
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
  ide_compiler_port:start(Path, [file]),
  receive
    {_From, ok} ->
      case ide_proj_man:is_known_project(Path) of
        {true, P0} ->
          P1 = filename:join([P0, "ebin", filename:basename(Path)]),
          ide_proj_tree_wx:set_has_children(filename:dirname(P1)),
          load_file(P1, []);
        _ ->
          load_file(Path, [])
      end;
    {_From, error} ->
      ok
  end.
  
  
load_file(Path, _Options) ->
  Mod = filename:basename(Path, ".erl"),
  Beam = filename:join([filename:dirname(Path), Mod]),
  ide_console_port_gen:eval("code:load_abs(\"" ++ Beam ++ "\")." ++ io_lib:nl(), false),
  ide_console_wx:append_message("Loaded module: " ++ Mod).
  
  
build_project(Parent, ProjectId) ->
  case ide_proj_man:get_build_config(ProjectId) of
    undefined -> 
      notify_missing_config(Parent);
    Config ->
      try
        ParsedConfig = parse_config(Config),
        receive after 50 -> ok end, %% NASTY pause so we dont print the chang_dir response WTF
        ide_console_wx:append_message("Running project:", false),
        execute_function(ParsedConfig)
      catch
        error:_ ->
          error("ERROR")
      end
  end.
  
notify_missing_config(Parent) ->
  Dialog = ide_lib_dlg_wx:notify_missing_config(Parent),
  case wxDialog:showModal(Dialog) of
		?wxID_CANCEL ->
			cancelled;
		?wxID_OK ->
      ide_proj_man:set_project_configuration(Parent)
	end.

parse_config([{module, M}, {function, F}]) -> {M,F};
parse_config([{module, M}, {function, F}, {args, Args}]) -> {M,F,Args}.

execute_function({M, F}) ->
  ide_console_port_gen:eval(M ++ ":" ++ F ++ "()." ++ io_lib:nl());
execute_function({M, F, Args}) ->
  ide_console_port_gen:eval("erlang:apply(" ++ M ++ "," ++ F ++ ",[" ++ Args ++ "])." ++ io_lib:nl()).

change_dir(Path) ->
  ide_console_port_gen:eval("cd(\"" ++ Path ++ "\")." ++ io_lib:nl(), false).