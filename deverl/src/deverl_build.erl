%% =====================================================================
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%% 
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%% 
%% You should have received a copy of the GNU General Public License
%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%
%% @author Tom Richmond <tr201@kent.ac.uk>
%% @author Mike Quested <mdq3@kent.ac.uk>
%% @copyright Tom Richmond, Mike Quested 2014
%%
%% @doc Defines and handles all make, compile and run functionality for
%% Deverl projects and files.
%% @end
%% =====================================================================

-module(deverl_build).
-include_lib("wx/include/wx.hrl").
-include("deverl.hrl").

%% API
-export([compile_file/0,
         make_project/0,
         run_project/1]).


%% =====================================================================
%% Client API
%% =====================================================================

%% =====================================================================
%% @doc Compile the active document.

-spec compile_file() -> ok.

compile_file() ->
  DocId = deverl_doc_man_wx:get_active_document(),
  case deverl_doc_man_wx:save_document(DocId) of
    ok ->
      Path = deverl_doc_man_wx:get_path(DocId),
      compile_file(Path);
    cancelled ->
      ok
  end.


%% =====================================================================
%% @doc Make the active project, and write message to the console.
%% @equiv make_project(true)

-spec make_project() -> {ok, project_id(), string()} | {atom(), atom()}.

make_project() ->
  make_project(true).


%% =====================================================================
%% @doc Make the active project. Pass false to do this quietly (doesn't
%% print to console).

-spec make_project(boolean()) -> {ok, project_id(), string()} | Error when
  Error :: {error, compile} |
           {error, not_saved} |
           {error, could_not_start}.

make_project(PrintMsg) ->
  case deverl_doc_man_wx:save_active_project() of
    ok ->
      ProjectId = deverl_proj_man:get_active_project(),
      Path = deverl_proj_man:get_root(ProjectId),  
      deverl_compiler_port:start(Path),
      receive
        {error, could_not_start} ->
          {error, could_not_start};
        {_From, ok} ->
          deverl:display_output_window(?WINDOW_OUTPUT),
          deverl_proj_tree_wx:set_has_children(Path ++ "/ebin"),
          case PrintMsg of
            true ->
              deverl_console_wx:append_message("Project ready: " ++ filename:basename(Path));
            _ -> ok
          end,
          load_files(filelib:wildcard(Path ++ "/src/*")),
          {ok, ProjectId, Path};
        {_From, error} ->
          deverl:display_output_window(?WINDOW_OUTPUT),
          {error, compile}
      end;
    cancelled ->
      {error, not_saved}
  end.


%% =====================================================================
%% @doc Run the active project.

-spec run_project(wxFrame:wxFrame()) -> ok | cancelled | error | no_return().

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

%% =====================================================================
%% @doc

-spec compile_file(string()) -> ok | {error, could_not_start} | {error, failed}.

compile_file(Path) ->
  %% Remember to send the output directory to load_file/1 if the flag is set
  %% for the compiler (-o).
  deverl_compiler_port:start(Path, [file]),
    receive
      {error, could_not_start} -> 
        {error, could_not_start};
      {_From, ok} ->
        case deverl_proj_man:is_known_project(Path) of
          {true, P0} ->
            P1 = filename:join([P0, "ebin", filename:basename(Path)]),
            deverl_proj_tree_wx:set_has_children(filename:dirname(P1)),
            load_file(P1, []);
          _ ->
            load_file(Path, [])
        end;
      {_From, error} ->
        {error, failed}
    end.


%% =====================================================================
%% @doc

-spec load_files([path()]) -> ok.

load_files([]) ->
  ok;
load_files([File|Files]) ->
  Mod = filename:basename(File, ".erl"),
  Ebin = filename:join(filename:dirname(filename:dirname(File)), "ebin"),
  Beam = filename:join([Ebin, Mod]),
  
  deverl_console_port_gen:eval("code:delete(" ++ Mod ++ ")." ++ io_lib:nl(), false),
  deverl_console_port_gen:eval("code:purge(" ++ Mod ++ ")." ++ io_lib:nl(), false),
  deverl_console_port_gen:eval("code:load_abs(\"" ++ Beam ++ "\")." ++ io_lib:nl(), false),
  load_files(Files).


%% =====================================================================
%% @doc

-spec load_file(path(), list()) -> ok.

load_file(Path, _Options) ->
  Mod = filename:basename(Path, ".erl"),
  Beam = filename:join([filename:dirname(Path), Mod]),

  deverl_console_port_gen:eval("code:delete(" ++ Mod ++ ")." ++ io_lib:nl(), false),
  deverl_console_port_gen:eval("code:purge(" ++ Mod ++ ")." ++ io_lib:nl(), false),
  deverl_console_port_gen:eval("code:load_abs(\"" ++ Beam ++ "\")." ++ io_lib:nl(), false),
  deverl_console_wx:append_message("Loaded module: " ++ Mod).


%% =====================================================================
%% @doc

-spec build_project(wxFrame:wxFrame(), project_id()) -> ok | cancelled | error | no_return().

build_project(Parent, ProjectId) ->
  case deverl_proj_man:get_build_config(ProjectId) of
    undefined ->
      notify_missing_config(Parent);
    Config ->
      try
        ParsedConfig = parse_config(Config),
        receive after 50 -> ok end, %% NASTY pause so we dont print the chang_dir response WTF
        deverl_console_wx:append_message("Running project:", false),
        execute_function(ParsedConfig)
      catch
        error:_ ->
          error("Error parsing build config.")
      end
  end.


%% =====================================================================
%% @doc

-spec notify_missing_config(wxFrame:wxFrame()) -> ok | cancelled | error.

notify_missing_config(Parent) ->
  Dlg = deverl_lib_dlg_wx:message(wx:null(), 
    [{caption, "The project needs to be configured before it can be run."},
     {text1, "Do you want to configure it now?"},
     {buttons, [?wxID_CANCEL, ?wxID_OK]}]),
  Result = case wxDialog:showModal(Dlg) of
		?wxID_CANCEL ->
			cancelled;
		?wxID_OK ->
      deverl_proj_man:set_project_configuration(Parent)
	end,
  wxDialog:destroy(Dlg),
  Result.


%% =====================================================================
%% @doc

-spec parse_config(list()) -> tuple().

parse_config([{module, M}, {function, F}]) -> {M,F};
parse_config([{module, M}, {function, F}, {args, Args}]) -> {M,F,Args}.


%% =====================================================================
%% @doc

-spec execute_function(tuple()) -> ok.

execute_function({M, F}) ->
  deverl_console_port_gen:eval(M ++ ":" ++ F ++ "()." ++ io_lib:nl());
execute_function({M, F, Args}) ->
  deverl_console_port_gen:eval("erlang:apply(" ++ M ++ "," ++ F ++ ",[" ++ Args ++ "])." ++ io_lib:nl()).