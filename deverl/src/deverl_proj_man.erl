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
%% @doc Manages projects.
%% The project_id() should be the only term that is passed to other
%% modules. Any data relating to that project id should be retrieved
%% from this module by passing in the project id.
%% @end
%% =====================================================================

-module(deverl_proj_man).

-include_lib("wx/include/wx.hrl").
-include("deverl.hrl").

%% gen_server
-behaviour(gen_server).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

%% API
-export([start/1,
				 new_project/1,
         add_project/1,
				 open_project_dialog/1,
         open_project/1,
         get_project/1,
         close_active_project/0,
				 get_open_projects/0,
				 get_active_project/0,
				 set_active_project/1,
				 get_root/1,
         get_name/1,
         get_build_config/1,
         is_known_project/1,
         set_project_configuration/1,
         import/1,
         get_project_src_files/1
         ]).

-export_type([project_id/0]).

%% Records
-record(project, {root :: path(),
                  build_config,
									open_files :: [{path(), term()}]}).

%% Types
-type project_record() :: {project_id, #project{}}.

%% Server state
-record(state, {projects :: [project_record()],
								active_project :: project_id(),
								frame :: wxFrame:wxFrame()}).
                


-spec get_project_src_files(project_id()) -> [path()].

get_project_src_files(ProjId) ->
  Path = filename:join(get_root(ProjId), "src"),
  Fns = filelib:wildcard(Path ++ "/*.erl").


%% =====================================================================
%% Client API
%% =====================================================================

%% =====================================================================
%% @doc

-spec start(list()) -> {ok, pid()} | ignore | {error, {already_started, pid()} | term()}.

start(Config)->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).


%% =====================================================================
%% @doc Add a new project as specified through the dialog.
%% This function will attempt to create the standard erlang directory
%% structure. An error dialog will be displayed should this fail.

-spec new_project(wxFrame:wxFrame()) -> ok.

new_project(Parent) ->
  Dlg = deverl_dlg_new_proj_wx:new(Parent),
  case wxDialog:showModal(Dlg) of
    ?wxID_OK ->
      Path =  deverl_dlg_new_proj_wx:get_path(Dlg),
      try
        Path = deverl_io:create_directory_structure(deverl_dlg_new_proj_wx:get_path(Dlg)),
        add_project(Path)
      catch
        throw:Error ->
          deverl_lib_dlg_wx:message_quick(Parent, "Oops", Error)
      end;
    ?wxID_CANCEL ->
      ok
  end,
  deverl_dlg_new_proj_wx:destroy(Dlg),
  ok.


%% =====================================================================
%% @doc Add the project located at Path. No directories will be created.

-spec add_project(path()) -> ok.

add_project(Path) ->
  deverl_sys_pref_gen:set_preference(projects, [Path | deverl_sys_pref_gen:get_preference(projects)]),
  gen_server:call(?MODULE, {new_project, Path}),
  ok.


%% =====================================================================
%% @doc Open an existing project using a dialog.

-spec open_project_dialog(wxFrame:wxFrame()) -> ok.

open_project_dialog(Frame) ->
  Dlg = deverl_dlg_open_proj_wx:new(Frame, deverl_sys_pref_gen:get_preference(projects)),
  case wxDialog:showModal(Dlg) of
    ?wxID_CANCEL ->
      deverl_dlg_open_proj_wx:destroy(Dlg);
    ?wxID_OK ->
      Path = deverl_dlg_open_proj_wx:get_path(Dlg),
      deverl_dlg_open_proj_wx:destroy(Dlg),
      open_project(Path)
  end.


%% =====================================================================
%% @doc Open an existing project.

-spec open_project(path()) -> project_id().

open_project(Path) ->
  gen_server:call(?MODULE, {new_project, Path}).


%% =====================================================================
%% @doc

-spec get_project(path()) -> project_id().

get_project(Path) ->
  wx_object:call(?MODULE, {get_project, Path}).


%% =====================================================================
%% @doc

-spec close_active_project() -> ok.

close_active_project() ->
  case deverl_doc_man_wx:close_project(get_active_project()) of
    cancelled ->
      ok;
    ok ->
      wx_object:call(?MODULE, close_project)
  end.


%% =====================================================================
%% @doc Get the currently active project.
%% This is either the project to which the active document belongs, or
%% the last clicked item in the project tree if this is more recent.

-spec get_active_project() -> project_id().

get_active_project() ->
	gen_server:call(?MODULE, active_project).


%% =====================================================================
%% @doc Set the currently active project.

-spec set_active_project(project_id()) -> ok.

set_active_project(ProjectId) ->
	gen_server:cast(?MODULE, {active_project, ProjectId}).


%% =====================================================================
%% @doc Get all currently open projects

-spec get_open_projects() -> [project_id()].

get_open_projects() ->
	gen_server:call(?MODULE, open_projects).


%% =====================================================================
%% @doc

-spec get_root(project_id()) -> path().

get_root(ProjectId) ->
	gen_server:call(?MODULE, {get_root, ProjectId}).

get_name(ProjectId) ->
  filename:basename(get_root(ProjectId)).


%% =====================================================================
%% @doc

-spec get_build_config(project_id()) -> BuildConfig when
  BuildConfig :: undefined | [tuple()].


get_build_config(ProjectId) ->
  gen_server:call(?MODULE, {get_build_config, ProjectId}).


%% =====================================================================
%% @doc

-spec is_known_project(path()) -> {true, path()} | false.

is_known_project(Path) ->
  Projects = deverl_sys_pref_gen:get_preference(projects),
  is_subpath(Path, Projects).


%% =====================================================================
%% @doc

-spec set_project_configuration(wxFrame:wxFrame()) -> ok | cancelled | error.

set_project_configuration(Parent) ->
  Dialog = deverl_dlg_proj_conf_wx:start(Parent),
  case wxDialog:showModal(Dialog) of
    ?wxID_CANCEL ->
      cancelled;
    ?wxID_OK ->
      Config = deverl_dlg_proj_conf_wx:get_build_config(Dialog),
      deverl_dlg_proj_conf_wx:close(Dialog),
      wx_object:call(?MODULE, {set_project_configuration, Config})
  end.


%% =====================================================================
%% @doc

-spec import(wxFrame:wxFrame()) -> ok.

import(Parent) ->
  Dlg = deverl_dlg_import_proj_wx:new(Parent),
  case wxDialog:showModal(Dlg) of
    ?wxID_OK ->
        Path = deverl_dlg_import_proj_wx:get_path(Dlg),
        case deverl_dlg_import_proj_wx:copy_dir(Dlg) of
          true -> %% Copy the directory to 
            case deverl_io:copy_to_poject_dir(Path, deverl_sys_pref_gen:get_preference(project_directory)) of
              ok ->
                deverl_proj_man:add_project(Path);
              {error, _} -> ok
            end;
          false -> 
            deverl_proj_man:add_project(Path)
        end;
    ?wxID_CANCEL ->
      ok
  end,
  deverl_dlg_import_proj_wx:destroy(Dlg).

%% =====================================================================
%% Callback functions
%% =====================================================================

init(Args) ->
	process_flag(trap_exit, true), %% Die when the parent process dies
	Frame = proplists:get_value(frame, Args),
	WxEnv = proplists:get_value(wx_env, Args),
	wx:set_env(WxEnv),
	{ok, #state{frame=Frame, projects=[]}}.

handle_info(Msg, State) ->
  io:format("Got Info (project manager) ~p~n",[Msg]),
  {noreply,State}.

handle_call({new_project, Path}, _From, State=#state{projects=Projects}) ->
  case is_already_open(Projects, Path) of
    {true, Id} ->
      {reply, Id, State};
    false ->
    	Id = generate_id(),
    	Record = {Id, #project{root=Path, open_files=[]}},
      deverl_proj_tree_wx:add_project(Id, Path),
      deverl:enable_menu_item_group([?MENU_GROUP_PROJECTS_EMPTY], true),
      code:add_path(filename:join(Path, "ebin")),
      {reply, Id, State#state{projects=[Record | Projects]}}
  end;

handle_call({add_open_project, Id, Path}, _From, State=#state{projects=Projects}) ->
	P=#project{open_files=Open} = proplists:get_value(Id, Projects),
	N = P#project{open_files=[Path | Open]},
	D = proplists:delete(Id, Projects),
  {reply, ok, State#state{projects=[{Id, N} | D]}};

handle_call({get_project, Path}, _From, State=#state{projects=Projects}) ->
  {reply, path_to_project_id(Projects, Path), State};

handle_call(open_projects, _From, State=#state{projects=Projects}) ->
	OpenProjects = lists:map(fun({Id, _Path}) -> Id end, Projects),
  {reply, OpenProjects, State};

handle_call(active_project, _From, State) ->
  {reply, State#state.active_project, State};

handle_call({get_root, ProjectId}, _From, State=#state{projects=Projects}) ->
	#project{root=Root} = proplists:get_value(ProjectId, Projects),
	{reply, Root, State};

handle_call({get_build_config, ProjectId}, _From, State=#state{projects=Projects0}) ->
  #project{build_config=Bc0, root=Root}=Project = proplists:get_value(ProjectId, Projects0),
    {Bc1, Projects1} = case Bc0 of
      undefined -> %% Attempt to read the config file
        Bc2 = load_build_config(Root),
        Tmp = proplists:delete(ProjectId, Projects0),
        {Bc2, [{ProjectId, Project#project{build_config=Bc2}} | Tmp]};
      C ->
        {C, Projects0}
    end,
    M = proplists:get_value(module, Bc1),
    F = proplists:get_value(function, Bc1),
    Bc3 = case M =:= [] orelse F =:= [] of
      true -> undefined;
      _ -> Bc1
    end,
  {reply, Bc3, State#state{projects=Projects1}};

handle_call(close_project, _From, State=#state{frame=Frame, active_project=ActiveProject, projects=Projects}) ->
  deverl_proj_tree_wx:remove_project(ActiveProject),
  update_ui(Frame, undefined),
  ProjectsList = proplists:delete(ActiveProject, Projects),
  deverl:enable_menu_item_group([?MENU_GROUP_PROJECTS_EMPTY], false),
  {reply, ok, State#state{active_project=undefined, projects=ProjectsList}};

handle_call({set_project_configuration, Config}, _From,
            State=#state{active_project=ActiveProject, projects=Projects}) ->
  #project{root=Root}=Project = proplists:get_value(ActiveProject, Projects),
  Result = case file:write_file(filename:join([Root, ".build_config"]), io_lib:fwrite("~p.\n",[Config])) of
    ok ->
      ok;
    {error, _} -> %% Project config will only be maintained while the application is open
      error
  end,
  Tmp = proplists:delete(ActiveProject, Projects),
  UpdatedProjects = [{ActiveProject, Project#project{build_config=Config}} | Tmp],
 {reply, Result, State#state{projects=UpdatedProjects}}.

handle_cast({active_project, ProjectId}, State=#state{frame=Frame, projects=Projects}) ->
  case ProjectId of
    undefined ->
      deverl:enable_menu_item_group([?MENU_GROUP_PROJECTS_EMPTY], false),
      update_ui(Frame, undefined);
    _ ->
      deverl:enable_menu_item_group([?MENU_GROUP_PROJECTS_EMPTY], true),
      update_ui(Frame, proplists:get_value(ProjectId, Projects))
  end,
  {noreply,State#state{active_project=ProjectId}}.

code_change(_, _, State) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.


%% =====================================================================
%% Internal functions
%% =====================================================================

%% =====================================================================
%% @doc

-spec generate_id() -> erlang:timestamp().

generate_id() ->
	now().


%% =====================================================================
%% @doc

-spec update_ui(wxFrame:wxFrame(), ProjectRoot) -> ok when
  ProjectRoot :: path() | undefined.

update_ui(Frame, undefined) ->
  deverl_menu:update_label(wxFrame:getMenuBar(Frame), ?MENU_ID_CLOSE_PROJECT, "Close Project"),
  deverl:set_title([]),
  ok;
update_ui(Frame, #project{root=Root}) ->
	ProjectName = filename:basename(Root),
	deverl:set_title(ProjectName),
	deverl_menu:update_label(wxFrame:getMenuBar(Frame), ?MENU_ID_CLOSE_PROJECT, "Close Project (" ++ ProjectName ++ ")"),
	ok.


%% =====================================================================
%% @doc

-spec path_to_project_id([project_record()], path()) -> project_id() | undefined.

path_to_project_id([], _Path) ->
  undefined;
path_to_project_id([{ProjId, #project{root=Path}} | _T], Path) ->
  ProjId;
path_to_project_id([_|T], Path) ->
  path_to_project_id(T, Path).


%% =====================================================================
%% @doc

-spec is_subpath(path(), [path()]) -> {true, path()} | false.

is_subpath(_Path, []) ->
  false;
is_subpath(Path, [ProjectPath|ProjectPaths]) ->
  Path1 = filename:join([Path]),
  ProjectPath1 = filename:join([ProjectPath]),
  case string:equal(string:sub_string(Path1, 1, string:len(ProjectPath1)), ProjectPath1) of
    true ->
      {true, ProjectPath1};
    false ->
      is_subpath(Path1, ProjectPaths)
  end.


%% =====================================================================
%% @doc Determine whether the project with path Path is already open.

-spec is_already_open([project_id()], path()) -> {true, project_id()} | false.

is_already_open(Projects, Path) ->
  case path_to_project_id(Projects, Path) of
    undefined ->
      false;
    ProjId ->
      {true, ProjId}
  end.


%% =====================================================================
%% @doc

-spec load_build_config(path()) -> BuildConfig when
  BuildConfig :: undefined | [tuple()].

load_build_config(Path) ->
  Fh = filename:join([Path, ".build_config"]),
  case file:consult(Fh) of
    {error, {_Line, _Mod, _Term}} -> %% The file is badly formatted
      % io:format("BUILD CONFIG BADLY FORMATTED~n"),
      undefined;
    {error, enoent} -> %% Does not exist
      % io:format("BUILD CONFIG NOT EXISTS~n"),
      undefined;
    {error, E} -> %% Other i/o error
      %% Notify user of i/o error
      % io:format("ERROR: ~p~n", [E]),
      undefined;
    {ok, [Terms]} ->
      % io:format("TERMS: ~p~n", [Terms]),
      Terms
  end.
