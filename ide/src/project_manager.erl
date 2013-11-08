%% =====================================================================
%% @author
%% @copyright
%% @title
%% @version
%% @doc
%% The project_id() should be the only term that is passed to other
%% modules. Any data relating to that project id should be retrieved
%% from this module by passing in the project id.
%% @end
%% =====================================================================

-module(project_manager).

-include_lib("wx/include/wx.hrl").
-include("ide.hrl").

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
				 open_project_dialog/1,
         open_project/1,
         get_project/1,
				 close_project/0,
				 open_file/3,
				 get_open_projects/0,
				 get_active_project/0,
				 set_active_project/1,
				 get_root/1,
         get_name/1,
         is_known_project/1]).


%% Records
-record(project, {root :: path(),
									open_files :: [{path(), term()}]}).

%% Types
-type project_id() :: {integer(), integer(), integer()}.
-type path() :: string().
-type project_record() :: {project_id, #project{}}.

%% Server state
-record(state, {projects :: [project_record()],
								active_project :: project_id(),
								frame :: wxFrame:wxFrame()}).


%% =====================================================================
%% Client API
%% =====================================================================

%% =====================================================================
%% @doc 

start(Config)->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).
	    
			
%% =====================================================================
%% @doc Add a new project.

new_project(Parent) ->
	Dialog = new_project_wx:start(Parent),
	new_project_wx:set_focus(Dialog),
	case new_project_wx:showModal(Dialog) of
		?wxID_CANCEL -> 
      ok;
		?wxID_OK ->
			new_project(Parent, Dialog),
			new_project_wx:close(Dialog)
	end.

  
new_project(Parent, Dialog) ->
  try
		Path = ide_io:create_directory_structure(new_project_wx:get_path(Dialog)),
    sys_pref_manager:set_preference(projects, [Path | sys_pref_manager:get_preference(projects)]),
		Id = gen_server:call(?MODULE, {new_project, Path}),
		ide_projects_tree:add_project(Id, Path)
  catch
    throw:E -> 
			lib_dialog_wx:msg_error(Parent, E)
  end.
	
	
%% =====================================================================
%% @doc Open an existing project using a dialog.

open_project_dialog(Frame) ->
  Dialog = open_project_wx:start(Frame, sys_pref_manager:get_preference(projects)),
  case wxDialog:showModal(Dialog) of
    ?wxID_CANCEL ->
      ok;
    ?wxID_OK ->
      open_project(open_project_wx:get_path(Dialog)),
      open_project_wx:close(Dialog)
  end.
  

%% =====================================================================
%% @doc Open an existing project.

-spec open_project(path()) -> project_id().

open_project(Path) ->
  Id = gen_server:call(?MODULE, {new_project, Path}),
  ide_projects_tree:add_project(Id, Path),
  Id.
  

%% =====================================================================
%% @doc

get_project(Path) ->
  wx_object:call(?MODULE, {get_project, Path}).
  

%% =====================================================================
%% @doc Close an open project.
%% This will close any files belonging to the project, and remove the
%% tree from the project tree. 

close_project() ->
	%% Check open files, save/close
	wx_object:call(?MODULE, close_project).


%% =====================================================================
%% @doc Open an project file..

open_file(Path, Contents, ProjectId) ->
	doc_manager:new_document_from_existing(Path, Contents, [{project_id, ProjectId}]),
	gen_server:call(?MODULE, {add_open_project, ProjectId, Path}),
	ok. 


%% =====================================================================
%% @doc Get the currently active project.
%% This is either the project to which the active document belongs, or
%% the last clicked item in the project tree if this is more recent.

get_active_project() ->
	gen_server:call(?MODULE, active_project).
	
	
%% =====================================================================
%% @doc Set the currently active project.

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

is_known_project(Path) ->
  Projects = sys_pref_manager:get_preference(projects),
  is_known_project(Path, Projects).
  
is_known_project(_Path, []) ->
  false;
is_known_project(Path, [ProjectPath|Projects]) ->
  case string:equal(string:sub_string(Path, 1, string:len(ProjectPath)), ProjectPath) of
    true ->
      true;
    false ->
      is_known_project(Path, Projects)
  end.
  
		
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
	Id = generate_id(),
	Record = {Id, #project{root=Path, open_files=[]}},
  {reply, Id, State#state{projects=[Record | Projects]}};
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
handle_call(close_project, _From, State=#state{projects=Projects, active_project=ProjectId}) ->
	doc_manager:close_project(ProjectId),
  {reply, ProjectId, State}.
  
handle_cast({active_project, ProjectId}, State=#state{frame=Frame, projects=Projects}) ->
  case ProjectId of
    undefined ->
      update_ui(Frame, undefined);
    _ ->
      update_ui(Frame, proplists:get_value(ProjectId, Projects))
  end,
  {noreply,State#state{active_project=ProjectId}}.
    
%% Event catchall for testing
handle_event(Ev = #wx{}, State) ->
  io:format("Project manager event catchall: ~p\n", [Ev]),
  {noreply, State}.
    
code_change(_, _, State) ->
  {stop, not_yet_implemented, State}.

terminate(_Reason, State) ->
  io:format("TERMINATE PROJECT MANAGER~n").
     
		
%% =====================================================================
%% Internal functions
%% =====================================================================

generate_id() ->
	now().
	
update_ui(Frame, undefined) ->
  ide_menu:update_label(wxFrame:getMenuBar(Frame), ?MENU_ID_CLOSE_PROJECT, "Close Project"),
  ide:set_title([]),
  ok;
update_ui(Frame, #project{root=Root}) ->
	ProjectName = filename:basename(Root),
	ide:set_title(ProjectName),
	ide_menu:update_label(wxFrame:getMenuBar(Frame), ?MENU_ID_CLOSE_PROJECT, "Close Project (" ++ ProjectName ++ ")"),
	ok.

path_to_project_id([], Path) ->
  undefined;
path_to_project_id([{ProjId, #project{root=Path}} | T], Path) ->
  ProjId;
path_to_project_id([_|T], Path) ->
  path_to_project_id(T, Path).