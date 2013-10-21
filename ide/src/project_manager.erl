%% =====================================================================
%% @author
%% @copyright
%% @title
%% @version
%% @doc
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
-export([start/0,
				 new_project/1,
				 open_project/1,
				 open_file/3,
				 get_open_projects/0]).


%% Records
-record(project, {root :: path(),
									open_files :: [{path(), term()}],
									tree_item :: integer}).

%% Types
-type project_id() :: {integer(), integer(), integer()}.
-type path() :: string().
-type project_record() :: {project_id, #project{}}.

%% Server state
-record(state, {projects :: [project_record()],
								active_project :: project_id()}).


%% =====================================================================
%% Client API
%% =====================================================================

%% =====================================================================
%% @doc 

start()->
	gen_server:start({local, ?MODULE}, ?MODULE, [], []).
	    
			
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
		Path = ide_io:create_directory_structure(Parent, 
    	new_project_wx:get_name(Dialog), new_project_wx:get_path(Dialog)),
		Id = gen_server:call(?MODULE, {new_project, Path}),
		ide_projects_tree:add_project(Id, Path)
  catch
    throw:E -> 
			lib_dialog_wx:msg_error(Parent, E)
  end.
	
	
%% =====================================================================
%% @doc Open an existing project.

open_project(Frame) ->
	case lib_dialog_wx:get_existing_dir(Frame) of
		cancelled -> ok;
		Path ->
			Id = gen_server:call(?MODULE, {new_project, Path}),
			ide_projects_tree:add_project(Id, Path)
	end,
	ok.
	
	
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
	doc_manager:new_document_from_existing(Path, Contents, [project_id, ProjectId]),
	gen_server:call(?MODULE, {add_open_project, ProjectId, Path}),
	ok. 


%% =====================================================================
%% @doc Get the currently active project.
%% This is either the project to which the active document belongs, or
%% the last clicked item in the project tree if this is more recent.

get_active_project() ->
	wx_object:call(?MODULE, active_project).
	
	
%% =====================================================================
%% @doc Set the currently active project.

set_active_project(Project) ->
	wx_object:cast(?MODULE, {active_project, Project}).	
	
	
%% =====================================================================
%% @doc

% -spec get_open_projects() -> Result when
% 	Result :: [string()].
% 
% get_open_projects() ->
%   OpenProjects = ide_projects_tree:get_open_projects(),
%   get_open_projects(OpenProjects, []).
% get_open_projects([], Acc) ->
%   Acc;
% get_open_projects([{_,Path}|Projects], Acc) ->
%   get_open_projects(Projects, Acc ++ [Path]).

get_open_projects() ->
	gen_server:call(?MODULE, open_project_paths).
	
%% =====================================================================
%% @doc Returns a list containing all open 
%% documents belonging to the project Project.
	
% get_active_project_documents(Project, DocEts, Notebook) ->
% 	ets:foldl(
% 		fun(Record, Acc) ->
% 			case record_get_project(Record) of
% 				Project ->
% 					[record_get_key(Record) | Acc];
% 				_ -> Acc
% 			end
% 		end, [], DocEts).
		
		
%% =====================================================================
%% Callback functions
%% =====================================================================

init(Args) ->
	{ok, #state{projects=[]}}.
	
handle_info(Msg, State) ->
  io:format("Got Info (prefs) ~p~n",[Msg]),
  {noreply,State}.
    
handle_call({new_project, Path}, _From, State=#state{projects=Projects}) ->
	io:format("PROJECTS: ~p~n", [Projects]),
	Id = generate_id(),
	Record = {Id, #project{root=Path, open_files=[]}},
  {reply, Id, State#state{projects=[Record | Projects]}};
handle_call({add_open_project, Id, Path}, _From, State=#state{projects=Projects}) ->
	P=#project{open_files=Open} = proplists:get_value(Id, Projects),
	N = P#project{open_files=[Path | Open]},
	D = proplists:delete(Id, Projects),
  {reply, ok, State#state{projects=[{Id, N} | D]}};
handle_call(open_project_paths, _From, State=#state{projects=Projects}) ->
	Paths = lists:map(fun({_Id, Path}) -> Path#project.root end, Projects),
  {reply, Paths, State}.    
	
handle_cast(Msg, State) ->
  io:format("Got cast ~p~n",[Msg]),
  {noreply,State}.
    
%% Event catchall for testing
handle_event(Ev = #wx{}, State) ->
  io:format("Prefs event catchall: ~p\n", [Ev]),
  {noreply, State}.
    
code_change(_, _, State) ->
  {stop, not_yet_implemented, State}.

terminate(_Reason, State) ->
  io:format("TERMINATE PREFS~n").
    
		
%% =====================================================================
%% Internal functions
%% =====================================================================

generate_id() ->
	now().
	
insert_project() ->
	
	ok.