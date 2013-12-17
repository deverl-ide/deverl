%% =====================================================================
%% @author
%% @copyright
%% @title
%% @version
%% @doc This module monitors a file for external updates.
%% @end
%% =====================================================================

-module(file_poller).

%% gen_server
-behaviour(gen_server).
-export([init/1, handle_info/2, handle_call/3, handle_cast/2, 
				 handle_event/2, code_change/3, terminate/2]).

%% API
-export([start/1, stop/0]).

%% Macros
-define(INTERVAL, 500).

%% Server state
-record(state, {type, editor_pid, root_path, root_lm}).


%% =====================================================================
%% Client API  
%% =====================================================================

start(Config) ->
	gen_server:start(?MODULE, Config, []).
	

stop() ->
	gen_server:cast(?MODULE, stop).
	
	
update_path() ->
	ok.
	
	
%% =====================================================================
%% Callback functions
%% =====================================================================
	
init(Config) ->
	Path = proplists:get_value(path, Config),
	State = case file_type(Path) of
		file -> 
			DateTime = filelib:last_modified(Path),
			#state{type=file, root_path=Path, root_lm=DateTime};
		directory -> ok %% undefined behaviour
	end,
	
	% Start timer
	erlang:send_after(?INTERVAL, self(), trap),
	{ok, State#state{editor_pid=proplists:get_value(editor_pid, Config)}}.
	
handle_info(trap, State=#state{type=file, root_path=Path, root_lm=Lm}) ->
	Mod = filelib:last_modified(Path),
	case Mod of
		0 -> 
			%% Prompt user to save or close the file
			file_not_found();
			%% Returns the new path, update the state
		Lm -> 
			ok;
		_ ->
			%% save the changes to the file
			%% DONE THROUGH DOC_MANAGER
      ok
	end,
	erlang:send_after(?INTERVAL, self(), trap),
	{noreply, State#state{root_lm=Mod}}.
	
file_not_found() ->
	ok.	

handle_call(_, _From, State) ->
	{noreply, State}.
	
handle_cast(stop, State) ->
	{stop, normal, State}.
	
handle_event(_, State) ->
	{noreply, State}.
	
code_change(_, _, State) ->
	{stop, ignore, State}.

terminate(_Reason, _) ->
  ok.


%% =====================================================================
%% Internal functions
%% =====================================================================
	
file_type(Path) ->
	IsRegular = filelib:is_regular(Path),
	case IsRegular of
		true ->
			file;
		false ->
			directory
	end.
