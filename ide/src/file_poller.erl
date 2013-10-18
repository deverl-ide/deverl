-module(file_poller).

-behaviour(gen_server).
-export([init/1, handle_info/2, handle_call/3, handle_cast/2, 
				 handle_event/2, terminate/2]).

%% Client API
-export([start/1, stop/0]).

-record(state, {type, editor_pid, root_path, root_lm}).

-define(INTERVAL, 500).

start(Config) ->
	gen_server:start(?MODULE, Config, []).
	
init(Config) ->
	Path = proplists:get_value(path, Config),
	State = case file_type(Path) of
		file -> 
			DateTime = filelib:last_modified(Path),
			#state{type=file, root_path=Path, root_lm=DateTime};
		directory -> 
			undefined
	end,
	
	% Start timer
	erlang:send_after(?INTERVAL, self(), trap),
	{ok, State#state{editor_pid=proplists:get_value(editor_pid, Config)}}.
	
handle_info(trap, State=#state{type=file, root_path=Path, root_lm=Lm}) ->
	Mod = filelib:last_modified(Path),
	case Mod =:= Lm of
		true -> ok;
		false ->
			io:format("File poller: file modified.~n")
	end,
	erlang:send_after(?INTERVAL, self(), trap),
	{noreply, State#state{root_lm=Mod}}.
	
handle_call(_, _From, State) ->
	{noreply, State}.
	
handle_cast(stop, State) ->
	{stop, normal, State}.
	
handle_event(_, State) ->
	{noreply, State}.

terminate(_Reason, _) ->
	io:foramt("TERMINATE POLLER"),
  ok.

stop() ->
	gen_server:cast(?MODULE, stop).
	
file_type(Path) ->
	IsRegular = filelib:is_regular(Path),
	case IsRegular of
		true ->
			file;
		false ->
			directory
	end.