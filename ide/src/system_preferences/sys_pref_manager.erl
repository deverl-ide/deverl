-module(sys_pref_manager).
-export([
        start/1,
        set_preference/2,
        get_preference/1
        ]).

-behaviour(gen_server).
-export([init/1, handle_info/2, handle_call/3, handle_cast/2, 
				 handle_event/2, code_change/3, terminate/2]).

-record(state, {prefs_table}).


%% =====================================================================
%% Client API
%% =====================================================================

%% =====================================================================
%% @doc 

start(Config) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).
  

%% =====================================================================
%% @doc 

set_preference(Key, Value) ->
  gen_server:cast(?MODULE, {Key, Value}).


%% =====================================================================
%% @doc 

get_preference(Key) ->
  gen_server:call(?MODULE, Key).
  

%% =====================================================================
%% Callback functions
%% =====================================================================
	
init(Config) ->
  wx:set_env(proplists:get_value(wx_env, Config)),
  Table = case filelib:is_file(system_prefs) of
    true ->
      load_prefs();
    false ->
      create_dets()
  end,
	{ok, #state{prefs_table=Table}}.
	
handle_info(_, State) ->
	{noreply, State}.

handle_call(Key, _From, State=#state{prefs_table=Table}) ->
	{reply, ets:lookup_element(Table, Key, 2), State}.
	
handle_cast({Key, Value}, State=#state{prefs_table=Table}) ->
  ets:update_element(Table, Key, {2, Value}),
  write_dets(Table),
	{noreply, State}.
	
handle_event(_, State) ->
	{noreply, State}.
	
code_change(_, _, State) ->
	{stop, ignore, State}.

terminate(_Reason, _) ->
	io:format(""),
  ok.

%% =====================================================================
%% Internal functions
%% =====================================================================

%% =====================================================================
%% @doc Load the preferences from disk and transfer to ETS table.

load_prefs() ->
  case dets:open_file(system_prefs, []) of
    {ok, DetsTable} ->
      PrefsTable = dets:to_ets(DetsTable, ets:new(prefs ,[])),
      dets:close(DetsTable),
      PrefsTable;
    {error, _Reason} ->
      PrefsTable = ets:new(prefs, []),
      insert_default_prefs(PrefsTable),
      PrefsTable
  end.
  

%% =====================================================================
%% @doc 
  
create_dets() ->
  PrefsTable = ets:new(prefs, []),
  insert_default_prefs(PrefsTable),
  write_dets(PrefsTable).
  
  
%% =====================================================================
%% @doc Write the preferences to disk.

write_dets(PrefsTable) ->
  case dets:open_file(system_prefs, []) of
    {ok, DetsTable} ->
      ets:to_dets(PrefsTable, DetsTable),
      dets:close(DetsTable),
      PrefsTable;
    {error, _Reason} ->
      PrefsTable
  end.
  
  
%% =====================================================================
%% @doc 
  
insert_default_prefs(PrefsTable) ->
  [ets:insert(PrefsTable, {Key, Value}) || {Key, Value} <- sys_pref_defaults:get_defaults()],
  ets:insert(PrefsTable, {project_directory, wx_misc:getHomeDir() ++ "/erlang_projects"}).