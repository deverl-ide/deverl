%% =====================================================================
%% @author
%% @copyright
%% @title
%% @version
%% @doc
%% @end
%% =====================================================================

-module(ide_sys_pref_gen).

-include("ide.hrl").

%% gen_server
-behaviour(gen_server).
-export([init/1, handle_info/2, handle_call/3, handle_cast/2,
				 handle_event/2, code_change/3, terminate/2]).

%% API
-export([
        start/1,
        set_preference/2,
        get_preference/1,
        get_font/1,
        set_font/2
        ]).

%% Server state
-record(state, {prefs_table}).


%% =====================================================================
%% Client API
%% =====================================================================

%% =====================================================================
%% @doc

-spec start(Config) -> {ok, pid()} | ignore | {error, Error} when
  Config :: list(),
  Error  :: {already_started, pid()} | term().

start(Config) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).


%% =====================================================================
%% @doc Save a new user preference. To set a font preference always
%% use set_font/2.

-spec set_preference(term(), term()) -> ok.

set_preference(Key, Value) ->
  gen_server:cast(?MODULE, {Key, Value}).


%% =====================================================================
%% @doc Get a single preference from the saved preferences.
%% To retrieve a font always use get_font/1.

-spec get_preference(term()) -> term().

get_preference(Key) ->
  gen_server:call(?MODULE, Key).


%% =====================================================================
%% @doc Helper function to create a wxFont from the font record stored 
%% in the prefs.
  
-spec get_font(console_font | editor_font | log_font) -> wxFont:wxFont().

get_font(Name) ->
  FontRec = ide_sys_pref_gen:get_preference(Name),
  wxFont:new(FontRec#font.size,
             FontRec#font.family,
             FontRec#font.style,
             FontRec#font.weight,
             [{face, FontRec#font.facename}]).


%% =====================================================================
%% @doc Update the saved font preference for Name.

-spec set_font(Name, wxFont:wxFont()) -> ok when
  Name :: console_font | editor_font | log_font.           
             
set_font(Name, Font) ->
  FontRec = #font{size=wxFont:getPointSize(Font),
                  family=wxFont:getFamily(Font),
                  style=wxFont:getStyle(Font),
                  weight=wxFont:getWeight(Font),
                  facename=wxFont:getFaceName(Font)
                  },
  ide_sys_pref_gen:set_preference(Name, FontRec),
  ok.

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
	{ok, State}.

terminate(_Reason, _) ->
  ok.

%% =====================================================================
%% Internal functions
%% =====================================================================

%% =====================================================================
%% @doc Load the preferences from disk and transfer to ETS table.

-spec load_prefs() -> ets:ets().

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

-spec create_dets() -> ets:ets().

create_dets() ->
  PrefsTable = ets:new(prefs, []),
  insert_default_prefs(PrefsTable),
  write_dets(PrefsTable).


%% =====================================================================
%% @doc Write the preferences to disk.

-spec write_dets(ets:ets()) -> ets:ets().

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

-spec insert_default_prefs(ets:ets()) -> true.

insert_default_prefs(PrefsTable) ->
  [ets:insert(PrefsTable, {Key, Value}) || {Key, Value} <- ide_sys_pref_defs:get_defaults()],
  Folder = case os:type() of
    {_,linux} -> "erlang_projects";
    _ -> "ErlangProjects"
  end,
  ets:insert(PrefsTable, {project_directory, filename:join(wx_misc:getHomeDir(), Folder)}).
