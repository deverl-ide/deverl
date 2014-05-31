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
%% @doc A process responsible for loading saved preferences from disk
%% into an ETS table, performing lookups on the table for other
%% processes, and saving any changes to disk.
%% @end
%% =====================================================================

-module(deverl_sys_pref_gen).

-include("deverl.hrl").
-include_lib("wx/include/wx.hrl").
-include_lib("kernel/include/file.hrl").

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

-define(UNI_PATH_TO_ERTS, "Q:\\Erlang_VFS\\erl5.8.3\\erts-5.10.1\\bin").


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
  FontRec = deverl_sys_pref_gen:get_preference(Name),
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
  deverl_sys_pref_gen:set_preference(Name, FontRec),
  ok.

%% =====================================================================
%% Callback functions
%% =====================================================================
%% @hidden
init(Config) ->
  wx:set_env(proplists:get_value(wx_env, Config)),
  Table = case filelib:is_file(get_pref_file()) of
    true ->
      load_prefs();
    false ->
      create_dets()
  end,
  initialise_prefs(Table),
	{ok, #state{prefs_table=Table}}.
%% @hidden
handle_info(_, State) ->
	{noreply, State}.
%% @hidden
handle_call(Key, _From, State=#state{prefs_table=Table}) ->
  try
    ets:lookup_element(Table, Key, 2),
    {reply, ets:lookup_element(Table, Key, 2), State}
  catch
    error:_ -> % not in table (probably a new pref addition to an existing table), get from defaults. stops crash on boot when prefs have been added
      Defaults = deverl_sys_pref_defs:get_defaults(),
      case proplists:get_value(Key, Defaults, no_default) of
        no_default -> error("Requested preference does not exist");
        Pref -> {reply, Pref, State}
      end
  end.
%% @hidden
handle_cast(Tuple, State=#state{prefs_table=Table}) ->
  ets:insert(Table, Tuple), % overwrite existing
  write_dets(Table),
	{noreply, State}.
%% @hidden
handle_event(_, State) ->
	{noreply, State}.
%% @hidden
code_change(_, _, State) ->
	{ok, State}.
%% @hidden
terminate(_Reason, _) ->
  ok.

%% =====================================================================
%% Internal functions
%% =====================================================================

%% =====================================================================
%% @doc Validate the saved preferences.
%% Any invalid prefs should be replaced with the default.

initialise_prefs(Table) ->
  Defaults = deverl_sys_pref_defs:get_defaults(),
  
  %% Project dir
  ProjDir = ets:lookup_element(Table, project_directory, 2),
  DefaultDir = proplists:get_value(project_directory, Defaults),
  
  case ProjDir of 
    DefaultDir -> %% Using the default, ensure it exists (it wont when the IDE is started the very first time)
      ensure_proj_dir(DefaultDir);
    _UserDefined ->
      case file:read_file_info(ProjDir) of
        {error, _Reason0} ->
          %% Project dir invalid, inform user, reset to default
          Dlg = deverl_lib_dlg_wx:message(wx:null(), 
            [{caption, "Your project home directory " ++ ProjDir ++ " has been moved or deleted."},
             {text1, "We've reset it to " ++ wx_misc:getHomeDir()},
             {buttons, [?wxID_OK]}]),
          wxDialog:showModal(Dlg),
          wxDialog:destroy(Dlg),
          ets:update_element(Table, project_directory, {2, DefaultDir}),
          ensure_proj_dir(DefaultDir);
        _FileInfo0 ->
          ok 
      end
  end,
  
  %% General prefs
  GenPref0 = ets:lookup_element(Table, general_prefs, 2),
  
  %% Home env
  Home0 = GenPref0#general_prefs.home_env_var,
  GenPref1 = case file:read_file_info(Home0) of
    {error, _Reason1} ->
      %% Point to invalid directory, reset to default
      GenPref0#general_prefs{home_env_var=wx_misc:getHomeDir()};
    _FileInfo1 ->
      GenPref0
  end,
  
  %% TODO Remove all trace of this code for Uni
  %% Set the HOME env for those users who can't set it manually (Uni)
  case os:getenv("HOME") of
    false ->
      %% Set it
      os:putenv("HOME", GenPref1#general_prefs.home_env_var);
    _Home1 ->
      ok
  end,
  
  NotifyNotFound = fun(Name) ->
    Dlg1 = deverl_lib_dlg_wx:message(wx:null(), 
            [{caption, "Oops."},
             {text1, Name},
             {text2, "You can set the path manually in the preferences."},
             {buttons, [?wxID_OK]}]),
    wxDialog:showModal(Dlg1),
    wxDialog:destroy(Dlg1)
  end,
  
  %% Test path to erl
  GenPref2 = case GenPref1#general_prefs.path_to_erl of
    false -> %% default not found
      case areWeOnCampus("erl.exe") of
        false -> %% notify not found, disable ctrls
          NotifyNotFound("erl"),
          GenPref1;
        ErlPath -> %% save into prefs
          GenPref1#general_prefs{path_to_erl=ErlPath}
      end;
    _Path0 -> %% validate the path
      GenPref1
  end,
    
  % Test path to erlc
  GenPref3 = case GenPref2#general_prefs.path_to_erlc of
    false -> %% default not found
      case areWeOnCampus("erlc.exe") of
        false -> %% notify not found, disable ctrls
          NotifyNotFound("erlc"),
          GenPref2;
        ErlcPath -> %% save into prefs
          GenPref2#general_prefs{path_to_erlc=ErlcPath}
      end;
    _Path1 -> %% validate the path
      GenPref2
  end,
  
  %% Test path to dialyzer
  GenPref4 = case GenPref3#general_prefs.path_to_dialyzer of
    false -> %% default not found
      case areWeOnCampus("dialyzer.exe") of
        false -> %% notify not found, disable ctrls
          NotifyNotFound("dialyzer"),
          GenPref3;
        DlzrPath -> %% save into prefs
          GenPref3#general_prefs{path_to_dialyzer=DlzrPath}
      end;
    _Path2 -> %% validate the path
      GenPref3
  end,
  
  %% Update general prefs
  ets:update_element(Table, general_prefs, {2, GenPref4}),
  
  %% Write any updates
  write_dets(Table),
  ok.

areWeOnCampus(Name) ->
  Path = filename:join(?UNI_PATH_TO_ERTS, Name),
  case filelib:is_file(Path) of
    true -> %% Yes, we probably are
      Path;
    false ->
      ok
  end.

%% =====================================================================
%% @doc A text prompt for the user to manualy add the path to the
%% exes when automatic finding has failed.
  
prompt_for_path(Exe) ->
  Dlg = wxTextEntryDialog:new(wx:null(),
    "The " ++ atom_to_list(Exe) ++ " executable could not be found automatically,\n"
    "please enter the path:"),
  case wxTextEntryDialog:showModal(Dlg) of
    ?wxID_OK ->
      %% Validate the input
      ok;
    ?wxID_CANCEL ->
      %% Try again (Can't do this forever)
      prompt_for_path(Exe)
  end.


%% =====================================================================
%% @doc Ensure the default project directory exists.
%% Called when the IDE is started for the first time, and if the
%% project directory has to be reset for any reason.

ensure_proj_dir(DefaultDir) ->
  case filelib:is_dir(DefaultDir) of
    false ->
      file:make_dir(DefaultDir);
    true ->
      ok
  end.
  

%% =====================================================================
%% @doc Load the preferences from disk and transfer to ETS table.

-spec load_prefs() -> ets:ets().

load_prefs() ->
  case dets:open_file(get_pref_file(), []) of
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
  case dets:open_file(get_pref_file(), []) of
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
  [ets:insert(PrefsTable, {Key, Value}) || {Key, Value} <- deverl_sys_pref_defs:get_defaults()],
  ok.


%% =====================================================================
%% @doc Get the system pref file.

get_pref_file() ->
  filename:join(filename:dirname(code:which(?MODULE)), system_prefs).