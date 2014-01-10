%% Create the data to send the the ide_find_dlg_wx when started.

-module(ide_find_dlg_data_wx).

-include("../../include/ide.hrl").
               
-export([new/0,
         set_find_string/2,
         set_replace_string/2,
         set_options/2,
         set_search_location/2,
         get_find_string/1,
         get_replace_string/1,
         get_options/1,
         get_search_location/1,
         get_data/1,
         stop/1]).

-export([init/0]).

-export_type([ide_find_dlg_data_wx/0]).
-type ide_find_dlg_data_wx() :: pid().

-record(state, {find_str :: string(),
                replace_str :: string(),
                options :: integer(),
                search_loc
               }).

-spec new() -> ide_find_dlg_data_wx().

new() ->
  spawn(?MODULE, init, []).
  
init() ->
  %% Define default data
  loop(#state{}).
  
loop(State) ->
  receive Msg ->
    NewState = case Msg of
      stop ->
        exit(normal);
      {set_find, Str} ->
        State#state{find_str=Str};
      {set_rep, Str} ->
        State#state{replace_str=Str};
      {set_options, Options} ->
        State#state{options=Options};
      {set_search_loc, Loc} ->
        State#state{search_loc=Loc};
      {From, {get, data}} ->
        From ! {result,[{find_str, State#state.find_str}, 
                {replace_str, State#state.replace_str},
                {options, State#state.options},
                {search_loc, State#state.search_loc}]},
        State;
      {From, {get, find_str}} ->
        From ! {result, State#state.find_str},
        State;
      {From, {get, replace_str}} ->
        From ! {result, State#state.replace_str},
        State;
      {From, {get, options}} ->
        From ! {result, State#state.options},
        State;
      {From, {get, search_loc}} ->
        From ! {result, State#state.search_loc},
        State
    end
  end,
  loop(NewState).
      

  
%% =====================================================================
%% @doc API

set_find_string(This, String) ->
  This ! {set_find, String},
  ok.
  
  
%% =====================================================================
%% @doc API
set_replace_string(This, String) ->
  This ! {set_rep, String},
  ok.
  
  
%% =====================================================================
%% @doc Set the flags for the search.
%% Possible flags are IGNORE_CASE, WHOLE_WORD, START_WORD, REGEX.
%% Combine flags by adding them.

-spec set_options(ide_find_dlg_data_wx(), integer()) -> 'ok'.

set_options(This, Options) ->
  This ! {set_options, Options},
  ok.
  
  
%% =====================================================================
%% @doc Set where find searches
%% Choose one of FIND_LOC_PROJ, FIND_LOC_OPEN, FIND_LOC_DOC

set_search_location(This, Int) ->
  This ! {set_search_loc, Int},
  ok.
  
  
%% =====================================================================
%% @doc API

get_data(This) ->
  This ! {self(), {get, data}},
  receive {result, Data} ->
     Data
  end.
  
  
%% =====================================================================
%% @doc API   

get_find_string(This) ->
  This ! {self(), {get, find_str}},
  receive {result, Str} ->
     Str
  end.
  
  
%% =====================================================================
%% @doc API  

get_replace_string(This) ->
  This ! {self(), {get, replace_str}},
  receive {result, Str} ->
     Str
  end.
  
  
%% =====================================================================
%% @doc API  

get_options(This) ->
  This ! {self(), {get, options}},
  receive {result, Options} ->
     Options
  end.
  
  
%% =====================================================================
%% @doc API  

get_search_location(This) ->
  This ! {self(), {get, search_loc}},
  receive {result, Loc} ->
     Loc
  end.
  
  
%% =====================================================================
%% @doc API  
stop(This) ->
  This ! stop,
  ok.
