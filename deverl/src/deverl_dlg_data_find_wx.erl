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
%% @doc Create the data to send the the deverl_dlg_find_wx when started.
%% @end
%% =====================================================================


-module(deverl_dlg_data_find_wx).

-include("deverl.hrl").

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

-export_type([deverl_dlg_data_find_wx/0]).

-type deverl_dlg_data_find_wx() :: pid().

-record(state, {find_str :: string(),
                replace_str :: string(),
                options :: integer(),
                search_loc :: integer()
               }).

%% =====================================================================
%% Client API
%% =====================================================================

%% =====================================================================
%% @doc

-spec new() -> deverl_dlg_data_find_wx().

new() ->
  spawn(?MODULE, init, []).


%% =====================================================================
%% Internal functions
%% =====================================================================

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
%% @doc

-spec set_find_string(deverl_dlg_data_find_wx(), string()) -> ok.

set_find_string(This, String) ->
  This ! {set_find, String},
  ok.


%% =====================================================================
%% @doc

-spec set_replace_string(deverl_dlg_data_find_wx(), string()) -> ok.

set_replace_string(This, String) ->
  This ! {set_rep, String},
  ok.


%% =====================================================================
%% @doc Set the flags for the search.
%% Possible flags are IGNORE_CASE, WHOLE_WORD, START_WORD, REGEX.
%% Combine flags by adding them.

-spec set_options(deverl_dlg_data_find_wx(), integer()) -> ok.

set_options(This, Options) ->
  This ! {set_options, Options},
  ok.


%% =====================================================================
%% @doc Set where find searches
%% Choose one of FIND_LOC_PROJ, FIND_LOC_OPEN, FIND_LOC_DOC

-spec set_search_location(deverl_dlg_data_find_wx(), integer()) -> ok.

set_search_location(This, Int) ->
  This ! {set_search_loc, Int},
  ok.


%% =====================================================================
%% @doc

-spec get_data(deverl_dlg_data_find_wx()) -> Data when
  Data :: [tuple()].

get_data(This) ->
  This ! {self(), {get, data}},
  receive {result, Data} ->
     Data
  end.


%% =====================================================================
%% @doc

-spec get_find_string(deverl_dlg_data_find_wx()) -> string().

get_find_string(This) ->
  This ! {self(), {get, find_str}},
  receive {result, Str} ->
     Str
  end.


%% =====================================================================
%% @doc

-spec get_replace_string(deverl_dlg_data_find_wx()) -> string().

get_replace_string(This) ->
  This ! {self(), {get, replace_str}},
  receive {result, Str} ->
     Str
  end.


%% =====================================================================
%% @doc

-spec get_options(deverl_dlg_data_find_wx()) -> integer().

get_options(This) ->
  This ! {self(), {get, options}},
  receive {result, Options} ->
     Options
  end.


%% =====================================================================
%% @doc

-spec get_search_location(deverl_dlg_data_find_wx()) -> integer().

get_search_location(This) ->
  This ! {self(), {get, search_loc}},
  receive {result, Loc} ->
     Loc
  end.


%% =====================================================================
%% @doc

-spec stop(deverl_dlg_data_find_wx()) -> ok.

stop(This) ->
  This ! stop,
  ok.
