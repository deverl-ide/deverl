%% =====================================================================
%% @author
%% @copyright
%% @title
%% @version
%% @doc
%% @end
%% =====================================================================

-module(console_parser).

-export([start/0,
		     parse_input/1,
		     parse_response/1]).

-export([init/0]).

%% =====================================================================
%% Client API
%% =====================================================================

%% =====================================================================
%% @doc Close the selected editor

start()->
    Pid =  spawn_link(?MODULE, init, []),
    register(?MODULE, Pid),
    {ok, Pid}.


%% =====================================================================
%% @doc

parse_input(Message) ->
	M = Message ++ io_lib:nl(),
	console_port:eval(M).


%% =====================================================================
%% @doc OLD IMPLEMENTATION - KEPT FOR COMPARISON
%
% parse_response(Response) ->
%   io:format("RESPONSE FROM PORT: ~p~n", [Response]),
%   console_wx:append_command(Response).
%
% loop(_) -> ok.


%% =====================================================================
%% @doc

parse_response(Response) ->
  ?MODULE ! {data, Response}.


%% =====================================================================
%% Internal functions
%% =====================================================================

init() ->
  process_flag(trap_exit, true), %% Die when the parent process dies
  loop([]).

loop(R0) ->
  receive
    {data, R1} ->
      %% Split data at newline
      io:format("R1: ~p~n", [R1]),
      io:format("SPLIT: ~p~n",[re:split(R1, "(\\R)", [{newline, any}, {return, list}, trim])]),
      case build_response(re:split(R1, "(\\R)", [{newline, any}, {return, list}, trim]), R0) of
        {prompt, []} ->
          loop([]); %% Single prompt, ignore, start over
        {prompt, R2} ->
          io:format("R2: ~p~n", [R2]),
          console_wx:append_command(R2),
          loop([]); %% Complete response, start over
        {incomplete, R2} ->
          loop(R2) %% No prompt yet, loop again with current data
      end;
    {waa, _R} ->
      ok
  end.

build_response([], Acc) ->
  {incomplete, Acc};
build_response(["\n"], Acc) ->
  {incomplete, Acc};
%build_response([[],"\n"], Acc) -> 
  %{incomplete, Acc};
build_response([H], Acc) ->
  case is_prompt(H) of
    true -> %% Ok, done
      {prompt, Acc};
    false ->
      build_response([], Acc ++ H)
  end;
build_response([H|T], Acc) ->
  %% NOTE currently two prompts on one line would cause any data after
  %% the first to be lost i.e. "DATA 4> \nLOST_DATA 5> \n",
  %% This should never happen however, if it does there's an easy fix
  case is_prompt(H) of
    true -> %% Ok, done
      {prompt, Acc};
    false ->
      build_response(T, Acc ++ H)
  end.

is_prompt(Cmd) ->
  case re:run(Cmd, "^\\d+> $", []) of
    nomatch ->
      false;
    {match, _} ->
      true
  end.

remove_nl(L) ->
<<<<<<< HEAD
  case lists:last(L) of
    10 -> %% newline
      lists:sublist(L, length(L) - 1);
    _ ->
      L
  end.
=======
  string:strip(L, both, $\n).
>>>>>>> 5d9697d5e5aa37b2ad3a0efb98850040bbb6a7cb
