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
  loop().

loop() ->
  receive
    {data, R1} ->
      %% Split data at newline
      Split = re:split(R1, "(\\R)", [{newline, any}, {return, list}, trim]),
      lists:foreach(fun eval_response/1, Split),
      loop()
  end.
  
eval_response(R0) ->
  case is_prompt(R0) of
    true ->
      console_wx:append_command({response, complete});
    false ->
      console_wx:append_command({response, R0})
  end.

is_prompt(Cmd) ->
  case re:run(Cmd, "^\\d+> $", []) of
    nomatch ->
      false;
    {match, _} ->
      true
  end.