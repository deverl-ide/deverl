%% =====================================================================
%% @author
%% @copyright
%% @title
%% @version
%% @doc
%% @end
%% =====================================================================

-module(deverl_console_parser).

-export([start/0,
		     parse_input/1,
		     parse_response/1]).

-export([init/0]).

%% =====================================================================
%% Client API
%% =====================================================================

%% =====================================================================
%% @doc Close the selected editor

-spec start() -> {ok, pid()}.

start()->
    Pid =  spawn_link(?MODULE, init, []),
    register(?MODULE, Pid),
    {ok, Pid}.


%% =====================================================================
%% @doc

-spec parse_input(string()) -> ok.

parse_input(Message) ->
	M = Message ++ io_lib:nl(),
	deverl_console_port_gen:eval(M).


%% =====================================================================
%% @doc

-spec parse_response(string()) -> {data, string()}.

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
      deverl_console_wx:append_command({response, complete});
    false ->
      deverl_console_wx:append_command({response, R0})
  end.

is_prompt(Cmd) ->
  case re:run(Cmd, "^\\d+> $", []) of
    nomatch ->
      false;
    {match, _} ->
      true
  end.
