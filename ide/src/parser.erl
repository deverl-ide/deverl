-module(parser).

-compile(export_all).



%% =====================================================================
%% @doc Close the selected editor

start()->
    register(parser, spawn(?MODULE, loop, [])).


%% =====================================================================
%% @doc

parse_input(Message) ->
  M = Message++"\n",
  io:format("TO_PORT: ~p~n", [M]),
  port:call_port(M).


%% =====================================================================
%% @doc

parse_response(Response) ->
  io:format("RESPONSE: ~p~n", [Response]),
  ide_shell:load_response(Response).
  