-module(parser).

-compile(export_all).



%% =====================================================================
%% @doc Close the selected editor

start()->
    register(parser, spawn(?MODULE, loop, [])).


%% =====================================================================
%% @doc

parse_input(Message) ->
  port:call_port(Message).


%% =====================================================================
%% @doc

parse_response(Response) ->
  ide_shell:load_response(Response).