-module(parser).

-compile(export_all).



%% =====================================================================
%% @doc Close the selected editor

start()->
    register(?MODULE, spawn(?MODULE, loop, [])).


%% =====================================================================
%% @doc

parse_input(Message) ->
	M = Message ++ io_lib:nl(),
	port:call_port(M).


%% =====================================================================
%% @doc

parse_response(Response) ->
	M = Response,
	ide_shell:load_response(M).
  
