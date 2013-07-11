-module(parser).

-compile(export_all).



%% =====================================================================
%% @doc Close the selected editor

start()->
    register(parser, spawn(?MODULE, loop, [])).


%% =====================================================================
%% @doc
  
loop() ->
  receive
    Any ->
      io:format("MESSAGE, ~p~n",[Any]),
      parser:parse_response(Any)
  end,
  loop().


%% =====================================================================
%% @doc

parse(Message, From) ->
  port:call_port(Message, From).


%% =====================================================================
%% @doc

parse_response(Response) ->
  ide_shell:load_response(Response).