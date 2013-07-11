-module(parser).

-compile(export_all).

start()->
    register(parser, spawn(?MODULE, loop, [])).
    
loop() ->
  receive
    Any ->
      io:format("MESSAGE, ~p~n",[Any]),
      parser:parse_response(Any)
  end,
  loop().


parse(Message, From) ->
  port:call_port(Message, From).
  
parse_response(Response) ->
  ide_shell:load_response(Response).