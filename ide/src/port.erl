-module(port).

-export([call_port/1]).

start()->
    register(spawn(?MODULE, read, [])).
   
read() ->
  Port = open_port({spawn,"/usr/local/lib/erlang/erts-5.10.1/bin/erl"},[use_stdio]),
  do_read(Port).

do_read(Port) ->
  receive
    {Port,{data,Data}} ->
    	io:format("Data: ~p~n",[Data]);
    {call, Msg} ->
      port_command(Port, Msg)
  end,
  do_read(Port). 

call_port(Message) ->
  port ! {call, Message}.