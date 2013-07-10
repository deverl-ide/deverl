-module(ide_shell_port).


%%     /usr/local/lib/erlang/erts-5.10.1/bin/erl


-compile(export_all).

start()->
    register(port, spawn(?MODULE , read, [])).
   
read() ->
  Port = open_port({spawn,"/usr/local/lib/erlang/erts-5.10.1/bin/erl"},[binary,{line, 255}]),
  do_read(Port).

do_read(Port) ->
  receive
    {Port,{data,Data}} ->
        io:format("Data: ~p~n",[Data]);
    {Port,eof} ->
      read();
    {go, Go} ->
        Port ! {self(), {command, Go}};
    Any ->
      io:format("No match fifo_client:do_read/1, ~p~n",[Any])
  end,
  do_read(Port).
  
call_port(Message) ->
  % M = Message ++ ""
  port ! Message.
 