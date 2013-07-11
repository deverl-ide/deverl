-module(port).


%%     /usr/local/lib/erlang/erts-5.10.1/bin/erl
%% TESTING GIT 

-compile(export_all).

start()->
    register(port, spawn(?MODULE, read, [])).
   
read() ->
  Port = open_port({spawn,"/usr/local/lib/erlang/erts-5.10.1/bin/erl"},[stream]),
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
      io:format("MESSAGE, ~p~n",[Any])
      % parser:parse_response(Any)
      % ide_shell:load_response(Any)
  end,
  do_read(Port).

call_port(Message) ->
  port ! Message. 