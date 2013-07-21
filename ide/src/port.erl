-module(port).

-export([start/0, read/0, call_port/1]).


%% =====================================================================
%% @doc 

start()->
    register(?MODULE, spawn(?MODULE, read, [])).


%% =====================================================================
%% @doc 
   
read() ->
	Port = open_port({spawn,"/usr/local/lib/erlang/erts-5.10.1/bin/erl"},[use_stdio]),
	do_read(Port).


%% =====================================================================
%% @doc 

do_read(Port) ->
  receive
    {Port,{data,Data}} ->
      % io:format("Data:~p~n",[Data]);
      parser:parse_response(Data);
    {call, Msg} ->
      port_command(Port, Msg)
  end,
  do_read(Port). 


%% =====================================================================
%% @doc

call_port(Message) ->
	?MODULE ! {call, Message}.
