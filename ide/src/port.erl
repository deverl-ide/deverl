-module(port).

-export([start/0, read/0, call_port/1, close_port/0]).


%% =====================================================================
%% @doc 

start()->
    register(?MODULE, spawn(?MODULE, read, [])).


%% =====================================================================
%% @doc 
   
read() ->
	Port = open_port({spawn,"/usr/local/lib/erlang/erts-5.10.2/bin/erl"},[use_stdio]),
	do_read(Port).


%% =====================================================================
%% @doc 

do_read(Port) ->
  receive
    {Port,{data,Data}} ->
      % io:format("Data:~p~n",[Data]);
      parser:parse_response(Data);
    {call, Msg} ->
      port_command(Port, Msg);
    {From, close} ->
      Port ! {From, close}
  end,
  do_read(Port). 


%% =====================================================================
%% @doc

call_port(Message) ->
	?MODULE ! {call, Message}.


%% =====================================================================
%% @doc Close the port.
%% Don't attempt to write/read from the port after this!

close_port() ->
  ?MODULE ! {self(), close}.