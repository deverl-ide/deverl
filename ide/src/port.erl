-module(port).

-export([start/0, init/0, call_port/1, close_port/0]).

%% This module generates a throw if the port cannot be opened.
%% ATM this is not caught, but the exit is trapped in ide.erl.
%% The error caught in ide.erl is a badarg, because spawn()
%% fails, and so link has a badarg.

%% =====================================================================
%% @doc 

start()->
	register(?MODULE, link(spawn(?MODULE, init, []))).


%% =====================================================================
%% @doc 
%% @private
   
init() ->
	{Path, Options} = case os:type() of
		{win32,_} ->
			{"C:\Program Files\erl5.10.2\erts-5.10.2\bin\erl", []};
		Other ->
			{"/usr/local/lib/erlang/erts-5.10.2/bin/kerl", [use_stdio]}
	end,
	open(Path, Options).


%% =====================================================================
%% @doc Open the port.
%% @throws 
%% @private

open(Path, Options) ->
	try open_port({spawn_executable, Path}, Options) of
		Port ->
			do_read(Port)
	catch
		_:_ ->
			throw(no_port)
	end.

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