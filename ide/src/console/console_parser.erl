%% =====================================================================
%% @author
%% @copyright
%% @title
%% @version
%% @doc 
%% @end
%% =====================================================================

-module(console_parser).

-export([start/0,
		  parse_input/1,
		  parse_response/1]).


%% =====================================================================
%% Client API
%% =====================================================================

%% =====================================================================
%% @doc Close the selected editor

start()->
    register(?MODULE, spawn(?MODULE, loop, [])).


%% =====================================================================
%% @doc

parse_input(Message) ->
  io:format("CALL PORT~n"),
	M = Message ++ io_lib:nl(),
	console_port:call_port(M).


%% =====================================================================
%% @doc

parse_response(Response) ->
  io:format("RESPONSE FROM PORT: ~p~n", [Response]),
  M = check_for_prompt(Response),
	console_wx:append_to_console(M).
  
  
%% =====================================================================
%% @doc

check_for_prompt(Response) ->
  case re:run(Response, "(.+)\\d> $", [{capture, [1]}]) of
    nomatch ->
      "";
    {match, [{Start, Length}]} ->
      R = string:substr(Response, Start, Length),
      io:format("RESPONSE: ~p~n", [R]),
      R
  end.
