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

-export([loop/1]).

%% =====================================================================
%% Client API
%% =====================================================================

%% =====================================================================
%% @doc Close the selected editor

start()->
    register(?MODULE, spawn(?MODULE, loop, [[]])).


%% =====================================================================
%% @doc

parse_input(Message) ->
	M = Message ++ io_lib:nl(),
	console_port:eval(M).


%% =====================================================================
%% @doc
% 
% parse_response(Response) ->
%   io:format("RESPONSE FROM PORT: ~p~n", [Response]),
%   console_wx:append_to_console(Response).
% 
% loop(_) -> ok.

% The parser now makes use of a receive loop to assemble responses into complete units.
% Previously each line (at newline) was passed back to the console when received so:
% "** exception error: an error occurred when evaluating an arithmetic expression\n
%      in operator  +/2\n
%              called as tom + 5\n"
% was passed back as three lines.
% The parser now assembles the above into a response (which it is), and passes it back to
% the console only when complete, i.e. the parser receives a prompt ("4> \n").

% This refactoring of the console modules allows us to manage the prompt ourselves which
% gives us much greater flexibility when writing to the console, and allows us to hide
% certain operations and/or output from the user (i.e cd(), l()). We can also write at any position without interfering with 
% the prompt (command number), which A) looked ugly, and B) became out of sync (jump from 2 to 4, for example, if we deleted a line).
% The prompt returned from the port is now discarded, as is the newline character at the end of each response (but not those within a long response).




parse_response(Response) ->
  ?MODULE ! {data, Response}.
  
loop(R0) ->
  receive
    {data, R1} ->
      %% Split data at newline
      case build_response(re:split(R1, "\\R", [{newline, any}, {return, list}, trim]), R0) of
        {prompt, []} ->
          loop([]); %% Single prompt, ignore, start over
        {prompt, R2} ->
          console_wx:append_to_console(R2),
          loop([]); %% Complete response, start over
        {incomplete, R2} ->
          loop(R2) %% No prompt yet, loop again with current data
      end;
      % loop([R1 | R0]);
    {waa, _R} ->
      ok
  end.
  
build_response([], Acc) ->
  {incomplete, Acc};
build_response([H], Acc) ->
  case is_prompt(H) of
    true -> %% Ok, done
      {prompt, Acc};
    false ->
      build_response([], Acc ++ H)
  end;
build_response([H|T], Acc) ->
  %% NOTE currently two prompts on one line would cause any data after
  %% the first to be lost i.e. "DATA 4> \nLOST_DATA 5> \n"
  case is_prompt(H) of
    true -> %% Ok, done
      {prompt, Acc};
    false ->
      build_response(T, Acc ++ H ++ "\n")
  end.
  
is_prompt(Cmd) ->
  case re:run(Cmd, "^\\d+> $", []) of
    nomatch ->
      false;
    {match, _} ->
      true
  end.