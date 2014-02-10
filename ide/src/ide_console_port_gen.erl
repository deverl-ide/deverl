%% =====================================================================
%% @author
%% @copyright
%% @title
%% @version
%% @doc This module initalises and manages the port for the console.
%% @end
%% =====================================================================

-module(ide_console_port_gen).

-include("ide.hrl").

%% gen_server
-behaviour(gen_server).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

%% API
-export([start/0,
				 eval/1,
         eval/2,
				 close_port/0]).

%% Server state
-record(state, {port :: port(),
                respond :: boolean()
                }).


%% =====================================================================
%% Client API
%% =====================================================================

%% =====================================================================
%% @doc

-spec start() -> {ok, pid()} | ignore | {error,Error} when
  Error :: {already_started, pid()} | term().

start()->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% =====================================================================
%% @doc

-spec eval(string()) -> ok.

eval(Message) ->
	eval(Message, true).

-spec eval(string(), boolean()) -> ok.

eval(Message, Respond) ->
	gen_server:call(?MODULE, {call, Message, Respond}).


%% =====================================================================
%% @doc Close the port.
%% Don't attempt to write/read from the port after this!

-spec close_port() -> {pid(), close}.

close_port() ->
  ?MODULE ! {self(), close}.


%% =====================================================================
%% Callback functions
%% =====================================================================

init(_Args) ->
  %% get peth to erl from prefs
  #general_prefs{path_to_erl=Erl} = ide_sys_pref_gen:get_preference(general_prefs),
  
  Options = [use_stdio, exit_status],
	try open(Erl, Options) of
		Port ->
			{ok, #state{port=Port, respond=false}}
	catch
		_:_ ->
			{stop, no_port}
	end.

handle_call({call, Msg, Respond}, _From, #state{port=Port}=State) ->
  port_command(Port, Msg),
	{reply, ok, State#state{respond=Respond}}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({_From, close}, State) ->
  {stop, normal, State};
handle_info({'EXIT', Port, Reason}, #state{port=Port}=State) ->
  {stop, {port_terminated, Reason}, State};
handle_info({_Port, {data, Response}}, State=#state{respond=Respond}) ->
	case Respond of
		false ->
      ok;
		true ->
			ide_console_parser:parse_response(Response)
	end,
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate({port_terminated, _Reason}, _State) ->
  ok;
terminate(_Reason, _State) ->
  % port_close(Port),
	ok.


%% =====================================================================
%% Internal functions
%% =====================================================================

%% =====================================================================
%% @doc Open the port.
%% @throws
%% @private

-spec open(path(), Options) -> port() | no_return() when
  Options :: list().

open(Path, Options) ->
	try open_port({spawn_executable, Path}, Options) of
		Port ->
      Port
	catch
		_:_ ->
			throw(no_port)
	end.