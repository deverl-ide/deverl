-module(node).
-compile(export_all).

-define(LINEMAX, 30).
-define(CHAR_MAX, 60).

start() ->
  register(shell, spawn(node, loop, [])).
	%% Start distributed erlang
  % case net_kernel:start([one, shortnames]) of
  %     {ok, Pid} -> ok;
  %     {error, _Reason} -> ok
  %   end,
  %   
  %   erlang:set_cookie(node(), secretcookie),
  % 
  % io:format("MASTER NODE: ~p~n", [node()]),
  %   io:format("LOCALHOST: ~p~n", [net_adm:localhost()]),
	
  %% Start new node
  % {ok, Node} = slave:start(net_adm:localhost(), two, "-setcookie secretcookie"),
  % {ok, Node} = slave:start(node(), two, "-setcookie secretcookie"),
  
  % {Path, Options} = case os:type() of
  %   {win32,_} ->
  %     {"C:\\Program Files\\erl5.10.3\\erts-5.10.3\\bin\\erl", [use_stdio]};
  %   {_,darwin} ->
  %     {"/usr/local/lib/erlang/erts-5.10.1/bin/erl", [use_stdio, exit_status]}; %% For Tom, for now.
  %   _Other ->
  %     {"/usr/local/lib/erlang/erts-5.10.2/bin/erl", [use_stdio, exit_status]}
  % end,
  %   
  % Port = open_port({spawn_executable, Path}, [use_stdio, 
  %                                      exit_status, 
  %                                      {args, ["-sname", "two", "-setcookie", "secretcookie"]}]),
                                       
  % Names = net_adm:names(),
  % io:format("NAMES: ~p~n", [Names]),
  
  % Result = net_adm:ping(two@tom),
  % io:format("PING: ~p~n", [Result]),
  
  % receive
  %   after 1000 ->
  %     ok
  % end,
  %     
  % {ok, Node} = slave:start(list_to_atom(net_adm:localhost()), two, "-setcookie secretcookie"),
  % io:format("NODE: ~p~n", [Node]),
  % 
  % SPid = spawn(Node, node, register, []),
  % io:format("PID: ~p~nISPID: ~p~n", [SPid, is_pid(SPid)]),
  % 
  % io:format("NODES: ~p~n", [nodes()]),
  % ok.
%  
% register() ->
%   register(waa, self()),
%   loop().


   
loop() ->
  receive
    Msg ->
      {ok, Tokens, _End} = erl_scan:string(Msg),
      {ok, Exprs} = erl_parse:parse_exprs(Tokens),
      try 
        {value, V, _Nb} = erl_eval:exprs(Exprs,[]),
        io:format("~p~n", [V])
      catch
        Class:Reason ->
          Stacktrace = erlang:get_stacktrace(),
          M = {self(),Class,{Reason,Stacktrace}},
          case do_catch(Class, Reason) of
              true ->
                  report_exception(Class, benign, {Reason,Stacktrace});
              false ->
                  %% We don't want the ERROR REPORT generated by the
                  %% emulator. Note: exit(kill) needs nothing special.
                  {links,LPs} = process_info(self(), links),
                  ER = nocatch(Class, {Reason,Stacktrace}),
                  lists:foreach(fun(P) -> exit(P, ER) end, LPs--[self()]),
                  report_exception(Class, {Reason,Stacktrace}),
                  % exit(normal)
                  loop()
          end
      end,  
      % Pid ! {result, Result},
      loop()
  end.
  
nocatch(throw, {Term,Stack}) ->
    {{nocatch,Term},Stack};
nocatch(error, Reason) ->
    Reason;
nocatch(exit, Reason) ->
    Reason.

do_catch(exit, restricted_shell_stopped) ->
    false;
do_catch(exit, restricted_shell_started) ->
    false;
do_catch(_Class, _Reason) ->
    case application:get_env(stdlib, shell_catch_exception) of
        {ok, true} ->
            true;
        _ ->
            false
    end.

report_exception(Class, Reason) ->
    report_exception(Class, serious, Reason).
        
report_exception(Class, Severity, {Reason,Stacktrace}) ->
    Tag = severity_tag(Severity),
    I = iolist_size(Tag) + 1,
    PF = fun(Term, I1) -> pp(Term, I1, not_used) end,
    SF = fun(M, _F, _A) -> (M =:= erl_eval) or (M =:= ?MODULE) end,
    Str = lib:format_exception(I, Class, Reason, Stacktrace, SF, PF),
    io:requests([{put_chars, latin1, Tag},
                 {put_chars, unicode, Str},
                 nl]).

severity_tag(fatal)   -> <<"*** ">>;
severity_tag(serious) -> <<"** ">>;
severity_tag(benign)  -> <<"* ">>.
   
pp(V, I, RT) ->
    pp(V, I, RT, enc()).

pp(V, I, RT, Enc) ->
    Strings =
        case application:get_env(stdlib, shell_strings) of
            {ok, false} ->
                false;
            _ ->
                true
        end,
    io_lib_pretty:print(V, ([{column, I}, {line_length, columns()},
                             {depth, ?LINEMAX}, {max_chars, ?CHAR_MAX},
                             {strings, Strings},
                             {record_print_fun, no}]
                            ++ Enc)).

columns() ->
    case io:columns() of
        {ok,N} -> N;
        _ -> 80
    end.

enc() ->
    case lists:keyfind(encoding, 1, io:getopts()) of
	false -> [{encoding,latin1}]; % should never happen
	Enc -> [Enc]
    end.