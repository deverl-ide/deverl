%% =====================================================================
%% @author
%% @copyright
%% @title
%% @version
%% @doc 
%% @end
%% =====================================================================

-module(dlg_ld).

-include_lib("wx/include/wx.hrl").

-compile(export_all).

% %% For xrc_cb
% show_dlg(Frame, Name) ->
%   Env = wx:get_env(),
%   dlgxrc:start(Frame, Env),
%   ok.
  
show_dlg(Frame, _Name) ->
  Dlg = dlgxrc:new(Frame),
  case wxDialog:showModal(Dlg) of
    ?wxID_OK ->
      io:format("OK~n");
    ?wxID_CANCEL ->
      D = dlgxrc:test_get_dlg(Dlg),
      io:format("CANCEL: ~p~n", [D])
  end,
  dlgxrc:destroy(Dlg),
  ok.
  
win_var(Dlg) ->
  %% Smaller dialogs etc look better on OSX
  case os:type() of
    {_, darwin} ->
      wxDialog:setWindowVariant(Dlg, ?wxWINDOW_VARIANT_SMALL);
     _ -> ok
  end.
  

kill_wait(Pid) ->
  case is_process_alive(Pid) of
    true -> 
      % io:format("not_dead~n"),
      kill_wait(Pid);
    _ ->
      % io:format("dead~n"),
      ok
  end.