%% =====================================================================
%% @author
%% @copyright
%% @title
%% @version
%% @doc 
%% @end
%% =====================================================================

-module(test).

-include_lib("wx/include/wx.hrl").

-compile(export_all).

wx_object() ->
  WX = init(),
  show_dlg(new_proj_wxobject, WX).
  
callback() ->
  WX = init(),
  new_proj_callbacks:start(WX,WX).

init() ->
  WX = wx:new(),
  Xrc = wxXmlResource:get(),
  wxXmlResource:initAllHandlers(Xrc),
  true = wxXmlResource:load(Xrc, "dlgs.xrc"),
  WX.
  
show_dlg(Mod, Parent) ->
  Dlg = Mod:start(Parent),
  case wxDialog:showModal(Dlg) of
    ?wxID_OK ->
      io:format("OK~n");
    ?wxID_CANCEL ->
      io:format("CANCEL~n")
  end,
  % Mod:destroy(Dlg),
  wxDialog:destroy(Dlg),
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