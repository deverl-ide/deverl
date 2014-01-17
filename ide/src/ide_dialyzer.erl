%% =====================================================================
%% @author Tom Richmond <tr201@kent.ac.uk>
%% @author Mike Quested <mdq3@kent.ac.uk>
%% @copyright 2014 Tom Richmond, Mike Quested
%% @version 1
%% @doc Starts an instance of the IDE. Builds all wx components.
%% @end
%% =====================================================================

-module(ide_dialyzer).

-include_lib("wx/include/wx.hrl").

-export([
  run/1
	]).
  
%% Spawned
-export([
  build_plt/2
]).
	
-record(file_plt, {version,
                   file_md5_list,
                   info, contracts,
                   callbacks,
                   types,
                   exported_types,
                   mod_deps,
                   implementation_md5}).

run(Parent) ->
  case check_plt() of
    {ok, Plt} ->
      run(Parent, Plt);
    {error, _} ->
      init(Parent)
  end.

run(Parent, Plt) ->
  %% open projects, standalone
  Dlg = ide_dlg_dialyzer_wx:new(Parent, [{projects, ide_proj_man:get_open_projects()},
                                         {standalone, []}]),
  case wxDialog:showModal(Dlg) of
    ?wxID_CANCEL ->
      ide_dlg_dialyzer_wx:destroy(Dlg);
    ?wxID_OK ->
      Files = ide_dlg_dialyzer_wx:get_files(Dlg),
      ide_dlg_dialyzer_wx:destroy(Dlg),
      run1(Files)
  end.
  
run1(Files) ->
  ide_dialyzer_port:run(self(), [{files, Files}]),
  ok.

init(Parent) ->
  Msg = "Before Dialyzer is ran, the PLT table must be built.\nWould you like to build it now?",
  Dlg = wxMessageDialog:new(Parent, Msg, [{style, ?wxYES_NO}]),
  case wxMessageDialog:showModal(Dlg) of
    ?wxID_YES ->
      wxDialog:destroy(Dlg),
      spawn_link(?MODULE, build_plt, [Parent, wx:get_env()]);
    ?wxID_NO ->
      wxDialog:destroy(Dlg),
      cancelled
  end.
  
build_plt(Parent, WxEnv) ->
  wx:set_env(WxEnv),
  Dlg = build_dlg(Parent),
  wxDialog:show(Dlg),
  %% Disable GUI components here
  ide_dialyzer_port:run(self(), [build_plt]),
  receive
    {_From, ok} ->
      wxDialog:destroy(Dlg);
    {_From, error} ->
      wxDialog:destroy(Dlg)
  end.
  %% Re-enable here

build_dlg(Parent) ->
  Xrc = wxXmlResource:get(),
  Dlg = wxDialog:new(),
  wxXmlResource:loadDialog(Xrc, Dlg, Parent, "build_plt"),
  Gauge = wxXmlResource:xrcctrl(Dlg, "gauge", wxGauge),
  wxGauge:pulse(Gauge),
  Dlg.
  
check_plt() ->
  FileName = "/home/qqq/.dialyzer_plt",
  R = case file:read_file(FileName) of
    {ok, Bin} ->
      try binary_to_term(Bin) of
    	  #file_plt{}=FilePLT -> {ok, FilePLT};
    	  _ -> {error, not_valid}
      catch
      	_:_ -> {error, not_valid}
      end;
    {error, enoent} ->
      {error, no_such_file};
    {error, _} ->
      {error, read_error}
  end.