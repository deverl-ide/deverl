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
    {ok, _} ->
      ok;
    {error, _} ->
      init(Parent)
  end.

init(Parent) ->
  Msg = "Before Dialyzer is ran, the PLT table must be built.\nWould you like to build it now?",
  Dlg = wxMessageDialog:new(Parent, Msg, [{style, ?wxYES_NO}]),
  case wxMessageDialog:showModal(Dlg) of
    ?wxID_YES ->
      build_plt(Parent);
    ?wxID_NO ->
      cancelled
  end.
  
build_plt(Parent) ->
  Dlg = build_dlg(Parent),
  wxDialog:show(Dlg),
  ide_dialyzer_port:run(self(), [build_plt]),
  receive
    {_From, ok} ->
      wxDialog:destroy(Dlg);
    {_From, error} ->
      wxDialog:destroy(Dlg)
  end.

build_dlg(Parent) ->
  Xrc = wxXmlResource:get(),
  Dlg = wxDialog:new(),
  wxXmlResource:loadDialog(Xrc, Dlg, Parent, "build_plt"),
  Gauge = wxXmlResource:xrcctrl(Dlg, "gauge", wxGauge),
  wxGauge:pulse(Gauge),
  Dlg.
  
check_plt() ->
  FileName = "/Users/tommo/.dialyzer_plt",
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