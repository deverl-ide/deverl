%% =====================================================================
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%% 
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%% 
%% You should have received a copy of the GNU General Public License
%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%
%% @author Tom Richmond <tr201@kent.ac.uk>
%% @author Mike Quested <mdq3@kent.ac.uk>
%% @copyright Tom Richmond, Mike Quested 2014
%%
%% @doc A library of functions for managing Dialyzer.
%% @end
%% =====================================================================

-module(deverl_dialyzer).

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
    {error, _E} ->
      init(Parent)
  end.

run(Parent, _Plt) ->

  F = fun(Id) ->
    {Id, deverl_proj_man:get_project_src_files(Id)}
  end,
  W = deverl_proj_man:get_open_projects(),
  ProjsSrc = lists:map(F, deverl_proj_man:get_open_projects()),
  StdlnSrc = deverl_doc_man_wx:get_standalone_src_files(),
  
  Dlg = deverl_dlg_dialyzer_wx:new(Parent, [{projects, ProjsSrc},
                                         {standalone, StdlnSrc}]),
  case wxDialog:showModal(Dlg) of
    ?wxID_CANCEL ->
      deverl_dlg_dialyzer_wx:destroy(Dlg);
    ?wxID_OK ->
      Files = deverl_dlg_dialyzer_wx:get_files(Dlg),
      deverl_dlg_dialyzer_wx:destroy(Dlg),
      run1(Files)
  end.
  
run1(Files) ->
  deverl_dialyzer_port:run(self(), [{files, Files}]),
  ok.

init(Parent) ->
  Dlg = deverl_lib_dlg_wx:message(Parent, 
    [{caption, "Before Dialyzer is ran, the PLT table must be built. Would you like to build it now?"},
     {buttons, [?wxID_CANCEL, ?wxID_OK]}]),
  case wxDialog:showModal(Dlg) of
    ?wxID_OK ->
      wxDialog:destroy(Dlg),
      spawn_link(?MODULE, build_plt, [Parent, wx:get_env()]);
    ?wxID_CANCEL ->
      wxDialog:destroy(Dlg),
      cancelled
  end.
  
build_plt(Parent, WxEnv) ->
  wx:set_env(WxEnv),
  Dlg = build_dlg(Parent),
  wxDialog:show(Dlg),
  %% Disable GUI components here IE COMPILER
  deverl_dialyzer_port:run(self(), [build_plt]),
  receive
    {_From, ok} ->
      wxDialog:destroy(Dlg);
    {_From, {error, _Msg}} ->
      io:format("WWWWWWWW~n"),
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
  %% Get this from the preferences (so it can be changed)
  FileName = filename:join(wx_misc:getHomeDir(), ".dialyzer_plt"),
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
