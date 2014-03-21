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
%% @doc Manages the Eunit testing display.
%% @end
%% =====================================================================

-module(deverl_testpane).

-include_lib("wx/include/wx.hrl").
-include("deverl.hrl").

-export([
  start/1,
  add_module_tests/1,
  show_test_results/2,
  clear/0
  ]).

%% The test list.
-define(ID_LIST, 1023).

%% The test indicator icons
-define(TEST_UNTESTED, 0).
-define(TEST_SUCCESS,  1).
-define(TEST_FAIL,     2).


%% =====================================================================
%% Client API
%% =====================================================================

%% =====================================================================
%% @doc

start(Config) ->
  Parent = proplists:get_value(parent, Config),
  Panel = wxPanel:new(Parent),
  List = wxListCtrl:new(Panel, [{winid, ?ID_LIST}, {style, ?wxLC_REPORT bor ?wxLC_NO_HEADER bor ?wxLC_SINGLE_SEL bor ?wxBORDER_NONE}]),
  wxListCtrl:insertColumn(List, 0, "Heading", []),
  wxListCtrl:setColumnWidth(List, 0, ?wxLIST_AUTOSIZE),

  Resize = fun(_EvtRec, WxEvt) ->
    {W,_H} = wxSizeEvent:getSize(WxEvt),
    wxListCtrl:setColumnWidth(List, 0, W),
    wxEvent:skip(WxEvt)
  end,
  wxListCtrl:connect(List, size, [{callback, Resize}]),

  %% Images
	ImgList = wxImageList:new(20,20),
	wxImageList:add(ImgList, wxBitmap:new(wxImage:new(deverl_lib_widgets:rc_dir("test_empty.png")))),
	wxImageList:add(ImgList, wxBitmap:new(wxImage:new(deverl_lib_widgets:rc_dir("test_success.png")))),
	wxImageList:add(ImgList, wxBitmap:new(wxImage:new(deverl_lib_widgets:rc_dir("test_fail.png")))),
  wxListCtrl:assignImageList(List, ImgList, ?wxIMAGE_LIST_SMALL),

  Sz = wxBoxSizer:new(?wxVERTICAL),
  wxSizer:add(Sz, List, [{flag, ?wxEXPAND}, {proportion, 1}]),
  wxPanel:setSizer(Panel, Sz),
  Panel.


%% =====================================================================
%% @doc

add_module_tests(Module) ->
  code:delete(Module),
  code:purge(Module),
  List = wx:typeCast(wxWindow:findWindowById(?ID_LIST), wxListCtrl),
  wxListCtrl:deleteAllItems(List),
  try
    Tests = lists:sort(eunit_data:get_module_tests(Module)),
    insert(List, Tests),
    case length(Tests) of
      0 ->
        wxListCtrl:insertItem(List, 0, " No tests to run.");
      _ ->
        ok
    end
  catch
    throw:_E ->
      wxListCtrl:insertItem(List, 0, " No tests to run.")
  end.


%% =====================================================================
%% @doc

clear() ->  
  List = wx:typeCast(wxWindow:findWindowById(?ID_LIST), wxListCtrl),
  wxListCtrl:deleteAllItems(List).


%% =====================================================================
%% @doc Set the test indicator icon to signify test pass or failure.

-spec show_test_results(Results, wx:wx_env()) -> ok when
  Results :: list({atom(), boolean()}).

show_test_results(Results, Env) ->
  wx:set_env(Env),
  ListCtrl = wx:typeCast(wxWindow:findWindowById(?ID_LIST), wxListCtrl),
  show_test_results(Results, ListCtrl, waa).

show_test_results([], _ListCtrl, waa) ->
  ok;
show_test_results([{FunctionName, Result}|Results], ListCtrl, waa) ->
  Item = wxListCtrl:findItem(ListCtrl, -1, atom_to_list(FunctionName)),
  case Result of
    true ->
      wxListCtrl:setItemImage(ListCtrl, Item, ?TEST_SUCCESS);
    false ->
      wxListCtrl:setItemImage(ListCtrl, Item, ?TEST_FAIL)
  end,
  show_test_results(Results, ListCtrl, waa).


%% =====================================================================
%% Internal functions
%% =====================================================================

%% =====================================================================
%% @doc

insert(ListCtrl, L) ->
  Fn = fun({test, _Mod, F}, Acc) ->
    Str = atom_to_list(F),
		wxListCtrl:insertItem(ListCtrl, Acc, ""),
		wxListCtrl:setItem(ListCtrl, Acc, 0, Str),
    wxListCtrl:setItemImage(ListCtrl, Acc, ?TEST_UNTESTED),
    wxListCtrl:refreshItem(ListCtrl, Acc),
    Acc + 1
  end,
  lists:foldl(Fn, 0, L).
