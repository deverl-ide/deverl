-module(ide_testpane).

-include_lib("wx/include/wx.hrl").
-include("ide.hrl").

-export([
  new/1,
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

new(Config) ->
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
	wxImageList:add(ImgList, wxBitmap:new(wxImage:new(ide_lib_widgets:rc_dir("test_empty.png")))),
	wxImageList:add(ImgList, wxBitmap:new(wxImage:new(ide_lib_widgets:rc_dir("test_success.png")))),
	wxImageList:add(ImgList, wxBitmap:new(wxImage:new(ide_lib_widgets:rc_dir("test_fail.png")))),
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
  % case wx:is_null(List) of
  %   true -> ok; %% notebook_empty cast sent after test pane is destroyed
  wxListCtrl:deleteAllItems(List),
  % end,
  % io:format("LISTCTRL: ~p~n", [List]),
  % io:format("IS NULL: ~p~n", [wx:is_null(List)])
  ok.


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
