-module(ide_testpane).

-include_lib("wx/include/wx.hrl").
-include("ide.hrl").

-export([
  new/1,
  insert/1
]).

-define(ID_LIST, 1023).



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
  
insert(L) ->
  List = wx:typeCast(wxWindow:findWindowById(?ID_LIST), wxListCtrl),
  Fn = fun({test, _Mod, F}, Acc) ->
    Str = atom_to_list(F),
		wxListCtrl:insertItem(List, Acc, ""),
		wxListCtrl:setItem(List, Acc, 0, Str),
    wxListCtrl:setItemImage(List, Acc, 1),
    wxListCtrl:refreshItem(List, Acc),
    Acc + 1
  end,
  lists:foldl(Fn, 0, L).