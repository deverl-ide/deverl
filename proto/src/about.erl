-module(about).
-compile(export_all).
-include_lib("wx/include/wx.hrl").

-behaviour(wx_object).

-export([start/1, init/1, terminate/2,  code_change/3, handle_event/2]).
-record(state, {win}).

-define(TABBED_PANE,  7000).
-define(INFO_PANE,    7001).
-define(CREDITS_PANE, 7002).
-define(LICENSE_PANE, 7003).

new(Frame) ->
	start([Frame]).

start(Config) ->
	wx_object:start_link(?MODULE, Config, []).

init(Args) ->
	Parent = proplists:get_value(parent, Args),
	Frame = wxFrame:new(wx:null(), ?wxID_ANY, "About", [{size,{500, 500}}, 
														{style, ?wxSTAY_ON_TOP bor
																?wxSYSTEM_MENU bor
																?wxFRAME_NO_TASKBAR bor
																?wxCLOSE_BOX}]),
	Panel = wxPanel:new(Frame),
	MainSizer   = wxBoxSizer:new(?wxVERTICAL),
	
	TabbedPane  = wxNotebook:new(Panel, ?TABBED_PANE, []),
	InfoPane    = wxStaticText:new(TabbedPane, ?INFO_PANE, []),
	CreditsPane = wxStaticText:new(TabbedPane, ?CREDITS_PANE, []),
	LicensePane = wxStaticText:new(TabbedPane, ?LICENSE_PANE, []),
	CloseButton = wxButton:new(Panel, ?wxID_EXIT, [{label, "&Close"}]),
	
	wxNotebook:addPage(TabbedPane, InfoPane, "Info"),
	wxNotebook:addPage(TabbedPane, CreditsPane, "Credits"),
	wxNotebook:addPage(TabbedPane, LicensePane, "License"),
	
	wxSizer:add(MainSizer, TabbedPane, [{flag, ?wxEXPAND}, {proportion, 1}]),
	wxSizer:add(MainSizer, CloseButton, [{flag, ?wxALIGN_RIGHT}]),
	
	wxPanel:setSizer(Panel, MainSizer),
	wxFrame:centerOnParent(Frame),
	wxFrame:show(Frame).

terminate(_Reason, _State) ->
    io:format("aui callback: terminate~n"),
    wx:destroy().

code_change(_, _, State) ->
    {stop, not_yet_implemented, State}.

handle_event(_,_) ->
	ok.
