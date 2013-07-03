-module(about).
-export([new/1]).
-export([start/1, init/1, terminate/2, code_change/3, 
		 handle_event/2, handle_call/3]).
-include_lib("wx/include/wx.hrl").

-behaviour(wx_object).

-record(state, {win}).

-define(TABBED_PANE,  7000).
-define(INFO_PANE,    7001).
-define(LICENSE_PANE, 7002).

-define(INFO, " ").

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
	  %set_info(InfoPane),
	LicensePane = wxStaticText:new(TabbedPane, ?LICENSE_PANE, []),
	CloseButton = wxButton:new(Panel, ?wxID_EXIT, [{label, "&Close"}]),
	
	wxNotebook:addPage(TabbedPane, InfoPane, "Info"),
	wxNotebook:addPage(TabbedPane, LicensePane, "License"),
	
	wxSizer:add(MainSizer, TabbedPane, [{flag, ?wxEXPAND}, {proportion, 1}]),
	wxSizer:add(MainSizer, CloseButton, [{flag, ?wxALIGN_RIGHT}]),
	
	wxPanel:setSizer(Panel, MainSizer),
	wxFrame:centerOnParent(Frame),
	wxFrame:show(Frame),
	
	wxButton:connect(CloseButton, command_button_clicked),
	
	{state, Frame}.

handle_call(shutdown, _From, State=#state{win=Frame}) ->
    wxFrame:destroy(Frame),
    {stop, normal, ok, State}.

handle_event(#wx{event = #wxClose{}}, State) ->
	io:format("CALL!!!"),
	{stop, normal, State};
handle_event(#wx{id = ?wxID_EXIT, event = #wxCommand{type = command_button_clicked}}, State=#state{win=Frame}) ->
	io:format("CALL!!!"),
	wxFrame:destroy(Frame),
	{stop, normal, State}.

code_change(_, _, State) ->
    {stop, not_yet_implemented, State}.
    
terminate(_Reason, State=#state{win=Frame}) ->
	io:format("TERMINATE!!!"),
    wxFrame:destroy(Frame).
	
set_info(StaticText) ->
	%wxStaticText:setLabel(StaticText, ?INFO).
	ok.
