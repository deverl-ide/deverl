-module(about).
-export([new/1]).
-export([start/1, init/1, terminate/2, code_change/3, handle_event/2, 
		 handle_call/3, handle_cast/2, handle_info/2]).
-include_lib("wx/include/wx.hrl").

-behaviour(wx_object).

-record(state, {win}).

-define(TABBED_PANE,  7000).
-define(INFO_PANE,    7001).
-define(LICENSE_PANE, 7002).

-define(INFO, "\n\nCool Erlang IDE" ++ 
			  "\n\nAuthors:\n" ++
			  "Tom Richmond (tr201@kent.ac.uk)\n" ++ 
			  "Michael Quested (mdq3@kent.ac.uk)\n").

new(Frame) ->
	start([Frame]).

start(Config) ->
	wx_object:start_link(?MODULE, Config, []).

init(Args) ->
	Parent = proplists:get_value(parent, Args),
	Frame = wxFrame:new(Parent, ?wxID_ANY, "About", [{size,{500, 500}}, 
													 {style, ?wxSTAY_ON_TOP bor
															 ?wxSYSTEM_MENU bor
															 ?wxFRAME_NO_TASKBAR bor
															 ?wxCLOSE_BOX}]),
	Panel = wxPanel:new(Frame),
	MainSizer   = wxBoxSizer:new(?wxVERTICAL),
	wxPanel:setSizer(Panel, MainSizer),

	Banner = wxPanel:new(Panel),
	wxPanel:setBackgroundColour(Banner, ?wxCYAN),
	wxPanel:setSize(Banner, -1, 75),

	TabbedPane  = wxNotebook:new(Panel, ?TABBED_PANE, []),
	InfoPane    = wxTextCtrl:new(TabbedPane, ?INFO_PANE,    [{style, ?wxTE_MULTILINE bor 
																     ?wxTE_CENTRE bor 
																     ?wxTE_READONLY bor
																     ?wxBORDER_NONE}]),
	set_info(InfoPane, ?INFO),
	LicensePane = wxTextCtrl:new(TabbedPane, ?LICENSE_PANE, [{style, ?wxTE_MULTILINE bor 
																	 ?wxTE_CENTRE bor 
																	 ?wxTE_READONLY bor
																	 ?wxBORDER_NONE}]),
	CloseButton = wxButton:new(Panel, ?wxID_EXIT, [{label, "&Close"}]),

	wxNotebook:addPage(TabbedPane, InfoPane, "Info"),
	wxNotebook:addPage(TabbedPane, LicensePane, "License"),

	wxSizer:add(MainSizer, Banner,      [{border, 10}, {proportion, 0}, {flag, ?wxALL bor ?wxEXPAND}]),
	wxSizer:add(MainSizer, TabbedPane,  [{border, 8},  {proportion, 1}, {flag, ?wxALL bor ?wxEXPAND}]),
	wxSizer:add(MainSizer, CloseButton, [{border, 10}, {flag, ?wxALL bor ?wxALIGN_RIGHT}]),

	wxFrame:centerOnParent(Frame),
	wxFrame:show(Frame),

	wxFrame:connect(CloseButton, command_button_clicked),

	State = #state{win = Frame},
	{Frame, State}.

handle_cast(_Msg, State) ->
	io:format("handle_cast/2: ABOUT PANE"),
	{noreply, State}.

handle_info(_Info, State) ->
	io:format("handle_info/2: ABOUT PANE"),
	{noreply, State}.

handle_call(shutdown, _From, State=#state{win=Frame}) ->
    wxFrame:destroy(Frame),
    {stop, normal, ok, State}.

handle_event(#wx{event = #wxClose{}}, State) ->
	{stop, normal, State};
handle_event(#wx{id = ?wxID_EXIT, event = #wxCommand{type = command_button_clicked}}, State=#state{win=Frame}) ->
	wxFrame:destroy(Frame),
	{stop, normal, State}.

code_change(_, _, State) ->
    {stop, not_yet_implemented, State}.

terminate(_Reason, #state{win=Frame}) ->
    wxFrame:destroy(Frame).

set_info(TextBox, Text) ->
	wxTextCtrl:appendText(TextBox, Text).

