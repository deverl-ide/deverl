-module(new_file).

-include_lib("wx/include/wx.hrl").

-behaviour(wx_object).

%% wx_objects callbacks
-export([start/1, init/1, terminate/2, code_change/3, handle_event/2,
         handle_call/3, handle_cast/2, handle_info/2]).
%% API
-export([new/1]).

-record(state, {win}).

-define(BACK,   9000).
-define(NEXT,   9001).
-define(FINISH, 9002).


new(Frame) ->
	start([Frame]).

start(Config) ->
	wx_object:start_link(?MODULE, Config, []).

init(_Args) ->
	Frame = wxDialog:new(wx:null(), ?wxID_ANY, "New File", [{size,{500, 300}}]),
	Panel = wxPanel:new(Frame),
  LRSizer = wxBoxSizer:new(?wxHORIZONTAL),
  wxPanel:setSizer(Panel, LRSizer),
  wxSizer:addSpacer(LRSizer, 20),
  
	MainSizer = wxBoxSizer:new(?wxVERTICAL),
  wxSizer:addSpacer(MainSizer, 40),
	wxSizer:add(LRSizer, MainSizer, [{proportion, 1}, {flag, ?wxEXPAND}]),

	%%%%%%%%
  
  FlexGridSizer = wxFlexGridSizer:new(2, 2, 10, 10),

  wxSizer:add(FlexGridSizer, wxStaticText:new(Panel, ?wxID_ANY, "Project:"),   []),
  wxSizer:add(FlexGridSizer, wxChoice:new(Panel, ?wxID_ANY), [{proportion, 1}, {flag, ?wxEXPAND}]),

  wxSizer:add(FlexGridSizer, wxStaticText:new(Panel, ?wxID_ANY, "File Type:"), []),
  wxSizer:add(FlexGridSizer, wxListBox:new(Panel, ?wxID_ANY), [{proportion, 1}, {flag, ?wxEXPAND}]),

  wxSizer:add(FlexGridSizer, wxStaticText:new(Panel, ?wxID_ANY, "Description:"), []),
  wxSizer:add(FlexGridSizer, wxTextCtrl:new(Panel, ?wxID_ANY, [{style, ?wxTE_MULTILINE bor ?wxTE_READONLY}]), [{proportion, 1}, {flag, ?wxEXPAND}]),
  wxFlexGridSizer:addGrowableCol(FlexGridSizer, 1),
  wxFlexGridSizer:addGrowableCol(FlexGridSizer, 2),
  
  wxSizer:add(MainSizer, FlexGridSizer, [{proportion, 0}, {flag, ?wxEXPAND}]),

  %%%%%%%%

  ButtonSizer = wxBoxSizer:new(?wxHORIZONTAL),

  wxSizer:add(ButtonSizer, wxButton:new(Panel, ?BACK,      [{label, "Back"}]),   [{border, 2}, {flag, ?wxALL}]),
  wxSizer:add(ButtonSizer, wxButton:new(Panel, ?NEXT,      [{label, "Next"}]),   [{border, 2}, {flag, ?wxALL}]),
  wxSizer:add(ButtonSizer, wxButton:new(Panel, ?FINISH,    [{label, "Finish"}]), [{border, 2}, {flag, ?wxALL}]),
  wxSizer:add(ButtonSizer, wxButton:new(Panel, ?wxID_EXIT, [{label, "Cancel"}]), [{border, 2}, {flag, ?wxALL}]),
  
  wxSizer:add(MainSizer, ButtonSizer, [{proportion, 1}, {flag, ?wxALIGN_RIGHT}]),

  %%%%%%%%

  wxSizer:addSpacer(LRSizer, 20),

	wxDialog:showModal(Frame),

	State = #state{win = Frame},
	{Frame, State}.


%% =====================================================================
%% OTP callbacks
%%
%% =====================================================================

handle_cast(_Msg, State) ->
	io:format("handle_cast/2: NEW FILE DIALOG"),
	{noreply, State}.

handle_info(_Info, State) ->
	io:format("handle_info/2: NEW FILE DIALOG"),
	{noreply, State}.

handle_call(shutdown, _From, State=#state{win=Frame}) ->
  wxFrame:destroy(Frame),
  {stop, normal, ok, State}.

handle_event(#wx{event = #wxClose{}}, State) ->
	{stop, normal, State};
handle_event(#wx{id = ?wxID_EXIT, event = #wxCommand{type = command_button_clicked}}, State=#state{win=Frame}) ->
	wxDialog:destroy(Frame),
	{stop, normal, State}.

code_change(_, _, State) ->
  {stop, not_yet_implemented, State}.

terminate(_Reason, #state{win=Frame}) ->
  wxDialog:destroy(Frame).


dialog2() ->
  ok.
  







