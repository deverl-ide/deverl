-module(find_replace_dialog).

-include_lib("wx/include/wx.hrl").

-behaviour(wx_object).
-export([start/1, init/1, terminate/2,  code_change/3,
	       handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).

-record(state, {frame,
            	  config
            	 }).

start(Config) ->
  wx_object:start(?MODULE, Config, []).

init(Config) ->
  wx:batch(fun() -> do_init(Config) end).

do_init(Config) ->
  Parent = proplists:get_value(parent, Config),
  Dialog = wxDialog:new(Parent, ?wxID_ANY, "Find and Replace"),
  
  Panel = wxWindow:new(Dialog, ?wxID_ANY),     
  MainSz = wxBoxSizer:new(?wxHORIZONTAL),
  wxWindow:setSizer(Panel, MainSz),
  wxSizer:addSpacer(MainSz, 20),
  
  FlexGridSz = wxFlexGridSizer:new(4, 2, 10, 5),
  wxSizer:add(FlexGridSz, wxStaticText:new(Panel, ?wxID_ANY, "Find:"), [{flag, ?wxALIGN_RIGHT bor ?wxALIGN_CENTRE_VERTICAL}]),
  wxSizer:add(FlexGridSz, wxTextCtrl:new(Panel,?wxID_ANY, []), [{flag, ?wxEXPAND}, {proportion, 1}]),
  
  wxSizer:add(FlexGridSz, wxStaticText:new(Panel, ?wxID_ANY, "Replace:"), [{flag, ?wxALIGN_RIGHT bor ?wxALIGN_CENTRE_VERTICAL}]),
  wxSizer:add(FlexGridSz, wxTextCtrl:new(Panel,?wxID_ANY, []), [{flag, ?wxEXPAND}, {proportion, 1}]),
  
  wxSizer:add(FlexGridSz, wxStaticText:new(Panel, ?wxID_ANY, "Options:"), [{flag, ?wxALIGN_RIGHT bor ?wxALIGN_CENTRE_VERTICAL}]),
  OptionSz = wxGridSizer:new(2,2,10,10),
  wxSizer:add(OptionSz, wxCheckBox:new(Panel, ?wxID_ANY, "Ignore case")),
  wxSizer:add(OptionSz, wxCheckBox:new(Panel, ?wxID_ANY, "Whole word")),
  wxSizer:add(OptionSz, wxCheckBox:new(Panel, ?wxID_ANY, "Start of word")),
  wxSizer:add(OptionSz, wxCheckBox:new(Panel, ?wxID_ANY, "Regex")),
  wxSizer:add(FlexGridSz, OptionSz),
  
  wxSizer:add(FlexGridSz, wxStaticText:new(Panel, ?wxID_ANY, "Search In:"), [{flag, ?wxALIGN_RIGHT bor ?wxALIGN_CENTRE_VERTICAL}]),
  Choices = ["Document", "Project", "Open Documents"],
  wxSizer:add(FlexGridSz, wxChoice:new(Panel,?wxID_ANY, [{choices, Choices}])),
  
  wxSizer:add(MainSz, FlexGridSz, [{border,20}, {flag, ?wxTOP bor ?wxBOTTOM}]),
  wxSizer:addSpacer(MainSz, 20),
  
  ButtonSz = wxBoxSizer:new(?wxVERTICAL),
  wxSizer:add(ButtonSz, wxButton:new(Panel, ?wxID_ANY, [{label,"Find All"}]), [{border,5}, {flag, ?wxEXPAND bor ?wxBOTTOM}]),
  wxSizer:add(ButtonSz, wxButton:new(Panel, ?wxID_ANY, [{label,"Replace All"}]), [{border,5}, {flag, ?wxEXPAND bor ?wxBOTTOM}]),
  wxSizer:addSpacer(ButtonSz, 15),
  wxSizer:add(ButtonSz, wxButton:new(Panel, ?wxID_ANY, [{label,"Replace && Find"}]), [{border,5}, {flag, ?wxEXPAND bor ?wxBOTTOM}]),
  wxSizer:addSpacer(ButtonSz, 15),
  wxSizer:add(ButtonSz, wxButton:new(Panel, ?wxID_ANY, [{label,"Find Previous"}]), [{border,5}, {flag, ?wxEXPAND bor ?wxBOTTOM}]),
  wxSizer:add(ButtonSz, wxButton:new(Panel, ?wxID_ANY, [{label,"Find Next"}]), [{flag, ?wxEXPAND}]),
  
  wxSizer:add(MainSz, ButtonSz, [{border,20}, {flag, ?wxTOP bor ?wxBOTTOM}]),
  wxSizer:addSpacer(MainSz, 20),
  
  wxSizer:layout(MainSz),
	wxSizer:fit(MainSz, Dialog),
	wxSizer:setSizeHints(MainSz, Dialog),
  wxDialog:show(Dialog),
  
  wxPanel:connect(Panel, key_down, []),
  
  {Dialog, #state{frame=Dialog}}.
  
    
%% =====================================================================
%% @doc OTP behaviour callbacks
handle_event(Ev=#wx{obj=Dialog, event=#wxKey{type=key_down, keyCode=27}}, State) ->
  {stop, shutdown, State};
handle_event(Ev = #wx{}, State) ->
  io:format("Got Event ~p~n",[Ev]),
  {noreply,State}.

%% Callbacks handled as normal gen_server callbacks
handle_info(Msg, State) ->
  io:format("Got Info ~p~n",[Msg]),
  {noreply,State}.

handle_call(shutdown, _From, State=#state{frame=Dialog}) ->
  io:format("FIND SHUTDOWN~n"),
  wxWindow:destroy(Dialog),
  {stop, normal, ok, State};

handle_call(Msg, _From, State) ->
  io:format("Got Call ~p~n",[Msg]),
  {reply,ok,State}.

handle_cast(Msg, State) ->
  io:format("Got cast ~p~n",[Msg]),
  {noreply,State}.

code_change(_, _, State) ->
  {stop, ignore, State}.

terminate(_Reason, #state{frame=Dialog}) ->
  io:format("TERMINATE FIND/REPLACE~n"),
  wxDialog:destroy(Dialog).