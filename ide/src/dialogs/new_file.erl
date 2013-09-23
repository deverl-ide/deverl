-module(new_file).

-include_lib("wx/include/wx.hrl").

-behaviour(wx_object).

%% wx_objects callbacks
-export([init/1, terminate/2, code_change/3, handle_event/2,
         handle_call/3, handle_cast/2, handle_info/2]).
%% API
-export([start/1]).

-record(state, {win,
                dialog1 :: wxPanel:wxPanel(),
                dialog2 :: wxPanel:wxPanel(),
								swap_sizer :: wxSizer:wxSizer()
               }).

-define(BACK,   9000).
-define(NEXT,   9001).
-define(FINISH, 9002).
-define(BROWSE, 9003).


start(Parent) ->
	wx_object:start_link(?MODULE, Parent, []).

init(Parent) ->
	Dialog = wxDialog:new(Parent, ?wxID_ANY, "New File", [{size,{500, 500}}]),
  LRSizer = wxBoxSizer:new(?wxHORIZONTAL),
  wxPanel:setSizer(Dialog, LRSizer),
  
	wxSizer:addSpacer(LRSizer, 20),
	MainSizer = wxBoxSizer:new(?wxVERTICAL),
	wxSizer:addSpacer(MainSizer, 20),
	
	SwapSizer = wxBoxSizer:new(?wxVERTICAL),
	Dialog1 = dialog1(Dialog),
  wxSizer:add(SwapSizer, Dialog1, [{proportion, 0}, {flag, ?wxEXPAND}]),		
	wxSizer:add(MainSizer, SwapSizer, [{proportion, 1}, {flag, ?wxEXPAND}]),	
	
	wxSizer:add(LRSizer, MainSizer, [{proportion, 1}, {flag, ?wxEXPAND}]),

  %%%%%%%%
	ButtonPanel = wxPanel:new(Dialog),
  ButtonSizer = wxBoxSizer:new(?wxHORIZONTAL),
	wxPanel:setSizer(ButtonPanel, ButtonSizer),
  wxSizer:add(ButtonSizer, wxButton:new(ButtonPanel, ?BACK,      [{label, "Back"}]),   [{border, 2}, {flag, ?wxALL}]),
  wxSizer:add(ButtonSizer, wxButton:new(ButtonPanel, ?NEXT,      [{label, "Next"}]),   [{border, 2}, {flag, ?wxALL}]),
  wxSizer:add(ButtonSizer, wxButton:new(ButtonPanel, ?FINISH,    [{label, "Finish"}]), [{border, 2}, {flag, ?wxALL}]),
  wxSizer:add(ButtonSizer, wxButton:new(ButtonPanel, ?wxID_CANCEL, [{label, "Cancel"}]), [{border, 2}, {flag, ?wxALL}]),
  
  wxSizer:add(MainSizer, ButtonPanel, [{proportion, 1}, {flag, ?wxALIGN_RIGHT}]),

  %%%%%%%%

  wxSizer:addSpacer(LRSizer, 20),
	wxDialog:connect(ButtonPanel, command_button_clicked, [{skip, true}]), 

	wxDialog:show(Dialog),

	{Dialog, #state{win=Dialog, dialog1=Dialog1, swap_sizer=SwapSizer}}.


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

handle_call(shutdown, _From, State=#state{win=Dialog}) ->
  wxDialog:destroy(Dialog),
  {stop, normal, ok, State}.

handle_event(#wx{event = #wxClose{}}, State) ->
	{stop, normal, State};
handle_event(#wx{id=?wxID_CANCEL, event=#wxCommand{type=command_button_clicked}},
	 					 State=#state{win=Dialog}) ->
	wxDialog:destroy(Dialog),
	{stop, normal, State};
handle_event(#wx{id=?NEXT, event=#wxCommand{type=command_button_clicked}}, 
             State=#state{win=Parent, dialog1=Dialog1, swap_sizer=Sz}) ->
	C = wxSizer:getChildren(Sz),
	io:format("Children: ~p~n", [C]),
	Item = wxSizer:getItem(Sz, Dialog1),
	io:format("Item: ~p~n", [Item]),
	wxSizer:detach(Sz, Item),
	io:format("Children: ~p~n", [wxSizer:getChildren(Sz)]),
	wxSizer:clear(Sz),
	wxSizer:insert(Sz, 0, dialog2(Parent), []),
	wxSizer:layout(Sz),
	{noreply, State};
handle_event(#wx{id=Id, event=#wxCommand{type=command_button_clicked}}, 
             State=#state{win=Parent, swap_sizer=Sz}) ->
	io:format("CLICKED~n"),
	{noreply, State}.
		
code_change(_, _, State) ->
  {stop, not_yet_implemented, State}.

terminate(_Reason, #state{win=Dialog}) ->
  io:format("TERMINATE NEW FILE DIALOG~n"),
  wxDialog:destroy(Dialog).


%% =====================================================================
%% @doc 

<<<<<<< HEAD
dialog1(Panel) ->
=======
dialog1(Parent) -> 
	Dialog1 = wxWindow:new(Parent, 555),
>>>>>>> aa117dacb8a334c919224c8d01994b6283a8c573
  DialogSizer1 = wxFlexGridSizer:new(2, 2, 10, 10),
	wxPanel:setSizer(Dialog1, DialogSizer1),

  wxSizer:add(DialogSizer1, wxStaticText:new(Dialog1, ?wxID_ANY, "Project:"),   []),
  wxSizer:add(DialogSizer1, wxChoice:new(Dialog1, ?wxID_ANY), [{proportion, 1}, {flag, ?wxEXPAND}]),

  wxSizer:add(DialogSizer1, wxStaticText:new(Dialog1, ?wxID_ANY, "File Type:"), []),
  wxSizer:add(DialogSizer1, wxListBox:new(Dialog1, ?wxID_ANY), [{proportion, 1}, {flag, ?wxEXPAND}]),

  wxSizer:add(DialogSizer1, wxStaticText:new(Dialog1, ?wxID_ANY, "Description:"), []),
  wxSizer:add(DialogSizer1, wxTextCtrl:new(Dialog1, ?wxID_ANY, [{style, ?wxTE_MULTILINE bor ?wxTE_READONLY}]), [{proportion, 1}, {flag, ?wxEXPAND}]),
  wxFlexGridSizer:addGrowableCol(DialogSizer1, 1),
  wxFlexGridSizer:addGrowableCol(DialogSizer1, 2),
  
  Dialog1.


%% =====================================================================
%% @doc 

dialog2(Parent) ->
	Dialog2 = wxWindow:new(Parent, 666),
  DialogSizer2 = wxFlexGridSizer:new(2, 3, 10, 10),

  wxSizer:add(DialogSizer2, wxStaticText:new(Dialog2, ?wxID_ANY, "File Name:"),   []),
  wxSizer:add(DialogSizer2, wxTextCtrl:new(Dialog2, ?wxID_ANY), [{proportion, 1}, {flag, ?wxEXPAND}]),
  wxSizer:add(DialogSizer2, 0, 0, []),

  wxSizer:add(DialogSizer2, wxStaticText:new(Dialog2, ?wxID_ANY, "Project:"), []),
  wxSizer:add(DialogSizer2, wxTextCtrl:new(Dialog2, ?wxID_ANY, [{style, ?wxTE_READONLY}]), [{proportion, 1}, {flag, ?wxEXPAND}]),
  wxSizer:add(DialogSizer2, 0, 0, []),

  wxSizer:add(DialogSizer2, wxStaticText:new(Dialog2, ?wxID_ANY, "Folder:"), []),
  wxSizer:add(DialogSizer2, wxTextCtrl:new(Dialog2, ?wxID_ANY, [{style, ?wxTE_READONLY}]), [{proportion, 1}, {flag, ?wxEXPAND}]),
  wxSizer:add(DialogSizer2, wxButton:new(Dialog2, ?BROWSE, [{label, "Browse"}])),
 
  wxSizer:add(DialogSizer2, wxStaticText:new(Dialog2, ?wxID_ANY, "Path:"), []),
  Path = "Path goes here...",
  wxSizer:add(DialogSizer2, wxStaticText:new(Dialog2, ?wxID_ANY, Path), [{proportion, 1}, {flag, ?wxEXPAND}]),
  wxSizer:add(DialogSizer2, 0, 0, []),
  wxSizer:addSpacer(DialogSizer2, 10),
 
  wxFlexGridSizer:addGrowableCol(DialogSizer2, 1),
  wxFlexGridSizer:addGrowableCol(DialogSizer2, 2),
  wxFlexGridSizer:addGrowableCol(DialogSizer2, 3),
  
  Dialog2.
  
  
