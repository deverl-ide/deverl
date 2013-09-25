-module(new_file).

-include_lib("wx/include/wx.hrl").

-behaviour(wx_object).

%% wx_objects callbacks
-export([init/1, terminate/2, code_change/3, handle_event/2,
         handle_call/3, handle_cast/2, handle_info/2]).
%% API
-export([start/1]).

-record(state, {win,
                dialog1    :: wxPanel:wxPanel(),
                dialog2    :: wxPanel:wxPanel(),
                swap_sizer :: wxSizer:wxSizer()
               }).

-define(BACK,   9000).
-define(NEXT,   9001).
-define(FINISH, 9002).
-define(BROWSE, 9003).


start(Parent) ->
  wx_object:start_link(?MODULE, Parent, []).

init(Parent) ->
  Dialog = wxDialog:new(Parent, ?wxID_ANY, "New File", [{size,{640, 500}},
                                                        {style, ?wxDEFAULT_DIALOG_STYLE bor
                                                                ?wxRESIZE_BORDER bor
                                                                ?wxDIALOG_EX_METAL}]),
  LRSizer = wxBoxSizer:new(?wxHORIZONTAL),
  wxSizer:addSpacer(LRSizer, 20),
  wxDialog:setSizer(Dialog, LRSizer),

  MainSizer = wxBoxSizer:new(?wxVERTICAL),
  wxSizer:add(LRSizer, MainSizer, [{proportion, 1}, {flag, ?wxEXPAND}]),
  wxSizer:addSpacer(MainSizer, 40),
  wxSizer:add(MainSizer, wxStaticText:new(Dialog, ?wxID_ANY, "New File"), []),
  wxSizer:addSpacer(MainSizer, 5),
  wxSizer:add(MainSizer, wxStaticLine:new(Dialog, [{style, ?wxLI_HORIZONTAL}]), [{flag, ?wxEXPAND}]),
  wxSizer:addSpacer(MainSizer, 20),

  %% Swap dialog
  SwapSizer = wxBoxSizer:new(?wxVERTICAL),
  Dialog1 = dialog1(Dialog),
  wxSizer:add(SwapSizer, Dialog1,   [{proportion, 1}, {flag, ?wxEXPAND}]),
  wxSizer:add(MainSizer, SwapSizer, [{proportion, 1}, {flag, ?wxEXPAND}]),
  wxSizer:addSpacer(MainSizer, 20),

  %% Description Box
  wxSizer:add(MainSizer, wxStaticLine:new(Dialog, [{style, ?wxLI_HORIZONTAL}]), [{flag, ?wxEXPAND}]),
  wxSizer:addSpacer(MainSizer, 20),
  wxSizer:add(MainSizer, wxStaticText:new(Dialog, ?wxID_ANY, "Description"), []),
  wxSizer:addSpacer(MainSizer, 5),
  wxSizer:add(MainSizer, wxTextCtrl:new(Dialog, ?wxID_ANY, [{style, ?wxTE_MULTILINE bor ?wxTE_READONLY}]), [{proportion, 1}, {flag, ?wxEXPAND}]),
  wxSizer:addSpacer(MainSizer, 40),

  %% Buttons
  wxSizer:add(MainSizer, wxStaticLine:new(Dialog, [{style, ?wxLI_HORIZONTAL}]), [{flag, ?wxEXPAND}]),
  wxSizer:addSpacer(MainSizer, 20),
  ButtonPanel = wxPanel:new(Dialog),
  ButtonSizer = wxBoxSizer:new(?wxHORIZONTAL),
  wxPanel:setSizer(ButtonPanel, ButtonSizer),
  wxSizer:add(ButtonSizer, wxButton:new(ButtonPanel, ?BACK,        [{label, "Back"}]),   [{border, 2}, {flag, ?wxALL}]),
  wxSizer:add(ButtonSizer, wxButton:new(ButtonPanel, ?NEXT,        [{label, "Next"}]),   [{border, 2}, {flag, ?wxALL}]),
  wxSizer:add(ButtonSizer, wxButton:new(ButtonPanel, ?FINISH,      [{label, "Finish"}]), [{border, 2}, {flag, ?wxALL}]),
  wxSizer:add(ButtonSizer, wxButton:new(ButtonPanel, ?wxID_CANCEL, [{label, "Cancel"}]), [{border, 2}, {flag, ?wxALL}]),

  wxSizer:add(MainSizer, ButtonPanel, [{proportion, 0}, {flag, ?wxALIGN_RIGHT}]),

  wxButton:disable(wxWindow:findWindow(Parent, ?BACK)),
  %wxButton:disable(wxWindow:findWindow(Parent, ?NEXT)),
  wxButton:disable(wxWindow:findWindow(Parent, ?FINISH)),

  %%%%%%%%

  wxSizer:addSpacer(LRSizer, 20),
  wxSizer:addSpacer(MainSizer, 20),
  wxDialog:connect(ButtonPanel, command_button_clicked, [{skip, true}]),

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
  {stop, normal, ok, State};
handle_call(dialog1, _From, State) ->
  {reply, State#state.dialog1, State};
handle_call(dialog2, _From, State) ->
    {reply, State#state.dialog2, State}.

handle_event(#wx{event = #wxClose{}}, State) ->
  {stop, normal, State};
handle_event(#wx{id=?wxID_CANCEL, event=#wxCommand{type=command_button_clicked}},
             State=#state{win=Dialog}) ->
  wxDialog:destroy(Dialog),
  {stop, normal, State};
handle_event(#wx{id=?NEXT, event=#wxCommand{type=command_button_clicked}},
             State=#state{win=Parent, dialog1=Dialog1, dialog2=undefined, swap_sizer=Sz}) ->
  Dialog2 = dialog2(Parent),
  swap(Sz, Dialog1, Dialog2),
  wxButton:enable(wxWindow:findWindow(Parent, ?BACK)),
  wxButton:disable(wxWindow:findWindow(Parent, ?NEXT)),
  {noreply, State#state{dialog2=Dialog2}};
handle_event(#wx{id=?NEXT, event=#wxCommand{type=command_button_clicked}},
             State=#state{win=Parent, dialog1=Dialog1, dialog2=Dialog2, swap_sizer=Sz}) ->
  swap(Sz, Dialog1, Dialog2),
  wxButton:enable(wxWindow:findWindow(Parent, ?BACK)),
  wxButton:disable(wxWindow:findWindow(Parent, ?NEXT)),
  {noreply, State};
handle_event(#wx{id=?BACK, event=#wxCommand{type=command_button_clicked}},
             State=#state{win=Parent, dialog1=Dialog1, dialog2=Dialog2, swap_sizer=Sz}) ->
  swap(Sz, Dialog2, Dialog1),
  wxButton:enable(wxWindow:findWindow(Parent, ?NEXT)),
  wxButton:disable(wxWindow:findWindow(Parent, ?BACK)),
  {noreply, State};
handle_event(#wx{id=_Id, event=#wxCommand{type=command_button_clicked}},
             State=#state{win=_Parent, swap_sizer=_Sz}) ->
  io:format("CLICKED~n"),
  {noreply, State}.

code_change(_, _, State) ->
  {stop, not_yet_implemented, State}.

terminate(_Reason, #state{win=Dialog}) ->
  io:format("TERMINATE NEW FILE DIALOG~n"),
  wxDialog:destroy(Dialog).


%% =====================================================================
%% @doc Create the first page of the New File dialog.

dialog1(Parent) ->
  Dialog1 = wxPanel:new(Parent),
  DialogSizer1 = wxBoxSizer:new(?wxVERTICAL),
  wxPanel:setSizer(Dialog1, DialogSizer1),

  ProjectSizer = wxBoxSizer:new(?wxHORIZONTAL),
  wxSizer:add(DialogSizer1, ProjectSizer, [{proportion, 1}, {flag, ?wxEXPAND}]),
  wxSizer:add(ProjectSizer, wxStaticText:new(Dialog1, ?wxID_ANY, "Project:"),   []),
  wxSizer:addSpacer(ProjectSizer, 20),
  wxSizer:add(ProjectSizer, wxChoice:new(Dialog1, ?wxID_ANY), [{proportion, 0}, {flag, ?wxEXPAND}]),
  wxSizer:addSpacer(DialogSizer1, 20),

  FileSizer = wxFlexGridSizer:new(2, 2, 10, 10),
  wxSizer:add(DialogSizer1, FileSizer, [{proportion, 1}, {flag, ?wxEXPAND}]),
  wxSizer:add(FileSizer, wxStaticText:new(Dialog1, ?wxID_ANY, "File Type:"), []),
  wxSizer:add(FileSizer, wxStaticText:new(Dialog1, ?wxID_ANY, "Module Type:"), []),
  FileTypeList = wxListBox:new(Dialog1, ?wxID_ANY),
  wxSizer:add(FileSizer, FileTypeList, [{proportion, 1}, {flag, ?wxEXPAND}]),
  wxListBox:insertItems(FileTypeList, ["Erlang", "Plain Text"], 0),
  
  ModuleTypeList = wxListBox:new(Dialog1, ?wxID_ANY),
  wxSizer:add(FileSizer, ModuleTypeList, [{proportion, 1}, {flag, ?wxEXPAND}]),
  wxListBox:insertItems(ModuleTypeList, ["Erlang Module", "OTP Gen Server"], 0),
  
  wxFlexGridSizer:addGrowableCol(FileSizer, 0),
  wxFlexGridSizer:addGrowableRow(FileSizer, 0),
  wxFlexGridSizer:addGrowableCol(FileSizer, 1),
  wxFlexGridSizer:addGrowableRow(FileSizer, 1),

  Dialog1.


%% =====================================================================
%% @doc Create the second page of the New File dialog.

dialog2(Parent) ->
  Dialog2 = wxPanel:new(Parent),
  DialogSizer2 = wxFlexGridSizer:new(4, 3, 20, 20),
  wxPanel:setSizer(Dialog2, DialogSizer2),

  wxSizer:add(DialogSizer2, wxStaticText:new(Dialog2, ?wxID_ANY, "File Name:"),   []),
  wxSizer:add(DialogSizer2, wxTextCtrl:new(Dialog2, ?wxID_ANY), [{proportion, 1}, {flag, ?wxEXPAND}]),
  wxSizer:add(DialogSizer2, 0, 0, []),

  wxSizer:add(DialogSizer2, wxStaticText:new(Dialog2, ?wxID_ANY, "Project:"), []),
  wxSizer:add(DialogSizer2, wxTextCtrl:new(Dialog2, ?wxID_ANY, [{style, ?wxTE_READONLY}]), [{proportion, 1}, {flag, ?wxEXPAND}]),
  wxSizer:add(DialogSizer2, 0, 0, []),

  wxSizer:add(DialogSizer2, wxStaticText:new(Dialog2, ?wxID_ANY, "Folder:"), []),
  wxSizer:add(DialogSizer2, wxTextCtrl:new(Dialog2, ?wxID_ANY, []), [{proportion, 1}, {flag, ?wxEXPAND}]),
  wxSizer:add(DialogSizer2, wxButton:new(Dialog2, ?BROWSE, [{label, "Browse"}])),

  wxSizer:add(DialogSizer2, wxStaticText:new(Dialog2, ?wxID_ANY, "Path:"), []),
  Path = "Path goes here...",
  wxSizer:add(DialogSizer2, wxStaticText:new(Dialog2, ?wxID_ANY, Path), [{proportion, 1}, {flag, ?wxEXPAND}]),
  wxSizer:add(DialogSizer2, 0, 0, []),

  wxFlexGridSizer:addGrowableCol(DialogSizer2, 1),
  wxFlexGridSizer:addGrowableRow(DialogSizer2, 3),

  Dialog2.


%% =====================================================================
%% @doc Swap Dialog1 with Dialog2.

swap(Sizer, Dialog1, Dialog2) ->
  wxSizer:detach(Sizer, 0),
  wxPanel:hide(Dialog1),
  wxSizer:add(Sizer, Dialog2, [{proportion, 1}, {flag, ?wxEXPAND}]),
  wxSizer:show(Sizer, Dialog2),
  wxSizer:layout(Sizer).


