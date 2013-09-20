-module(new_project_wx).

-include_lib("wx/include/wx.hrl").

-behaviour(wx_object).
-export([init/1, terminate/2,  code_change/3,
	       handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).
				 
%% API
-export([start/1]).
				 
-record(state, {dialog,
            	  parent,
								project_name,
								project_path
            	 }).
							 
-define(ID_FILE_CHOOSER, 200).
-define(ID_PROJ_PATH, 201).
-define(ID_PROJ_NAME, 202).

start(Parent) ->
  wx_object:start({local, ?MODULE}, ?MODULE, Parent, []).

init(Parent) ->
  wx:batch(fun() -> do_init(Parent) end).

do_init(Parent) ->
	Dialog = wxDialog:new(Parent, ?wxID_ANY, "New Project", 
		[{size,{640,460}}, {style, ?wxDEFAULT_DIALOG_STYLE bor ?wxRESIZE_BORDER bor ?wxDIALOG_EX_METAL}]),
  Panel = wxPanel:new(Dialog),    
  LRSizer = wxBoxSizer:new(?wxHORIZONTAL),
  wxPanel:setSizer(Panel, LRSizer),
  wxSizer:addSpacer(LRSizer, 20),
  
  VertSizer = wxBoxSizer:new(?wxVERTICAL),
  wxSizer:addSpacer(VertSizer, 40),
	wxSizer:add(VertSizer, wxStaticText:new(Panel, ?wxID_ANY, "New Project"), []),
	wxSizer:addSpacer(VertSizer, 5),
  wxSizer:add(VertSizer, wxStaticLine:new(Panel, [{style, ?wxLI_HORIZONTAL}]), 
              [{flag, ?wxEXPAND}]),
  wxSizer:addSpacer(VertSizer, 20),

  FlexGridSz = wxFlexGridSizer:new(2, 3, 10, 10),
  wxSizer:add(FlexGridSz, wxStaticText:new(Panel, ?wxID_ANY, "Project Name:"), []),
	ProjName = wxTextCtrl:new(Panel, ?ID_PROJ_PATH, []),
  wxSizer:add(FlexGridSz, ProjName, [{proportion, 1}, {flag, ?wxEXPAND}]),
  wxSizer:add(FlexGridSz, 0, 0, []),
   
  wxSizer:add(FlexGridSz, wxStaticText:new(Panel, ?wxID_ANY, "Project Path:"), []),
	ProjPath = wxTextCtrl:new(Panel, ?ID_PROJ_NAME, []),
  wxSizer:add(FlexGridSz, ProjPath, [{proportion, 1}, {flag, ?wxEXPAND}]),
  wxSizer:add(FlexGridSz, wxButton:new(Panel, ?ID_FILE_CHOOSER, [{label, "Browse.."}]), [{proportion, 0}]),
   
  wxFlexGridSizer:addGrowableCol(FlexGridSz, 1),                      
  wxSizer:add(VertSizer, FlexGridSz, [{flag, ?wxEXPAND}, {proportion, 0}]),      
  wxSizer:addSpacer(VertSizer, 20),   
	
  wxSizer:add(VertSizer, wxStaticLine:new(Panel, [{style, ?wxLI_HORIZONTAL}]), 
              [{flag, ?wxEXPAND}]),
  wxSizer:addSpacer(VertSizer, 20),

	wxSizer:add(VertSizer, wxStaticText:new(Panel, ?wxID_ANY, "Description"), []),
	wxSizer:addSpacer(VertSizer, 5),  
	wxSizer:add(VertSizer, wxTextCtrl:new(Panel, ?wxID_ANY, []), [{proportion, 1}, {flag, ?wxEXPAND}]),
  wxSizer:addSpacer(VertSizer, 40),
	
  wxSizer:add(VertSizer, wxStaticLine:new(Panel, [{style, ?wxLI_HORIZONTAL}]), 
              [{flag, ?wxEXPAND}]),
	wxSizer:addSpacer(VertSizer, 20),
	ButtonSz = wxBoxSizer:new(?wxHORIZONTAL),
	wxSizer:addStretchSpacer(ButtonSz),
  wxSizer:add(ButtonSz, wxButton:new(Panel, ?wxID_ANY, [{label, "Finish"}]), [{proportion, 0}]),
	wxSizer:addSpacer(ButtonSz, 10),  
  wxSizer:add(ButtonSz, wxButton:new(Panel, ?wxID_CANCEL, [{label, "Cancel"}]), [{proportion, 0}]),
	wxSizer:add(VertSizer, ButtonSz, [{flag, ?wxEXPAND}, {proportion, 0}]),   
	wxSizer:addSpacer(VertSizer, 20),     
  
  wxSizer:add(LRSizer, VertSizer, [{proportion, 1}, {flag, ?wxEXPAND}]),
  wxSizer:addSpacer(LRSizer, 20),
	
	% wxSizer:setSizeHints(LRSizer, Dialog),
	wxSizer:layout(LRSizer),
	
	wxDialog:show(Dialog),
	
  wxDialog:connect(Dialog, close_window),
	wxDialog:connect(Dialog, command_button_clicked, [{skip, true}]), 

  {Panel, #state{dialog=Dialog, parent=Parent, project_name=ProjName,
		project_path=ProjPath}}.
  
    
%% =====================================================================
%% @doc OTP behaviour callbacks
handle_event(#wx{event=#wxClose{}}, State) ->
  {stop, normal, State};
handle_event(#wx{id=?wxID_CANCEL, event=#wxCommand{type=command_button_clicked}}, 
             State) ->
  {stop, normal, State};
handle_event(#wx{id=?ID_FILE_CHOOSER, event=#wxCommand{type=command_button_clicked}}, 
             State=#state{parent=Parent, project_path=PathTc}) ->
	 Dialog = wxDirDialog:new(Parent),
	 wxDirDialog:showModal(Dialog),
	 wxTextCtrl:setValue(PathTc, wxDirDialog:getPath(Dialog)),
	{noreply, State};
handle_event(Ev = #wx{}, State = #state{}) ->
  io:format("Got Event ~p~n",[Ev]),
  {noreply,State}.

handle_info(Msg, State) ->
  io:format( "Got Info ~p~nMsg:~p",[State, Msg]),
  {noreply,State}.

handle_call(Msg, _From, State) ->
  io:format("Got Call ~p~n",[Msg]),
  {reply,ok,State}.

handle_cast(Msg, State) ->
  io:format("Got cast ~p~n",[Msg]),
  {noreply,State}.

code_change(_, _, State) ->
  {stop, ignore, State}.

terminate(_Reason, #state{dialog=Dialog}) ->
  io:format("TERMINATE NEW DIALOG~n"),
	wxDialog:destroy(Dialog),
	ok.