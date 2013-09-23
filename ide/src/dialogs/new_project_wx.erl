-module(new_project_wx).

-include_lib("wx/include/wx.hrl").
				 
%% API
-export([start/1, get_name/1, get_path/1, close/1]).

-behaviour(wx_object).
-export([init/1, terminate/2, code_change/3,
	       handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).
			
%% inherited exports
-export([show/1,showModal/1]).
				 
-record(state, {dialog,
            	  parent,
								project_name_text_ctrl,
								project_path_text_ctrl,
								project_name,
								project_path
            	 }).
							 
-define(ID_BROWSE_PROJECTS, 200).
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
  wxSizer:add(FlexGridSz, wxButton:new(Panel, ?ID_BROWSE_PROJECTS, [{label, "Browse.."}]), [{proportion, 0}]),
   
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
  wxSizer:add(ButtonSz, wxButton:new(Panel, ?wxID_OK, [{label, "Finish"}]), [{proportion, 0}]),
	wxSizer:addSpacer(ButtonSz, 10),  
  wxSizer:add(ButtonSz, wxButton:new(Panel, ?wxID_CANCEL, [{label, "Cancel"}]), [{proportion, 0}]),
	wxSizer:add(VertSizer, ButtonSz, [{flag, ?wxEXPAND}, {proportion, 0}]),   
	wxSizer:addSpacer(VertSizer, 20),     
  
  wxSizer:add(LRSizer, VertSizer, [{proportion, 1}, {flag, ?wxEXPAND}]),
  wxSizer:addSpacer(LRSizer, 20),
	
	wxSizer:layout(LRSizer),
	% 
	% wxDialog:show(Dialog),
	
  wxDialog:connect(Dialog, close_window),
	wxDialog:connect(Dialog, command_button_clicked, [{skip, true}]), 

  {Dialog, #state{dialog=Dialog, parent=Parent, project_name_text_ctrl=ProjName,
		project_path_text_ctrl=ProjPath}}.
  
    
%% =====================================================================
%% @doc OTP behaviour callbacks
handle_event(#wx{event=#wxClose{}}, State) ->
  {stop, normal, State};
handle_event(#wx{id=?wxID_CANCEL, event=#wxCommand{type=command_button_clicked}}, 
             State) ->
  {stop, normal, State};
handle_event(#wx{id=?wxID_OK, event=#wxCommand{type=command_button_clicked}}, 
             State=#state{project_path_text_ctrl=PathTc, project_name_text_ctrl=NameTc}) ->
	%% Validation				
	
	Path = wxTextCtrl:getValue(PathTc), 
	Name = wxTextCtrl:getValue(NameTc), 
  {noreply, State#state{project_name=Name, project_path=Path}};
handle_event(#wx{id=?ID_BROWSE_PROJECTS, event=#wxCommand{type=command_button_clicked}}, 
             State=#state{parent=Parent, project_path_text_ctrl=PathTc}) ->
	case lib_dialog_wx:get_dir(Parent) of
		cancelled -> ok;
		Path -> wxTextCtrl:setValue(PathTc, Path)
	end,
	{noreply, State};
handle_event(Ev = #wx{}, State = #state{}) ->
  io:format("Got Event ~p~n",[Ev]),
  {noreply,State}.

handle_info(Msg, State) ->
  io:format( "Got Info ~p~nMsg:~p",[State, Msg]),
  {noreply,State}.

handle_call(name, _From, State=#state{project_name=Name}) ->
  {reply, Name, State};
handle_call(path, _From, State=#state{project_path=Path}) ->
  {reply, Path, State};
handle_call(shutdown, _From, State) ->
  {stop, normal, ok, State}.


handle_cast(Msg, State) ->
  io:format("Got cast ~p~n",[Msg]),
  {noreply,State}.

code_change(_, _, State) ->
  {stop, ignore, State}.

terminate(_Reason, #state{dialog=Dialog}) ->
  io:format("TERMINATE NEW DIALOG~n"),
	wxDialog:endModal(Dialog, ?wxID_CANCEL),
	ok.
	
get_name(This) ->
	wx_object:call(This, name).
	
get_path(This) ->
	wx_object:call(This, path).
	
close(This) ->
	wx_object:call(This, shutdown).
	
%% @hidden
show(This) -> wxDialog:show(This).
%% @hidden
showModal(This) -> wxDialog:showModal(This).