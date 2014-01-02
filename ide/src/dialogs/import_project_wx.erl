%% =====================================================================
%% @author
%% @copyright
%% @title
%% @version
%% @doc
%% @end
%% =====================================================================

-module(import_project_wx).

-include_lib("wx/include/wx.hrl").

%% wx_object
-behaviour(wx_object).
-export([init/1, terminate/2, code_change/3,
	       handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).
				 
%% API
-export([start/1, set_focus/1, close/1]).
	
%% Server state			 
-record(state, {dialog,
                parent,
								project_path_text_ctrl,
								project_path,
								copy_cb,
								desc_panel
            	 }).

%% Macros							 
-define(ID_BROWSE_PROJECTS, 200).
-define(ID_PROJ_PATH, 201).
-define(ID_DEFAULT_PATH_CB, 203).
-define(ID_DESCRIPTION, 204).
-define(ID_BITMAP_INFO, 0).
-define(ID_BITMAP_STOP, 1).


%% =====================================================================
%% Client API
%% =====================================================================

start(Parent) ->
  wx_object:start({local, ?MODULE}, ?MODULE, Parent, []).

set_focus(This) ->
	wx_object:cast(This, setfocus).
	
close(This) ->
	wx_object:call(This, shutdown).
	
	
%% =====================================================================
%% Callback functions
%% =====================================================================

init(Parent) ->
  wx:batch(fun() -> do_init(Parent) end).

do_init(Parent) ->
	Dialog = wxDialog:new(Parent, ?wxID_ANY, "Import Project", 
		[{size,{500,370}}, {style, ?wxDEFAULT_DIALOG_STYLE bor ?wxRESIZE_BORDER bor ?wxDIALOG_EX_METAL}]),
	wxDialog:centre(Dialog),
	
	%% Conditional compilation OSX
	case os:type() of
		{_, darwin} ->
			wxPanel:setWindowVariant(Dialog, ?wxWINDOW_VARIANT_SMALL);
		 _ -> ok
	end,
	
  LRSizer = wxBoxSizer:new(?wxHORIZONTAL),
  wxPanel:setSizer(Dialog, LRSizer),
  wxSizer:addSpacer(LRSizer, 20),

  VertSizer = wxBoxSizer:new(?wxVERTICAL),
	%% Header
  wxSizer:addSpacer(VertSizer, 40),
	wxSizer:add(VertSizer, wxStaticText:new(Dialog, ?wxID_ANY, "Import an Erlang Project"), []),
	wxSizer:addSpacer(VertSizer, 5),
  wxSizer:add(VertSizer, wxStaticLine:new(Dialog, [{style, ?wxLI_HORIZONTAL}]), 
              [{flag, ?wxEXPAND}]),
  wxSizer:addSpacer(VertSizer, 20),
  
  FlexGridSz = wxFlexGridSizer:new(3, [{vgap, 10}, {hgap, 10}]),

	%% Erlang project
  wxSizer:add(FlexGridSz, wxStaticText:new(Dialog, ?wxID_ANY, "Erlang Project:"), []),
	ProjPath = wxTextCtrl:new(Dialog, ?ID_PROJ_PATH, []),
  wxSizer:add(FlexGridSz, ProjPath, [{proportion, 1}, {flag, ?wxEXPAND}]),
	Browse = wxButton:new(Dialog, ?ID_BROWSE_PROJECTS, [{label, "Browse.."}]),
  wxSizer:add(FlexGridSz, Browse, [{proportion, 0}]),
	
	%% Copy to erlang project directory checkbox
  wxSizer:add(FlexGridSz, 0, 0, []),
	CopyCb = wxCheckBox:new(Dialog, ?ID_DEFAULT_PATH_CB, "Copy to default project directory"),
	wxCheckBox:setValue(CopyCb, false),
  wxSizer:add(FlexGridSz,CopyCb, []),
  wxSizer:add(FlexGridSz, 0, 0, []),
   
  wxFlexGridSizer:addGrowableCol(FlexGridSz, 1),                      
  wxSizer:add(VertSizer, FlexGridSz, [{flag, ?wxEXPAND}, {proportion, 0}]),      
  wxSizer:addSpacer(VertSizer, 20),   
	
  wxSizer:add(VertSizer, wxStaticLine:new(Dialog, [{style, ?wxLI_HORIZONTAL}]), 
              [{flag, ?wxEXPAND}]),
  wxSizer:addSpacer(VertSizer, 20),

	%% Decscription
	wxSizer:add(VertSizer, wxStaticText:new(Dialog, ?wxID_ANY, "Description"), []),
	wxSizer:addSpacer(VertSizer, 5),  
	Desc = wxPanel:new(Dialog),

	wxPanel:setBackgroundColour(Desc, ?wxWHITE),
	wxPanel:setForegroundColour(Desc, ?wxBLACK),
	insert_desc(Desc, "Import a project."),
	wxSizer:add(VertSizer, Desc, [{proportion, 1}, {flag, ?wxEXPAND}]),
  wxSizer:addSpacer(VertSizer, 40),
	
  wxSizer:add(VertSizer, wxStaticLine:new(Dialog, [{style, ?wxLI_HORIZONTAL}]), 
              [{flag, ?wxEXPAND}]),
	wxSizer:addSpacer(VertSizer, 20),
  
  %% Buttons
  ButtonSz = wxBoxSizer:new(?wxHORIZONTAL),
	wxSizer:addStretchSpacer(ButtonSz),
	Finish = wxButton:new(Dialog, ?wxID_OK, [{label, "Finish"}]),
	wxButton:disable(Finish),
  wxSizer:add(ButtonSz, Finish, [{proportion, 0}]),
	wxSizer:addSpacer(ButtonSz, 10),  
  wxSizer:add(ButtonSz, wxButton:new(Dialog, ?wxID_CANCEL, [{label, "Cancel"}]), [{proportion, 0}]),
	wxSizer:add(VertSizer, ButtonSz, [{flag, ?wxEXPAND}, {proportion, 0}]),   
	wxSizer:addSpacer(VertSizer, 20),     
  
  wxSizer:add(LRSizer, VertSizer, [{proportion, 1}, {flag, ?wxEXPAND}]),
  wxSizer:addSpacer(LRSizer, 20),
	
	wxSizer:layout(LRSizer),
		
  wxDialog:connect(Dialog, close_window),
	wxDialog:connect(Dialog, command_button_clicked, [{skip, true}]), 
  % wxDialog:connect(Dialog, command_checkbox_clicked, []),
  wxDialog:connect(Dialog, command_text_updated, [{skip, false}]),
		
	%% Setup the image list
	ImageList = wxImageList:new(24,24),
	wxImageList:add(ImageList, wxBitmap:new(wxImage:new("../icons/information.png"))),
	wxImageList:add(ImageList, wxBitmap:new(wxImage:new("../icons/prohibition.png"))),
	
	State = #state{
		dialog=Dialog, 
    parent=Parent,
    project_path_text_ctrl=ProjPath,
    copy_cb=CopyCb,
		desc_panel=Desc
	},
  
	{Dialog, State}.
      
%% =====================================================================
%% Event callbacks
%% =====================================================================

handle_event(#wx{event=#wxCommand{type=command_text_updated, cmdString=Str}}, State) ->
  case Str of
    [] ->
      wxWindow:disable(wxWindow:findWindowById(?wxID_OK));
    _ ->
      wxWindow:enable(wxWindow:findWindowById(?wxID_OK))
  end,
  {noreply, State};
handle_event(#wx{id=?ID_BROWSE_PROJECTS, event=#wxCommand{type=command_button_clicked}}, 
             State=#state{parent=Parent, project_path_text_ctrl=PathTc}) ->
	case lib_dialog_wx:get_dir(Parent) of
		cancelled -> ok;
		Path -> 
      wxTextCtrl:setValue(PathTc, Path),
      wxTextCtrl:setInsertionPointEnd(PathTc)
	end,
	{noreply, State};
handle_event(#wx{event=#wxClose{}}, State) ->
  {stop, normal, State};
handle_event(#wx{id=?wxID_CANCEL, event=#wxCommand{type=command_button_clicked}}, 
             State) ->
  {stop, normal, State};
handle_event(#wx{id=?wxID_OK, event=#wxCommand{type=command_button_clicked}}, 
             State=#state{parent=Parent, project_path_text_ctrl=PathTc, copy_cb=Cb}) ->
	Path = wxTextCtrl:getValue(PathTc), 
  case wxCheckBox:isChecked(Cb) of
    true -> %% Copy all files over to project directory
      io:format("NOT IMPLEMENTED"),
      ok;
    false -> %% Leave where it is
      project_manager:new_project(Parent, Path)
  end,
  {stop, normal, State}.
  % {noreply, State#state{project_path=Path}}.

handle_info(Msg, State) ->
  io:format( "Got Info ~p~nMsg:~p",[State, Msg]),
  {noreply,State}.

handle_call(shutdown, _From, State) ->
  {stop, normal, ok, State}.

handle_cast(setfocus, State=#state{project_path_text_ctrl=Tc}) ->
  wxWindow:setFocus(Tc),
  {noreply,State}.

code_change(_, _, State) ->
  {stop, ignore, State}.

terminate(_Reason, #state{dialog=Dialog}) ->
	wxDialog:endModal(Dialog, ?wxID_CANCEL),
	wxDialog:destroy(Dialog).
	

%% =====================================================================
%% Internal functions
%% =====================================================================
	
%% =====================================================================
%% @doc Display information to the user within the 'Description' box.	

insert_desc(Description, Msg) ->
	insert_desc(Description, Msg, []).

insert_desc(Description, Msg, Options) ->
	SzFlags = wxSizerFlags:new([{proportion, 0}]),
	wxSizerFlags:expand(wxSizerFlags:border(SzFlags, ?wxTOP, 10)),
	wxWindow:freeze(Description),
	wxPanel:destroyChildren(Description),
	Sz = wxBoxSizer:new(?wxHORIZONTAL),
	wxPanel:setSizer(Description, Sz),
	wxSizer:addSpacer(Sz, 10),
	case proplists:get_value(bitmap, Options) of
		undefined -> ok;
		Bitmap -> 
			wxSizer:add(Sz, wxStaticBitmap:new(Description, ?wxID_ANY, Bitmap), [{border, 5}, {flag, ?wxTOP bor ?wxRIGHT}])
	end,
	wxSizer:add(Sz, wxStaticText:new(Description, ?wxID_ANY, Msg), SzFlags),
	wxPanel:layout(Description),	
	wxWindow:thaw(Description),
	ok.