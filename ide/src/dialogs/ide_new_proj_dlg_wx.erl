%% =====================================================================
%% @author
%% @copyright
%% @title
%% @version
%% @doc The new project dialog.
%% @end
%% =====================================================================

-module(ide_new_proj_dlg_wx).

-include_lib("wx/include/wx.hrl").

%% wx_object
-behaviour(wx_object).
-export([init/1, terminate/2, code_change/3,
	       handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).
				 
%% API
-export([start/1, set_focus/1, get_name/1, get_path/1, close/1]).
			
%% inherited functions
-export([show/1, showModal/1]).
	
%% Server state			 
-record(state, {dialog,
            	  parent,
								image_list,
								info_messages,
								project_name_text_ctrl,
								project_path_text_ctrl,
								project_name,
								project_path,
								default_path,
								default_path_cb,
								default_project_checked,
								desc_panel,
								finish
            	 }).

%% Macros							 
-define(ID_BROWSE_PROJECTS, 200).
-define(ID_PROJ_PATH, 201).
-define(ID_PROJ_NAME, 202).
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
	
get_name(This) ->
	wx_object:call(This, name).
	
get_path(This) ->
	wx_object:call(This, path).
	
close(This) ->
	wx_object:call(This, shutdown).
	
	
%% =====================================================================
%% Callback functions
%% =====================================================================

init(Parent) ->
  wx:batch(fun() -> do_init(Parent) end).

do_init(Parent) ->
	Dialog = wxDialog:new(Parent, ?wxID_ANY, "New Project", 
		[{size,{640,460}}, {style, ?wxDEFAULT_DIALOG_STYLE bor ?wxRESIZE_BORDER bor ?wxDIALOG_EX_METAL}]),
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
	wxSizer:add(VertSizer, wxStaticText:new(Dialog, ?wxID_ANY, "New Project"), []),
	wxSizer:addSpacer(VertSizer, 5),
  wxSizer:add(VertSizer, wxStaticLine:new(Dialog, [{style, ?wxLI_HORIZONTAL}]), 
              [{flag, ?wxEXPAND}]),
  wxSizer:addSpacer(VertSizer, 20),
	
	%% Project name
  FlexGridSz = wxFlexGridSizer:new(3, [{vgap, 10}, {hgap, 10}]),
  wxSizer:add(FlexGridSz, wxStaticText:new(Dialog, ?wxID_ANY, "Project Name:"), []),
	ProjName = wxTextCtrl:new(Dialog, ?ID_PROJ_NAME, []),
  wxSizer:add(FlexGridSz, ProjName, [{proportion, 1}, {flag, ?wxEXPAND}]),
  wxSizer:add(FlexGridSz, 0, 0, []),
   
	%% Project path
	Path = ide_sys_pref_gen:get_preference(project_directory),
  wxSizer:add(FlexGridSz, wxStaticText:new(Dialog, ?wxID_ANY, "Project Path:"), []),
	ProjPath = wxTextCtrl:new(Dialog, ?ID_PROJ_PATH, [{value, Path}]),
	wxTextCtrl:disable(ProjPath),
  wxSizer:add(FlexGridSz, ProjPath, [{proportion, 1}, {flag, ?wxEXPAND}]),
	Browse = wxButton:new(Dialog, ?ID_BROWSE_PROJECTS, [{label, "Browse.."}]),
	wxButton:disable(Browse),
  wxSizer:add(FlexGridSz, Browse, [{proportion, 0}]),
	
	%% Project checkbox
  wxSizer:add(FlexGridSz, 0, 0, []),
	DefaultCb = wxCheckBox:new(Dialog, ?ID_DEFAULT_PATH_CB, "Use default location"),
	wxCheckBox:setValue(DefaultCb, true),
  wxSizer:add(FlexGridSz, DefaultCb, []),
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
	insert_desc(Desc, "Create a new project."),
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
	wxDialog:connect(Dialog, command_checkbox_clicked, []),
	wxDialog:connect(Dialog, command_text_updated, [{skip, false}]),
		
	%% Setup the image list
	ImageList = wxImageList:new(24,24),
	wxImageList:add(ImageList, wxBitmap:new(wxImage:new(ide_lib_widgets:rc_dir("information.png")))),
	wxImageList:add(ImageList, wxBitmap:new(wxImage:new(ide_lib_widgets:rc_dir("prohibition.png")))),
	
	%% Information messages
	Info = [
		{name, "Please specify a project name."}, 
		{location, "Please specify a project location."}, 
		{both, "Please specify a project name and location."}
	],
	
	State = #state{
		dialog=Dialog, 
		parent=Parent,
		image_list=ImageList,
		info_messages=Info,
		project_name_text_ctrl=ProjName,
		project_path_text_ctrl=ProjPath,
		default_path=Path,
		default_path_cb=DefaultCb,
		default_project_checked=true,
		desc_panel=Desc,
		finish=Finish
	},
  
	{Dialog, State}.
      
%% =====================================================================
%% Event callbacks
%% =====================================================================

handle_event(#wx{event=#wxClose{}}, State) ->
  {stop, normal, State};
handle_event(#wx{id=?wxID_CANCEL, event=#wxCommand{type=command_button_clicked}}, 
             State) ->
  {stop, normal, State};
handle_event(#wx{id=?wxID_OK, event=#wxCommand{type=command_button_clicked}}, 
             State=#state{project_path_text_ctrl=PathTc, project_name_text_ctrl=NameTc}) ->
	Path = wxTextCtrl:getValue(PathTc), 
	Name = wxTextCtrl:getValue(NameTc), 
  {noreply, State#state{project_name=Name, project_path=Path}};
handle_event(#wx{id=?ID_BROWSE_PROJECTS, event=#wxCommand{type=command_button_clicked}}, 
             State=#state{parent=Parent, project_path_text_ctrl=PathTc}) ->
	case ide_lib_dlg_wx:get_dir(Parent) of
		cancelled -> ok;
		Path -> wxTextCtrl:setValue(PathTc, Path)
	end,
	{noreply, State};
handle_event(#wx{event=#wxCommand{type=command_checkbox_clicked, commandInt=0}}, 
             State=#state{parent=Parent, project_path_text_ctrl=Path, project_path=Input}) ->
	case Input of %% Generates command_text_updated event for ?ID_PROJ_PATH
		undefined -> wxTextCtrl:clear(Path);
		Str -> wxTextCtrl:setValue(Path, Str)
	end,
	wxWindow:enable(wxWindow:findWindow(Parent, ?ID_BROWSE_PROJECTS)),
	wxWindow:enable(wxWindow:findWindow(Parent, ?ID_PROJ_PATH)),
  {noreply, State};
handle_event(#wx{event=#wxCommand{type=command_checkbox_clicked, commandInt=1}}, 
             State=#state{parent=Parent, project_path_text_ctrl=Path, default_path=DefPath,
						 							project_name_text_ctrl=Name}) ->
	Input = wxTextCtrl:getValue(Path),
	wxTextCtrl:setValue(Path, DefPath), %% Generates command_text_updated event for ?ID_PROJ_PATH
	N = wxTextCtrl:getValue(Name),
	case length(N) of
		0 -> ok;
		_ -> wxTextCtrl:appendText(Path, filename:nativename("/") ++ N)
	end,
	wxWindow:disable(wxWindow:findWindow(Parent, ?ID_BROWSE_PROJECTS)),
	wxWindow:disable(wxWindow:findWindow(Parent, ?ID_PROJ_PATH)),
	{noreply, State#state{project_path=Input}};
handle_event(#wx{id=?ID_PROJ_NAME, event=#wxCommand{type=command_text_updated, cmdString=Str}}, 
             State=#state{image_list=ImageList, info_messages=Info, project_path_text_ctrl=Path, default_path=DefPath, 
						 							default_path_cb=Cb, project_name_text_ctrl=Name, desc_panel=Desc, finish=Finish})  ->													
	InputName = wxTextCtrl:getValue(Name),
	InputPath = wxTextCtrl:getValue(Path),
	Valid = case wxCheckBox:isChecked(Cb) of
		true when length(InputName) =:= 0 ->
			insert_desc(Desc, get_message(Info, name), [{bitmap, wxImageList:getBitmap(ImageList, ?ID_BITMAP_INFO)}]),
			StartPos = length(DefPath),
			wxTextCtrl:replace(Path, StartPos, -1, Str),
			false;
		true ->
			StartPos = length(DefPath),
			wxTextCtrl:replace(Path, StartPos, -1, filename:nativename("/") ++ Str),
			validate_name(Str, Desc);
		false when length(InputName) =:= 0, length(InputPath) =:= 0 ->
			insert_desc(Desc, get_message(Info, both), [{bitmap, wxImageList:getBitmap(ImageList, ?ID_BITMAP_INFO)}]),
			false;
		false when length(InputName) =:= 0 ->
			insert_desc(Desc, get_message(Info, name), [{bitmap, wxImageList:getBitmap(ImageList, ?ID_BITMAP_INFO)}]),
			false;	
		false when length(InputPath) =:= 0 ->
			insert_desc(Desc, get_message(Info, location), [{bitmap, wxImageList:getBitmap(ImageList, ?ID_BITMAP_INFO)}]),	
			false;
		false -> 
			validate_name(Str, Desc)
	end,
	wxButton:enable(Finish, [{enable, Valid}]),
	display_message(Desc, Valid),
	{noreply, State};
handle_event(#wx{id=?ID_PROJ_PATH, event=#wxCommand{type=command_text_updated}}, 
             State=#state{image_list=ImageList, info_messages=Info, project_path_text_ctrl=Path,
						 							default_path_cb=Cb, project_name_text_ctrl=Name, desc_panel=Desc, finish=Finish})  ->													
	InputPath = wxTextCtrl:getValue(Path),
	InputName = wxTextCtrl:getValue(Name),
	case wxCheckBox:isChecked(Cb) of
		false when length(InputPath) =:= 0, length(InputName) =:= 0 ->
			insert_desc(Desc, get_message(Info, both), [{bitmap, wxImageList:getBitmap(ImageList, ?ID_BITMAP_INFO)}]);
		false when length(InputPath) =:= 0 -> %%
			insert_desc(Desc, get_message(Info, location), [{bitmap, wxImageList:getBitmap(ImageList, ?ID_BITMAP_INFO)}]),
			wxWindow:disable(Finish);
		_ when length(InputName) =:= 0 ->
			insert_desc(Desc, get_message(Info, name), [{bitmap, wxImageList:getBitmap(ImageList, ?ID_BITMAP_INFO)}]);
		_ ->
			EnableFinish = validate_path(Path, Desc),
			ValidName = validate_name(InputName, Desc),
			Bool = EnableFinish and ValidName,
			display_message(Desc, Bool),
			wxButton:enable(Finish, [{enable, Bool}])
	end,	
	{noreply, State}.
	
handle_info(Msg, State) ->
  io:format( "Got Info ~p~nMsg:~p",[State, Msg]),
  {noreply,State}.

handle_call(name, _From, State=#state{project_name=Name}) ->
  {reply, Name, State};
handle_call(path, _From, State=#state{project_path=Path}) ->
  {reply, Path, State};
handle_call(shutdown, _From, State) ->
  {stop, normal, ok, State}.


handle_cast(setfocus, State=#state{project_name_text_ctrl=Tc}) ->
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
%% @doc Get message (Id) from Messages.	

get_message(Messages, Id) ->
	proplists:get_value(Id, Messages).

	
%% =====================================================================
%% @doc Display the passed message if Display is true.

display_message(Desc, Display) ->
	display_message(Desc, "Create a new project.", [], Display).
display_message(Desc, Msg, Display) ->
	display_message(Desc, Msg, [], Display).
display_message(_, _, _, false) -> ok;
display_message(Desc, Msg, Options, true) -> 
	insert_desc(Desc, Msg, Options).


%% =====================================================================
%% @doc Display information to the user within the 'Description' box.	

insert_desc(Description, Msg) ->
	insert_desc(Description, Msg, []).

insert_desc(Description, Msg, Options) ->
	SzFlags = wxSizerFlags:new([{proportion, 0}]),
	wxSizerFlags:expand(wxSizerFlags:border(SzFlags, ?wxTOP, 10)),
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
	wxPanel:layout(Description).

	
%% =====================================================================
%% @doc Validate the input Path.

validate_path(_Path, _Desc) ->
	% case filelib:is_dir(Path) of 
	% 	true -> true;
	% 	false -> 
	% 		Bitmap = wxBitmap:new(wxImage:new("../icons/prohibition.png")),
	% 		insert_desc(Desc, "Invalid path.", [{bitmap, Bitmap}]),
	% 		false
	% end,
	true. %% path not validated at the moment


%% =====================================================================
%% @doc Validate the input Name.

validate_name([], _) -> false;
validate_name(Str, Desc) ->
	case validate_name(Str) of
		nomatch -> 
			true;
		{match, [{Pos,_}]} -> 
			Bitmap = wxBitmap:new(wxImage:new("../icons/prohibition.png")),
			insert_desc(Desc, "Illegal character \"" ++ [lists:nth(Pos + 1, Str)] ++ "\" in filename.", [{bitmap, Bitmap}]),
			false
	end.
validate_name(Str) ->
	re:run(Str, "[/\]").
	
%% Overidden
%% @hidden
show(This) -> wxDialog:show(This).
%% @hidden
showModal(This) -> wxDialog:showModal(This).