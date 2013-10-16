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
								image_list,
								project_name_text_ctrl,
								project_path_text_ctrl,
								project_name,
								project_path,
								default_path,
								default_cb,
								default_project_checked,
								desc_panel,
								finish
            	 }).
							 
-define(ID_BROWSE_PROJECTS, 200).
-define(ID_PROJ_PATH, 201).
-define(ID_PROJ_NAME, 202).
-define(ID_DEFAULT_PATH_CB, 203).
-define(ID_DESCRIPTION, 204).

-define(ID_BITMAP_INFO, 0).
-define(ID_BITMAP_STOP, 1).

start(Parent) ->
  wx_object:start({local, ?MODULE}, ?MODULE, Parent, []).

init(Parent) ->
  wx:batch(fun() -> do_init(Parent) end).

do_init(Parent) ->
	Dialog = wxDialog:new(Parent, ?wxID_ANY, "New Project", 
		[{size,{640,460}}, {style, ?wxDEFAULT_DIALOG_STYLE bor ?wxRESIZE_BORDER bor ?wxDIALOG_EX_METAL}]),
		
  Panel = wxPanel:new(Dialog),  
	
	%% For MacOSX  
	wxPanel:setWindowVariant(Panel, ?wxWINDOW_VARIANT_SMALL),
	
	%% For other platforms
	SysFont = wxSystemSettings:getFont(?wxSYS_SYSTEM_FONT),
	wxFont:setPointSize(SysFont, 7),
	wxFont:setUnderlined(SysFont, true),
	wxPanel:setFont(Panel, SysFont),
	
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

  FlexGridSz = wxFlexGridSizer:new(3, 3, 10, 10),
  wxSizer:add(FlexGridSz, wxStaticText:new(Panel, ?wxID_ANY, "Project Name:"), []),
	ProjName = wxTextCtrl:new(Panel, ?ID_PROJ_NAME, []),
  wxSizer:add(FlexGridSz, ProjName, [{proportion, 1}, {flag, ?wxEXPAND}]),
  wxSizer:add(FlexGridSz, 0, 0, []),
   
	Path = wx_misc:getHomeDir(),
  wxSizer:add(FlexGridSz, wxStaticText:new(Panel, ?wxID_ANY, "Project Path:"), []),
	ProjPath = wxTextCtrl:new(Panel, ?ID_PROJ_PATH, [{value, Path}]),
	wxTextCtrl:disable(ProjPath),
  wxSizer:add(FlexGridSz, ProjPath, [{proportion, 1}, {flag, ?wxEXPAND}]),
	Browse = wxButton:new(Panel, ?ID_BROWSE_PROJECTS, [{label, "Browse.."}]),
	wxButton:disable(Browse),
  wxSizer:add(FlexGridSz, Browse, [{proportion, 0}]),
	
  wxSizer:add(FlexGridSz, 0, 0, []),
	DefaultCb = wxCheckBox:new(Panel, ?ID_DEFAULT_PATH_CB, "Use default location"),
	wxCheckBox:setValue(DefaultCb, true),
  wxSizer:add(FlexGridSz, DefaultCb, []),
  wxSizer:add(FlexGridSz, 0, 0, []),
   
  wxFlexGridSizer:addGrowableCol(FlexGridSz, 1),                      
  wxSizer:add(VertSizer, FlexGridSz, [{flag, ?wxEXPAND}, {proportion, 0}]),      
  wxSizer:addSpacer(VertSizer, 20),   
	
  wxSizer:add(VertSizer, wxStaticLine:new(Panel, [{style, ?wxLI_HORIZONTAL}]), 
              [{flag, ?wxEXPAND}]),
  wxSizer:addSpacer(VertSizer, 20),

	wxSizer:add(VertSizer, wxStaticText:new(Panel, ?wxID_ANY, "Description"), []),
	wxSizer:addSpacer(VertSizer, 5),  
	Desc = wxPanel:new(Panel),
	% Bitmap = wxBitmap:new(wxImage:new("../icons/prohibition.png")),
  % StaticBitmap = wxStaticBitmap:new(Desc, ?wxID_ANY, Bitmap),
	% wxStaticText:new(Desc, ?wxID_ANY, "Define a project name"),
	wxPanel:setBackgroundColour(Desc, {255,255,255}),
	insert_desc(Desc, "Create a new project."),
	wxSizer:add(VertSizer, Desc, [{proportion, 1}, {flag, ?wxEXPAND}]),
  wxSizer:addSpacer(VertSizer, 40),
	
  wxSizer:add(VertSizer, wxStaticLine:new(Panel, [{style, ?wxLI_HORIZONTAL}]), 
              [{flag, ?wxEXPAND}]),
	wxSizer:addSpacer(VertSizer, 20),
	ButtonSz = wxBoxSizer:new(?wxHORIZONTAL),
	wxSizer:addStretchSpacer(ButtonSz),
	Finish = wxButton:new(Panel, ?wxID_OK, [{label, "Finish"}]),
	wxButton:disable(Finish),
  wxSizer:add(ButtonSz, Finish, [{proportion, 0}]),
	wxSizer:addSpacer(ButtonSz, 10),  
  wxSizer:add(ButtonSz, wxButton:new(Panel, ?wxID_CANCEL, [{label, "Cancel"}]), [{proportion, 0}]),
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
	wxImageList:add(ImageList, wxBitmap:new(wxImage:new("../icons/information.png"))),
	wxImageList:add(ImageList, wxBitmap:new(wxImage:new("../icons/prohibition.png"))),
	
	State = #state{
		dialog=Dialog, 
		parent=Parent,
		image_list=ImageList,
		project_name_text_ctrl=ProjName,
		project_path_text_ctrl=ProjPath, 
		default_path=Path,
		default_cb=DefaultCb,
		default_project_checked=true,
		desc_panel=Desc,
		finish=Finish
	},
  
	{Dialog, State}.
  
    
%% =====================================================================
%% @doc OTP behaviour callbacks
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
	case lib_dialog_wx:get_dir(Parent) of
		cancelled -> ok;
		Path -> wxTextCtrl:setValue(PathTc, Path)
	end,
	{noreply, State};
handle_event(#wx{event=#wxCommand{type=command_checkbox_clicked, commandInt=0}}, 
             State=#state{parent=Parent, image_list=ImageList, project_name_text_ctrl=Name, project_path_text_ctrl=Path, desc_panel=Desc}) ->
	wxTextCtrl:clear(Path),
	wxWindow:enable(wxWindow:findWindow(Parent, ?ID_BROWSE_PROJECTS)),
	wxWindow:enable(wxWindow:findWindow(Parent, ?ID_PROJ_PATH)),
	Msg = case wxTextCtrl:getValue(Name) of
		[] -> "Please specify a project name and location";
		_ -> "Please specify a project location"
	end,
	insert_desc(Desc, Msg, [{bitmap, wxImageList:getBitmap(ImageList, ?ID_BITMAP_INFO)}]),
  {noreply, State};
handle_event(#wx{event=#wxCommand{type=command_checkbox_clicked, commandInt=1}}, 
             State=#state{parent=Parent, project_path_text_ctrl=Path, default_path=DefPath,
						 							project_name_text_ctrl=Name}) ->
	wxTextCtrl:setValue(Path, DefPath),
	N = wxTextCtrl:getValue(Name),
	case length(N) of
		0 -> ok;
		_ -> wxTextCtrl:appendText(Path, filename:nativename("/") ++ N)
	end,
	wxWindow:disable(wxWindow:findWindow(Parent, ?ID_BROWSE_PROJECTS)),
	wxWindow:disable(wxWindow:findWindow(Parent, ?ID_PROJ_PATH)),
	{noreply, State};
handle_event(#wx{id=?ID_PROJ_NAME, event=#wxCommand{type=command_text_updated, cmdString=Str}}, 
             State=#state{parent=Parent, project_path_text_ctrl=Path, default_path=DefPath, 
						 							default_cb=Cb, project_name_text_ctrl=Name, desc_panel=Desc, finish=Finish})  ->													
	InputName = wxTextCtrl:getValue(Name),
	InputPath = wxTextCtrl:getValue(Path),
	case wxCheckBox:isChecked(Cb) of
		true when length(InputName) =:= 0 ->
			display_message(Desc, "Please specify a project name.", true),
			StartPos = length(DefPath),
			wxTextCtrl:replace(Path, StartPos, -1, Str),
			wxWindow:disable(Finish);
		true ->
			StartPos = length(DefPath),
			wxTextCtrl:replace(Path, StartPos, -1, filename:nativename("/") ++ Str),
			EnableFinish = validate(Str, Desc),
			display_message(Desc, EnableFinish),
			wxButton:enable(Finish, [{enable, EnableFinish}]);
		false when length(InputName) =:= 0 , length(InputName) =:= 0 ->
			Bitmap = wxBitmap:new(wxImage:new("../icons/information.png")),
			insert_desc(Desc, "Please specify a project name and location.", [{bitmap, Bitmap}]),
			wxWindow:disable(Finish);
		false when length(InputName) =:= 0 ->
			wxWindow:disable(Finish);
		false when length(InputPath) =:= 0 ->
			Bitmap = wxBitmap:new(wxImage:new("../icons/information.png")),
			insert_desc(Desc, "Please specify a project location.", [{bitmap, Bitmap}]),
			EnableFinish = validate(Str, Desc),
			wxButton:enable(Finish, [{enable, EnableFinish}]);
		false -> 
			EnableFinish = validate(Str, Desc),
			wxButton:enable(Finish, [{enable, EnableFinish}])
	end,
	{noreply, State};
handle_event(Ev = #wx{}, State = #state{}) ->
  % io:format("Got Event ~p~n",[Ev]),
  {noreply,State}.

%% @doc Display a default message if Display is true.
display_message(Desc, Display) ->
	display_message(Desc, "Create a new project.", Display).
display_message(Desc, Msg, true) ->
	insert_desc(Desc, Msg);
display_message(Desc, Msg, false) -> ok.
	
validate(Str, Desc) ->
	case validate_name(Str) of
		nomatch -> 
			true;
		{match, [{Pos,_}]} -> 
			Bitmap = wxBitmap:new(wxImage:new("../icons/prohibition.png")),
			insert_desc(Desc, "Illegal character \"" ++ [lists:nth(Pos + 1, Str)] ++ "\" in filename.", [{bitmap, Bitmap}]),
			false
	end.
	
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
	wxDialog:destroy(Dialog),
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

validate_name(Str) ->
	re:run(Str, "[/]").
	
name_error(Str, Pos) ->
	io:format("VALIDATION ERROR ON ~c~n", [lists:nth(Pos, Str)]). 
	
insert_desc(Parent, Msg) ->
	insert_desc(Parent, Msg, []).

insert_desc(Parent, Msg, Options) ->
	SzFlags = wxSizerFlags:new([{proportion, 0}]),
	wxSizerFlags:expand(wxSizerFlags:border(SzFlags, ?wxTOP, 10)),
	
	wxPanel:destroyChildren(Parent),
	Sz = wxBoxSizer:new(?wxHORIZONTAL),
	wxPanel:setSizer(Parent, Sz),
	wxSizer:addSpacer(Sz, 10),
	case proplists:get_value(bitmap, Options) of
		undefined -> ok;
		Bitmap -> wxSizer:add(Sz, wxStaticBitmap:new(Parent, ?wxID_ANY, Bitmap), SzFlags)
	end,
	wxSizer:add(Sz, wxStaticText:new(Parent, ?wxID_ANY, Msg), SzFlags),
	wxPanel:layout(Parent),	
	ok.