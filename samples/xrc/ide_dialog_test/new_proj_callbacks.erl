%% =====================================================================
%% @author
%% @copyright
%% @title
%% @version
%% @doc This module is testing the use of xrc using callbacks only.
%% @end 
%% =====================================================================

-module(new_proj_callbacks).

-include_lib("wx/include/wx.hrl").

-compile(export_all).

%% Server state
-record(state, {frame,
                dlg,
                name_input,
                path_input,
                name_tc,
                path_tc
            	 }).

%% Macros							 
-define(ID_BROWSE, 200).

%% =====================================================================
%% Client API
%% =====================================================================

start(Parent, Env) ->
  do_init(Parent, Env).

%% =====================================================================
%% 
%% =====================================================================

do_init(Parent, Env) ->
  wx:set_env(Env),
  Xrc = wxXmlResource:get(),
  Dlg = wxDialog:new(),
  dlg_ld:win_var(Dlg),

  true = wxXmlResource:loadDialog(Xrc, Dlg, Parent, "new_project"),
  
  Path = ide_sys_pref_gen:get_preference(project_directory),
  PathTc = wxXmlResource:xrcctrl(Dlg, "path_tc", wxTextCtrl),
  NameTc = wxXmlResource:xrcctrl(Dlg, "name_tc", wxTextCtrl),
  wxTextCtrl:setValue(PathTc, Path),
  
  %% checkbox callback
  OnMyCheckBox = fun(_EvRec, _Event) ->
    CheckB = wxXmlResource:xrcctrl(Dlg, "checkbox", wxCheckBox),
    Bool = wxCheckBox:isChecked(CheckB),
    wxTextCtrl:enable(PathTc, [{enable, not Bool}])
  end,
  wxDialog:connect(Dlg,update_ui,[{id,wxXmlResource:getXRCID("checkbox")},
            {callback,OnMyCheckBox}]),
              
  %% Keep updateUI event interval at 250ms
  wxUpdateUIEvent:setUpdateInterval(250),

  %% browse for directory
  Browse = fun(_EvRec, _Event) ->
    case ide_lib_dlg_wx:get_dir(Parent) of
      cancelled -> ok;
      Path1 -> wxTextCtrl:setValue(PathTc, Path1)
    end
  end,
  wxDialog:connect(Dlg, command_button_clicked, 
	    [{id,wxXmlResource:getXRCID("browse_btn")}, {callback, Browse}]),
        
  State=#state{frame=Parent,
               dlg=Dlg,
               name_tc=NameTc,
               path_tc=PathTc},
               
  wxDialog:connect(PathTc, command_text_updated, [{callback, fun(E,O) -> path_cmd(E,O,State) end}]),
  wxDialog:connect(NameTc, command_text_updated, [{callback, fun(E,O) -> name_cmd(E,O,State) end}]),

  wxDialog:showModal(Dlg),
  ok.
  
path_cmd(EvtRef,_wxEvent,State=#state{dlg=Dlg, name_tc=Tc0, path_tc=Tc1}) ->
	Name = wxTextCtrl:getValue(Tc0),
	Path = wxTextCtrl:getValue(Tc1),
	case wxCheckBox:isChecked(wxXmlResource:xrcctrl(Dlg, "checkbox", wxCheckBox)) of
		false when length(Path) =:= 0, length(Name) =:= 0 ->
      % insert_desc(Desc, get_message(Info, both), [{bitmap, wxImageList:getBitmap(ImageList, ?ID_BITMAP_INFO)}]);
      ok;
		false when length(Path) =:= 0 -> %%
      % insert_desc(Desc, get_message(Info, location), [{bitmap, wxImageList:getBitmap(ImageList, ?ID_BITMAP_INFO)}]),
			wxButton:disable(wxXmlResource:xrcctrl(Dlg, "wxID_OK", wxButton));
		_ when length(Name) =:= 0 ->
      % insert_desc(Desc, get_message(Info, name), [{bitmap, wxImageList:getBitmap(ImageList, ?ID_BITMAP_INFO)}]);
      ok;
		_ ->
      % EnableFinish = validate_path(Path, Desc),
      % ValidName = validate_name(InputName, Desc),
			Bool = validate_path(Path, desc) and validate_name(Name, desc),
      % display_message(Desc, Bool),
			wxButton:enable(wxXmlResource:xrcctrl(Dlg, "wxID_OK", wxButton), [{enable, Bool}])
	end.
  
name_cmd(EvtRef,EvtObj,State=#state{dlg=Dlg, name_tc=Tc0, path_tc=Tc1}) ->
	Name = wxTextCtrl:getValue(Tc0),
	Path = wxTextCtrl:getValue(Tc1),
  Cb = wxXmlResource:xrcctrl(Dlg, "checkbox", wxCheckBox),
	Valid = case wxCheckBox:isChecked(Cb) of
		true when length(Name) =:= 0 ->
      % insert_desc(Desc, get_message(Info, name), [{bitmap, wxImageList:getBitmap(ImageList, ?ID_BITMAP_INFO)}]),
			StartPos = length(ide_sys_pref_gen:get_preference(project_directory)),
			wxTextCtrl:replace(Tc1, StartPos, -1, wxCommandEvent:getString(EvtObj)),
			false;
		true ->
			StartPos = length(ide_sys_pref_gen:get_preference(project_directory)),
			wxTextCtrl:replace(Tc1, StartPos, -1, filename:nativename("/") ++ wxCommandEvent:getString(EvtObj)),
      validate_name(wxCommandEvent:getString(EvtObj), desc);
		false when length(Name) =:= 0, length(Path) =:= 0 ->
      % insert_desc(Desc, get_message(Info, both), [{bitmap, wxImageList:getBitmap(ImageList, ?ID_BITMAP_INFO)}]),
			false;
		false when length(Name) =:= 0 ->
      % insert_desc(Desc, get_message(Info, name), [{bitmap, wxImageList:getBitmap(ImageList, ?ID_BITMAP_INFO)}]),
			false;	
		false when length(Path) =:= 0 ->
      % insert_desc(Desc, get_message(Info, location), [{bitmap, wxImageList:getBitmap(ImageList, ?ID_BITMAP_INFO)}]),  
			false;
		false -> 
      validate_name(wxCommandEvent:getString(EvtObj), desc)
	end,
	wxButton:enable(wxXmlResource:xrcctrl(Dlg, "wxID_OK", wxButton), [{enable, Valid}]),
  % display_message(Desc, Valid),
  ok.
  
  
%% =====================================================================
%% @doc Validate the input Name.

validate_name([], _) -> false;
validate_name(Str, Desc) ->
	case validate_name(Str) of
		nomatch -> 
			true;
		{match, [{Pos,_}]} -> 
      % Bitmap = wxBitmap:new(wxImage:new("../icons/prohibition.png")),
      % insert_desc(Desc, "Illegal character \"" ++ [lists:nth(Pos + 1, Str)] ++ "\" in filename.", [{bitmap, Bitmap}]),
			false
	end.
validate_name(Str) ->
	re:run(Str, "[/\]").
  
validate_path(Path, desc) -> true.