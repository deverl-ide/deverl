%% =====================================================================
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%% 
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%% 
%% You should have received a copy of the GNU General Public License
%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%
%% @author Tom Richmond <tr201@kent.ac.uk>
%% @author Mike Quested <mdq3@kent.ac.uk>
%% @copyright Tom Richmond, Mike Quested 2014
%%
%% @doc Displays the XRC based <em>Preferences</em> dialog.
%% @end
%% =====================================================================
  
-module(deverl_dlg_prefs_wx).
  
-include_lib("wx/include/wx.hrl").
-include("deverl.hrl").

%% wx_object
-behaviour(wx_object).
-export([init/1,
         terminate/2,
         code_change/3,
         handle_info/2,
         handle_call/3,
         handle_cast/2,
         handle_event/2]).
         
%% API
-export([start/1]).

%% Server state
-record(state, {frame,
                cur_pref
                }).
                
-define(CONSOLE_THEMES, [{"Light", ?wxBLACK, ?wxWHITE, {230,230,230}, ?wxRED},
                         {"Dark", ?wxWHITE, ?wxBLACK, {30,30,30}, {146, 91, 123}},
                         {"Matrix", {0,204,0}, ?wxBLACK, {30,30,30}, {146, 91, 123}}
                         ]).
  

%% =====================================================================
%% Client API
%% =====================================================================
  
%% =====================================================================
%% @doc Start a preference pane instance.
  
start(Config) ->
	wx_object:start_link(?MODULE, Config, []).
  

%% =====================================================================
%% Callback functions
%% =====================================================================
%% @hidden
init(Config) ->
  Parent = proplists:get_value(parent, Config),
  Xrc = wxXmlResource:get(),
  Frame = wxFrame:new(),
  deverl_lib_dlg_wx:win_variant(Frame),
  wxXmlResource:loadFrame(Xrc, Frame, Parent, "prefs"),
  
  %% Connect tools
  Connect = fun(UD) ->
    Id = wxXmlResource:getXRCID("tool_" ++ atom_to_list(UD)),
    wxFrame:connect(Frame,command_menu_selected,[{id,Id}, {userData,UD}])
  end,
  [Connect(Str) || Str <- [general, console, compiler, dialyzer]],
  
  %%
  %% General pref
  %%
  GenPrefs = deverl_sys_pref_gen:get_preference(general_prefs),
  
  %% Project directory
  ProjDir = wxXmlResource:xrcctrl(Frame, "proj_dir_st", wxStaticText),
  wxStaticText:setLabel(ProjDir, deverl_sys_pref_gen:get_preference(project_directory)),
  ProjBtn = wxXmlResource:xrcctrl(Frame, "proj_dir_btn", wxButton),
  wxButton:connect(ProjBtn, command_button_clicked, [{callback,
    fun(_E,_O) ->
      Dlg = wxDirDialog:new(Frame, []),
      case wxDialog:showModal(Dlg) of
        ?wxID_OK ->
          Dir = wxDirDialog:getPath(Dlg),
          wxStaticText:setLabel(ProjDir, Dir),
          deverl_sys_pref_gen:set_preference(project_directory, Dir);
        ?wxID_CANCEL ->
          ok
      end
    end
  }]),
  
  %% Set Paths
  GTc1 = wxXmlResource:xrcctrl(Frame, "path_to_erl_tc", wxTextCtrl),
  wxTextCtrl:setValue(GTc1, GenPrefs#general_prefs.path_to_erl),
  GTc2 = wxXmlResource:xrcctrl(Frame, "path_to_erlc_tc", wxTextCtrl),
  wxTextCtrl:setValue(GTc2, GenPrefs#general_prefs.path_to_erlc),
  GTc3 = wxXmlResource:xrcctrl(Frame, "path_to_dlzr_tc", wxTextCtrl),
  wxTextCtrl:setValue(GTc3, GenPrefs#general_prefs.path_to_dialyzer),
  
  %% Update text ctrl after browsing to file
  Browse = fun(Tc) ->
    FileDlg = wxFileDialog:new(Frame, [{style, ?wxFD_FILE_MUST_EXIST}]),
    case wxFileDialog:showModal(FileDlg) of
      ?wxID_CANCEL -> ok;
      ?wxID_OK ->
        Path = wxFileDialog:getPath(FileDlg),
        wxTextCtrl:setValue(Tc, Path)
    end,
    wxFileDialog:destroy(FileDlg)
  end,
  
  %% Browse path button handlers
  GBtn1 = wxXmlResource:xrcctrl(Frame, "path_to_erl_btn", wxButton),
  wxButton:connect(GBtn1, command_button_clicked, [{callback,
    fun(_E,_O) ->
      Browse(GTc1)
    end
  }]),
  
  GBtn2 = wxXmlResource:xrcctrl(Frame, "path_to_erlc_btn", wxButton),
  wxButton:connect(GBtn2, command_button_clicked, [{callback,
    fun(_E,_O) ->
      Browse(GTc2)
    end
  }]),
  
  GBtn3 = wxXmlResource:xrcctrl(Frame, "path_to_dlzr_btn", wxButton),
  wxButton:connect(GBtn3, command_button_clicked, [{callback,
    fun(_E,_O) ->
      Browse(GTc3)
    end
  }]),

  %% Update/set paths (in prefs) on kill focus
  SetPaths = fun(_E, _O) ->
    Prefs0 = deverl_sys_pref_gen:get_preference(general_prefs),
    Prefs1 = Prefs0#general_prefs{path_to_erl = wxTextCtrl:getValue(GTc1),
                                  path_to_erlc = wxTextCtrl:getValue(GTc2),
                                  path_to_dialyzer = wxTextCtrl:getValue(GTc3)},
    deverl_sys_pref_gen:set_preference(general_prefs, Prefs1)
  end,
  
  ResetErl = fun(E, O) ->
    SetPaths(E, O),
    case whereis(deverl_console_port_gen) of
      undefined -> %% no console currently
        deverl:restart_console(); %% start supervisor
      _ -> 
        deverl_console_port_gen:close_port() %% Reset the console, supervisor will restart
    end
  end,
  
  wxTextCtrl:connect(GTc1, kill_focus, [{callback, ResetErl}]),
  wxTextCtrl:connect(GTc2, kill_focus, [{callback, SetPaths}]),
  wxTextCtrl:connect(GTc3, kill_focus, [{callback, SetPaths}]),
  
  %% Home env var
  GSt = wxXmlResource:xrcctrl(Frame, "home_env_st", wxStaticText),
  wxStaticText:setLabel(GSt, GenPrefs#general_prefs.home_env_var),
  Btn2 = wxXmlResource:xrcctrl(Frame, "home_env_btn", wxButton),
  wxButton:connect(Frame, command_button_clicked, [{callback, 
    fun(_E,_O) ->
      TxtDlg = wxTextEntryDialog:new(Frame, "Enter your home directory:"),
      case wxTextEntryDialog:showModal(TxtDlg) of
        ?wxID_CANCEL -> ok;
        ?wxID_OK ->
          Str = wxTextEntryDialog:getValue(TxtDlg),
          wxStaticText:setLabel(GSt, Str)
      end,
      wxTextEntryDialog:destroy(TxtDlg)
    end
  }]),
  %%
  %% end General pref
  %%
  
  %%
  %% Console pref
  %%  
  F0 = deverl_sys_pref_gen:get_font(console_font),
  FStr0 = wxXmlResource:xrcctrl(Frame, "console_font_st", wxStaticText),
  wxStaticText:setLabel(FStr0, get_font_string(F0)),
  Themes = wxXmlResource:xrcctrl(Frame, "console_themes_pan", wxPanel),
  
  %% Add mini theme samples with radio button
  FSz = wxPanel:getSizer(Themes),
  AddTheme = fun({Name, Fg, Bg, _Mrkr, _Err}, Acc) ->
    Theme = wxXmlResource:loadPanel(Xrc, Themes, "theme_sample"),
    wxPanel:setBackgroundColour(wxXmlResource:xrcctrl(Theme, "theme_bg_pan", wxPanel), Bg),
    wxStaticText:setForegroundColour(wxXmlResource:xrcctrl(Theme, "theme_font_fg", wxStaticText), Fg),
    Radio = wxXmlResource:xrcctrl(Theme, "theme_radio", wxRadioButton),
    wxRadioButton:setLabel(Radio, Name),
    wxSizer:add(FSz, Theme),
    [Radio | Acc]
  end,
  RadioGroup = lists:foldl(AddTheme, [], ?CONSOLE_THEMES),
  
  %% Connect handlers
  DoTheme = fun(E=#wx{obj=Selected, userData=Group}, EvtObj) ->
    lists:foreach(fun(E) ->
      wxRadioButton:setValue(E, false)
    end, Group),
    wxRadioButton:setValue(Selected, true),
    Name = wxRadioButton:getLabel(Selected),
    Theme={Name, Fg, Bg, MrkrBg, ErrFg} = lists:keyfind(Name, 1, ?CONSOLE_THEMES),
    deverl_sys_pref_gen:set_preference(console_theme, Theme),
    deverl_console_wx:set_theme(Fg, Bg, MrkrBg, ErrFg)
  end,
  lists:foreach(fun(E) -> 
    wxPanel:connect(E, command_radiobutton_selected, [{callback, DoTheme}, 
        {userData, RadioGroup}])
  end, RadioGroup),

  Browse0 = wxXmlResource:xrcctrl(Frame, "console_font_btn", wxButton),
  %% Font
  wxButton:connect(Browse0, command_button_clicked, [{callback, 
    fun(_E,_O) ->
      Fd = wxFontData:new(),
      wxFontData:setInitialFont(Fd, deverl_sys_pref_gen:get_font(console_font)),
      Dlg = wxFontDialog:new(Parent, Fd),
      case wxDialog:showModal(Dlg) of
        ?wxID_OK ->
          F = wxFontData:getChosenFont(wxFontDialog:getFontData(Dlg)),
          wxStaticText:setLabel(FStr0, get_font_string(F)),
          deverl_sys_pref_gen:set_font(console_font, F),
          deverl_console_wx:set_font(F);
        ?wxID_CANCEL ->
          ok
      end
    end
  }]),
  %%
  %% end Console pref
  %%
  
  InitListCtrl = fun(Lc, Incs) ->
    wxListCtrl:insertColumn(Lc, 0, ""),
    wxListCtrl:setColumnWidth(Lc, 0, 300),
    lists:foldl(
      fun(Path, Acc) ->
        wxListCtrl:insertItem(Lc, Acc, ""),
        wxListCtrl:setItem(Lc, Acc, 0, filename:basename(Path)),
        deverl_lib_widgets:set_list_item_background(Lc, Acc),
        Acc + 1
      end, 0, Incs)
  end,
    
  InsertFolder = fun(Lc) ->
    Dlg = wxDirDialog:new(Frame, [{style, ?wxDD_DIR_MUST_EXIST}]),
    case wxDialog:showModal(Dlg) of
      ?wxID_OK ->
        Inc = wxDirDialog:getPath(Dlg),
        wxListCtrl:insertItem(Lc, 0, Inc);
      ?wxID_CANCEL ->
        ok
    end,
    wxDirDialog:destroy(Dlg)
  end,
  
  GetSelected = fun(Lc) ->
    wxListCtrl:getNextItem(Lc, -1, [{geometry, ?wxLIST_NEXT_ALL}, {state, ?wxLIST_STATE_SELECTED}])
  end,
  
  %%
  %% Dialyzer pref
  %%
  DlzrOpts = deverl_sys_pref_gen:get_preference(dialyzer_options),
  PLTStr = wxXmlResource:xrcctrl(Frame, "dlzr_plt_st", wxStaticText),
  wxStaticText:setLabel(PLTStr, DlzrOpts#dialyzer_options.plt),
  DlzrIncs = wxXmlResource:xrcctrl(Frame, "dlzr_incs_lc", wxListCtrl),
  InitListCtrl(DlzrIncs, DlzrOpts#dialyzer_options.include_dirs),
  
  %% Add/remove include folders
  Add0 = wxXmlResource:xrcctrl(Frame, "dlzr_add_inc_btn", wxButton),
  wxButton:connect(Add0, command_button_clicked, [{callback,
    fun(_E,_O) ->
      InsertFolder(DlzrIncs)
    end
  }]),
  Rm0 = wxXmlResource:xrcctrl(Frame, "dlzr_rm_inc_btn", wxButton),
  wxButton:connect(Rm0, command_button_clicked, [{callback,
    fun(_E,_O) ->
      Idx = GetSelected(DlzrIncs),
      wxListCtrl:deleteItem(DlzrIncs, Idx)
    end
  }]),
  
  %% Checkboxes
  DlzrCb1 = wxXmlResource:xrcctrl(Frame, "dlzr_verbose_out", wxCheckBox),
  wxCheckBox:setValue(DlzrCb1, DlzrOpts#dialyzer_options.verbose_out),
  DlzrCb2 = wxXmlResource:xrcctrl(Frame, "dlzr_stats_out", wxCheckBox),
  wxCheckBox:setValue(DlzrCb2, DlzrOpts#dialyzer_options.stats_out),
  DlzrCb3 = wxXmlResource:xrcctrl(Frame, "dlzr_quiet_out", wxCheckBox),
  wxCheckBox:setValue(DlzrCb3, DlzrOpts#dialyzer_options.quiet_out),
  
  %%
  %% Compiler pref
  %%
  CmpOpts = deverl_sys_pref_gen:get_preference(compiler_options),
  CmpIncs = wxXmlResource:xrcctrl(Frame, "cmp_incs_lc", wxListCtrl),
  InitListCtrl(CmpIncs, CmpOpts#compiler_options.include_dirs),
  
  %% Add/remove include folders
  Add1 = wxXmlResource:xrcctrl(Frame, "cmp_add_inc_btn", wxButton),
  wxButton:connect(Add1, command_button_clicked, [{callback,
    fun(_E,_O) ->
      InsertFolder(CmpIncs)
    end
  }]),
  Rm1 = wxXmlResource:xrcctrl(Frame, "cmp_rm_inc_btn", wxButton),
  wxButton:connect(Rm1, command_button_clicked, [{callback,
    fun(_E,_O) ->
      Idx = GetSelected(CmpIncs),
      wxListCtrl:deleteItem(CmpIncs, Idx)
    end
  }]),
  
  %% Checkboxes
  CmpCb1 = wxXmlResource:xrcctrl(Frame, "cmp_show_warns_cb", wxCheckBox),
  wxCheckBox:setValue(CmpCb1, CmpOpts#compiler_options.show_warnings),
  CmpCb2 = wxXmlResource:xrcctrl(Frame, "cmp_warn_to_err_cb", wxCheckBox),
  wxCheckBox:setValue(CmpCb2, CmpOpts#compiler_options.warn_to_err),
  CmpCb3 = wxXmlResource:xrcctrl(Frame, "cmp_verbose_out_cb", wxCheckBox),
  wxCheckBox:setValue(CmpCb3, CmpOpts#compiler_options.verbose_out),
  CmpCb4 = wxXmlResource:xrcctrl(Frame, "cmp_debug_info_cb", wxCheckBox),
  wxCheckBox:setValue(CmpCb4, CmpOpts#compiler_options.debug_info),
  
  wxFrame:centre(Frame),
  wxFrame:fit(Frame),
  wxFrame:show(Frame),  
  
  %% Active pref
  Pref0 = wxXmlResource:xrcctrl(Frame, "console", wxPanel),
  
  {Frame, #state{frame=Frame, cur_pref=Pref0}}.

%% @hidden
handle_info(Msg, State) ->
  {noreply,State}.
%% @hidden  
handle_call(_Msg, _From, State) ->
  {reply, ok, State}.
%% @hidden
handle_cast(Msg, State) ->
  io:format("Got cast ~p~n",[Msg]),
  {noreply,State}.

%% Swap pref panels
%% @hidden
handle_event(#wx{event=#wxCommand{type=command_menu_selected}, userData=UD}, 
             State=#state{frame=Frame, cur_pref=Pref0}) ->
  Pref1 = wxXmlResource:xrcctrl(Frame, atom_to_list(UD), wxPanel),
  wxPanel:hide(Pref0),
  wxPanel:show(Pref1),
  wxFrame:fit(Frame),
  {noreply, State#state{cur_pref=Pref1}};
    
%% Event catchall for testing
handle_event(Ev=#wx{}, State) ->
  io:format("Prefs event catchall: ~p~n", [Ev]),
  {noreply, State}.
%% @hidden   
code_change(_, _, State) ->
  {stop, not_yet_implemented, State}.
%% @hidden
terminate(_Reason, #state{frame=Frame}) ->
  wxFrame:destroy(Frame).
    

%% =====================================================================
%% Internal functions
%% =====================================================================

%% =====================================================================
%% @doc

-spec get_font_string(wxFont:wxFont()) -> io_lib:chars().

get_font_string(Font) ->
  io_lib:format("~s ~p pt.", [wxFont:getFaceName(Font), wxFont:getPointSize(Font)]).