%% =====================================================================
%% @author
%% @copyright
%% @title
%% @version
%% @doc 
%% @end
%% =====================================================================
  
-module(ide_dlg_prefs_wx).
  
-include_lib("wx/include/wx.hrl").
-include("ide.hrl").

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
  
init(Config) ->
  Parent = proplists:get_value(parent, Config),
  Xrc = wxXmlResource:get(),
  Frame = wxFrame:new(),
  ide_lib_dlg_wx:win_var(Frame),
  wxXmlResource:loadFrame(Xrc, Frame, Parent, "prefs"),
  
  %% Connect tools
  Connect = fun(UD) ->
    Id = wxXmlResource:getXRCID("tool_" ++ atom_to_list(UD)),
    wxFrame:connect(Frame,command_menu_selected,[{id,Id}, {userData,UD}])
  end,
  [Connect(Str) || Str <- [general, console, compiler, dialyzer]],
   
  %% Console pref
  Pref0 = wxXmlResource:xrcctrl(Frame, "console", wxPanel),
  F0 = ide_sys_pref_gen:get_font(console_font),
  FStr0 = wxXmlResource:xrcctrl(Frame, "console_font_st", wxStaticText),
  wxStaticText:setLabel(FStr0, get_font_string(F0)),
  Themes = wxXmlResource:xrcctrl(Frame, "console_themes_p", wxPanel),
  add_themes(Themes),
  Browse0 = wxXmlResource:xrcctrl(Frame, "console_font_btn", wxButton),
  %% Choose font
  wxButton:connect(Browse0, command_button_clicked, [{callback, 
    fun(_E,_O) ->
      Fd = wxFontData:new(),
      wxFontData:setInitialFont(Fd, ide_sys_pref_gen:get_font(console_font)),
      Dlg = wxFontDialog:new(Parent, Fd),
      case wxDialog:showModal(Dlg) of
        ?wxID_OK ->
          F = wxFontData:getChosenFont(wxFontDialog:getFontData(Dlg)),
          wxStaticText:setLabel(FStr0, get_font_string(F)),
          ide_sys_pref_gen:set_font(console_font, F),
          ide_console_wx:set_font(F),
          wxFrame:layout(Frame);
        ?wxID_CANCEL ->
          ok
      end
    end
  }]),
  
  %% Dialyzer pref
  DlzrOpts = ide_sys_pref_gen:get_preference(dialyzer_options),
  PLTStr = wxXmlResource:xrcctrl(Frame, "dlzr_plt_st", wxStaticText),
  wxStaticText:setLabel(PLTStr, DlzrOpts#dialyzer_options.plt),
  DlzrIncs = wxXmlResource:xrcctrl(Frame, "dlzr_incs_lc", wxListCtrl),
  %% Display includes
  wxListCtrl:insertColumn(DlzrIncs, 0, ""),
  wxListCtrl:setColumnWidth(DlzrIncs, 0, 300),
  lists:foldl(
    fun(Path, Acc) ->
      wxListCtrl:insertItem(DlzrIncs, Acc, ""),
      wxListCtrl:setItem(DlzrIncs, Acc, 0, filename:basename(Path)),
      ide_lib_widgets:set_list_item_background(DlzrIncs, Acc),
      Acc + 1
    end, 0, DlzrOpts#dialyzer_options.include_dirs),
  %% Add/remove include folders
  Add0 = wxXmlResource:xrcctrl(Frame, "dlzr_add_inc_btn", wxButton),
  wxButton:connect(Add0, command_button_clicked, [{callback,
    fun(_E,_O) ->
      Dlg = wxDirDialog:new(Frame, [{style, ?wxDD_DIR_MUST_EXIST}]),
      case wxDialog:showModal(Dlg) of
        ?wxID_OK ->
          Inc = wxDirDialog:getPath(Dlg),
          wxListCtrl:insertItem(DlzrIncs, 0, Inc);
        ?wxID_CANCEL ->
          ok
      end
    end
  }]),
  Rm0 = wxXmlResource:xrcctrl(Frame, "dlzr_rm_inc_btn", wxButton),
  
  %% Compiler pref
  
  wxFrame:centre(Frame),
  wxFrame:fit(Frame),
  wxFrame:show(Frame),  
  {Frame, #state{frame=Frame, cur_pref=Pref0}}.


handle_info(Msg, State) ->
  {noreply,State}.
    
handle_call(_Msg, _From, State) ->
  {reply, ok, State}.
    
handle_cast(Msg, State) ->
  io:format("Got cast ~p~n",[Msg]),
  {noreply,State}.

%% Swap pref panels
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
    
code_change(_, _, State) ->
  {stop, not_yet_implemented, State}.

terminate(_Reason, #state{frame=Frame}) ->
  % wxFrame:disconnect(Frame),
  wxFrame:destroy(Frame).
    

%% =====================================================================
%% Internal functions
%% =====================================================================

%% =====================================================================
%% @doc

-spec get_font_string(wxFont:wxFont()) -> io_lib:chars().

get_font_string(Font) ->
  io_lib:format("~s ~p pt.", [wxFont:getFaceName(Font), wxFont:getPointSize(Font)]).
  

add_themes(Parent) ->
  %% Console themes {Name, FgColour, BgColour, MarkerBg, ErrorFg}
  Themes = [{"Light", ?wxBLACK, ?wxWHITE, {230,230,230}, ?wxRED},
            {"Dark", ?wxWHITE, ?wxBLACK, {30,30,30}, {146, 91, 123}},
            {"Matrix", {0,204,0}, ?wxBLACK, {30,30,30}, {146, 91, 123}}],
            
  SavedTheme = ide_sys_pref_gen:get_preference(console_theme),  
  % Sz = wxGridSizer:new(3, [{vgap, 5}, {hgap, 5}]),
  Sz = wxBoxSizer:new(?wxHORIZONTAL),
  Add = fun({Name, Fg, Bg, Mrkr, Err}=Theme) ->
    Sz1 = wxBoxSizer:new(?wxHORIZONTAL),
    Sample = wxPanel:new(Parent, [{style, ?wxBORDER_SIMPLE}, {size, {65,40}}]),
    wxPanel:setMinSize(Sample, {65, 40}),
    T = wxStaticText:new(Sample, ?wxID_ANY, "1> 1+1.\n3\n2>"),
    wxWindow:setFont(T, wxFont:new(8, ?wxFONTFAMILY_TELETYPE, ?wxNORMAL, ?wxNORMAL)),
    wxWindow:setForegroundColour(T, Fg),
    wxWindow:setBackgroundColour(Sample, Bg),
    Radio = wxRadioButton:new(Parent, ?wxID_ANY, Name, []),
    wxSizer:add(Sz1, Sample, [{flag, ?wxALIGN_CENTRE}]),
    wxSizer:add(Sz1, Radio, [{flag, ?wxALIGN_CENTRE}]),
    wxSizer:add(Sz, Sz1, [{flag, ?wxALIGN_CENTRE}]),
    wxSizer:layout(Sz),
    wxPanel:layout(Sample),
    case SavedTheme of
      Theme ->
        wxRadioButton:setValue(Radio, true);
      _ ->
        ok
    end,
    % Evt Handler
    Update = fun(_E,_O) ->
      ide_console_wx:set_theme(Fg, Bg, Mrkr, Err),
      ide_sys_pref_gen:set_preference(console_theme, Theme)
    end,
    wxRadioButton:connect(Radio, command_radiobutton_selected, [{callback,Update}])
  end, 
  [ Add(Theme) || Theme <- Themes ],
  wxPanel:setSizer(Parent, Sz),
  wxPanel:fit(Parent).