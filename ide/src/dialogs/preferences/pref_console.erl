%% =====================================================================
%% @author
%% @copyright
%% @title
%% @version
%% @doc 
%% @end
%% =====================================================================

-module(pref_console).

-include_lib("wx/include/wx.hrl").

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
-record(state, {parent,
                themes,
                font,
                font_string
            	 }).


%% =====================================================================
%% Client API
%% =====================================================================

start(Config) ->
  wx_object:start_link(?MODULE, Config, []).


%% =====================================================================
%% Callback functions
%% =====================================================================

init(Config) ->
  wx:batch(fun() -> do_init(Config) end).

do_init(Config) ->
  Parent = proplists:get_value(parent, Config),
  Panel = wxPanel:new(Parent),
  
  Sz = wxBoxSizer:new(?wxHORIZONTAL), %% For left and right margins
  wxWindow:setSizer(Panel, Sz),
  wxSizer:addSpacer(Sz, 20),
  
  MainSz = wxBoxSizer:new(?wxVERTICAL),
  wxSizer:addSpacer(MainSz, 20),
 
  %% Font
  add_bold_label(Panel, MainSz, "Font"),
  
  Font = sys_pref_manager:get_font(console),
  
  FontSz = wxBoxSizer:new(?wxHORIZONTAL),
  FontStr = wxStaticText:new(Panel, ?wxID_ANY, get_font_string(Font)),
  wxSizer:add(FontSz, FontStr, [{flag, ?wxALIGN_CENTRE_VERTICAL bor ?wxRIGHT}, {border, 50}]),
  Browse = wxButton:new(Panel, ?wxID_ANY, [{label, "Browse..."}]),
  wxSizer:add(FontSz, Browse, [{flag, ?wxALIGN_CENTRE_VERTICAL}]),
  
  wxSizer:add(MainSz, FontSz, [{proportion, 0}]),
  wxSizer:addSpacer(MainSz, 20),
  
  %% Theme
  add_bold_label(Panel, MainSz, "Theme"),
  
  %% Console themes {Name, FgColour, BgColour, MarkerBg, ErrorFg}
  Themes = [{"Light", ?wxBLACK, ?wxWHITE, {230,230,230}, ?wxRED},
            {"Dark", ?wxWHITE, ?wxBLACK, {30,30,30}, {146, 91, 123}},
            {"Matrix", {0,204,0}, ?wxBLACK, {30,30,30}, {146, 91, 123}}],
  
  ThemeSz = wxBoxSizer:new(?wxHORIZONTAL),
  
  SavedTheme = sys_pref_manager:get_preference(console_theme),
  
  ThemeEx = 
    fun({Name, Fg, Bg, MrkrBf, ErrFg}=Theme) ->
      Profile = wxWindow:new(Panel, ?wxID_ANY, [{style, ?wxBORDER_SIMPLE}, {size, {65,40}}]),
      T = wxStaticText:new(Profile, ?wxID_ANY, "1> 1+1.\n3\n2>"),
      wxWindow:setFont(T, wxFont:new(8, ?wxFONTFAMILY_TELETYPE, ?wxNORMAL, ?wxNORMAL)),
      wxWindow:setForegroundColour(T, Fg),
      wxWindow:setBackgroundColour(Profile, Bg),
      Radio = wxRadioButton:new(Panel, ?wxID_ANY, Name, []),
      wxSizer:add(ThemeSz, Profile, [{proportion, 1}, {flag, ?wxEXPAND bor ?wxRIGHT}, {border, 5}]),
      wxSizer:add(ThemeSz, Radio, [{flag, ?wxALIGN_CENTRE}]),
      wxSizer:addSpacer(ThemeSz, 10),    
      wxRadioButton:connect(Radio, command_radiobutton_selected, [{userData,Theme}]),
      case SavedTheme of
        Theme ->
          wxRadioButton:setValue(Radio, true);
        _ ->
          ok
      end
    end,

  [ ThemeEx(Theme) || Theme <- Themes ],
  
  wxSizer:add(MainSz, ThemeSz, []),
  wxSizer:addSpacer(MainSz, 20),
  
  wxSizer:add(Sz, MainSz, []),
  wxSizer:addSpacer(Sz, 20),
  
  wxButton:connect(Browse, command_button_clicked, []),
  
  State=#state{parent=Panel,
               themes=Themes,
               font=Font,
               font_string=FontStr},
    
  {Panel, State}.

handle_event(#wx{id=Id, event=#wxCommand{type=command_radiobutton_selected}, userData={_Name,Fg,Bg,MrkrBg,ErrFg}=Theme}, State) ->
  console_wx:set_theme(Fg, Bg, MrkrBg, ErrFg),
  sys_pref_manager:set_preference(console_theme, Theme),
  {noreply, State};
handle_event(#wx{obj=Browse, event=#wxCommand{type=command_button_clicked}}, 
             State=#state{parent=Parent, font=Font, font_string=FontStr}) ->
  Fd = wxFontData:new(),
  wxFontData:setInitialFont(Fd, Font),
  Dialog = wxFontDialog:new(Parent, Fd),
  Result = case wxDialog:showModal(Dialog) of
    ?wxID_OK ->
      NewFont = wxFontData:getChosenFont(wxFontDialog:getFontData(Dialog)),
      wxStaticText:setLabel(FontStr, get_font_string(NewFont)),
      %% Crap way to set font prefs, need a function that does all this lot
      sys_pref_manager:set_preference(console_font_size, wxFont:getPointSize(NewFont)),
      sys_pref_manager:set_preference(console_font_family, wxFont:getFamily(NewFont)),
      sys_pref_manager:set_preference(console_font_style, wxFont:getStyle(NewFont)),
      sys_pref_manager:set_preference(console_font_weight, wxFont:getWeight(NewFont)),
      sys_pref_manager:set_preference(console_font_facename, wxFont:getFaceName(NewFont)),
      console_wx:set_font(NewFont),
      NewFont;
    ?wxID_CANCEL ->
			Font
	end,
  {noreply,State#state{font=Result}}.

handle_info(Msg, State) ->
  io:format( "Got Info ~p~n",[Msg]),
  {noreply,State}.

handle_call(shutdown, _From, State=#state{parent=Panel}) ->
  wxWindow:destroy(Panel),
  {stop, normal, ok, State};
handle_call(Msg, _From, State) ->
  io:format("Got Call ~p~n",[Msg]),
  {reply,ok,State}.

handle_cast(Msg, State) ->
  io:format("Got cast ~p~n",[Msg]),
  {noreply,State}.

code_change(_, _, State) ->
  {stop, ignore, State}.

terminate(_Reason, _) ->
  ok.
  
add_bold_label(Parent, Sz, Name) ->
  Label = wxStaticText:new(Parent, ?wxID_ANY, Name),
  Font = wxStaticText:getFont(Label),
  wxFont:setWeight(Font, ?wxFONTWEIGHT_BOLD),
  wxStaticText:setFont(Label, Font),
  wxSizer:add(Sz, Label, []),
  wxSizer:addSpacer(Sz, 10).
  
get_font_string(Font) ->
  io_lib:format("~s ~p pt.", [wxFont:getFaceName(Font), wxFont:getPointSize(Font)]).