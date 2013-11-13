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
                themes
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
     
  MainSz = wxBoxSizer:new(?wxHORIZONTAL),
  wxWindow:setSizer(Panel, MainSz),
  wxSizer:addSpacer(MainSz, 20),

  wxSizer:add(MainSz, wxStaticText:new(Panel, ?wxID_ANY, "Theme:"), [{border, 20}, {flag, ?wxTOP bor ?wxRIGHT}]),
  
  %% Console themes {Name, FgColour, BgColour}
  Themes = [{"Light", ?wxBLACK, ?wxWHITE},
            {"Dark", ?wxWHITE, ?wxBLACK},
            {"Matrix", {0,204,0}, ?wxBLACK}],
  
  ThemeSizer = wxFlexGridSizer:new(length(Themes), 2, 10, 10),
  
  SavedTheme = sys_pref_manager:get_preference(console_theme),
  
  ThemeEx = 
    fun({Name, Fg, Bg}=Theme) ->
      Profile = wxWindow:new(Panel, ?wxID_ANY, [{style, ?wxBORDER_SIMPLE}, {size, {65,40}}]),
      T = wxStaticText:new(Profile, ?wxID_ANY, "1> 1+1.\n3\n2>"),
      wxWindow:setFont(T, wxFont:new(8, ?wxFONTFAMILY_TELETYPE, ?wxNORMAL, ?wxNORMAL)),
      wxWindow:setForegroundColour(T, Fg),
      wxWindow:setBackgroundColour(Profile, Bg),
      Radio = wxRadioButton:new(Panel, ?wxID_ANY, Name, []),
      wxSizer:add(ThemeSizer, Profile, [{proportion, 1}, {flag, ?wxEXPAND}]),
      wxSizer:add(ThemeSizer, Radio, [{flag, ?wxALIGN_CENTRE}]),    
      wxRadioButton:connect(Radio, command_radiobutton_selected, [{userData,Theme}]),
      case SavedTheme of
        Theme ->
          wxRadioButton:setValue(Radio, true);
        _ ->
          ok
      end
    end,

  [ ThemeEx(Theme) || Theme <- Themes ],

  wxFlexGridSizer:addGrowableCol(ThemeSizer, 2),
  
  wxSizer:add(MainSz, ThemeSizer, [{border, 20}, {flag, ?wxTOP bor ?wxBOTTOM}]),
  wxSizer:addSpacer(MainSz, 20),
    
  {Panel, #state{parent=Panel, themes=Themes}}.

handle_event(#wx{id=Id, event=#wxCommand{type=command_radiobutton_selected}, userData={_Name,Fg,Bg}=Theme}, State) ->
  console_wx:set_theme(Fg, Bg),
  sys_pref_manager:set_preference(console_theme, Theme),
  {noreply, State};
handle_event(Ev = #wx{}, State = #state{}) ->
  io:format("Got Event ~p~n",[Ev]),
  {noreply,State}.

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