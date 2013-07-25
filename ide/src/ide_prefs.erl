%% The preference pane for the IDE
%% ide.erl

-module(ide_prefs).

-include_lib("wx/include/wx.hrl").
-behaviour(wx_object).

%% wx_objects callbacks
-export([start/0, init/1, terminate/2,  code_change/3,
         handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).
         
-record(state, {frame, pref_panel, pref}).        
         
-define(PREF_GENERAL,  1).
-define(PREF_EDITOR, 2).
-define(PREF_CONSOLE,  3).
-define(PREF_DEBUG,   4).


%% =====================================================================
%% @doc Start a preference pane instance.

start() ->
  start([]).
  
start(Args) ->
	wx_object:start_link(?MODULE, Args, []).
  
  
%% =====================================================================
%% @doc Initialise the preference pane.

init(Options) ->
	wx:new(Options), %% For testing independently
  
	Frame = wxFrame:new(wx:null(), ?wxID_ANY, "Preferences", 
													 [{size,{-1,-1}},
                            {style, ?wxSYSTEM_MENU bor
                                                   ?wxFRAME_NO_TASKBAR bor
                                                   ?wxCLOSE_BOX}]),
          
  ToolBar = wxFrame:createToolBar(Frame, [{style, ?wxTB_TEXT}]),
  wxToolBar:setToolBitmapSize(ToolBar, {48,48}),
  %% Id, text, bitmap path, args, add seperator
  Tools = [{?PREF_GENERAL,  "General", "../icons/prefs/general.png",    [],  false},
           {?PREF_EDITOR, "Editor", "../icons/prefs/editor.png",   [],  false},
           {?PREF_CONSOLE, "Console", "../icons/prefs/console.png",   [],  false},
           {?PREF_DEBUG, "Debugger", "../icons/prefs/debug.png",   [],  false}],

  AddTool = fun({Id, Tooltip, Filename, Args, true}) ->
            wxToolBar:addRadioTool(ToolBar, Id, Tooltip, wxBitmap:new(wxImage:new(Filename)), Args),
            wxToolBar:addSeparator(ToolBar);
           ({Id, Tooltip, Filename, Args, _}) ->
            wxToolBar:addRadioTool(ToolBar, Id, Tooltip, wxBitmap:new(wxImage:new(Filename)), Args)
            end,       

  [AddTool(Tool) || Tool <- Tools],

  wxToolBar:realize(ToolBar),
  wxFrame:connect(Frame, command_menu_selected, [{userData, Tools}]),
  
  Panel = wxPanel:new(Frame),
  PanelSz = wxBoxSizer:new(?wxVERTICAL),
  wxPanel:setSizer(Panel, PanelSz),
  
  wxFrame:show(Frame),
  
  State = #state{frame=Frame, pref_panel={Panel, PanelSz}},
  
  %% Load the first preference pane
  PrefPane = load_pref("general", State),
  
  wxSizer:add(PanelSz, PrefPane, [{proportion,1}, {flag, ?wxEXPAND}]),
  wxSizer:layout(PanelSz),

  FrameSizer = wxBoxSizer:new(?wxVERTICAL),
  wxSizer:add(FrameSizer, Panel),
  wxWindow:setSizerAndFit(Frame, FrameSizer),
  
  {Frame, State#state{pref=PrefPane}}.
  

%% =====================================================================
%% @doc OTP behaviour callbacks

handle_info(Msg, State) ->
  io:format("Got Info ~p~n",[Msg]),
  {noreply,State}.
    
handle_call(Msg, _From, State) ->
  {reply,{error, nyi}, State}.
    
handle_cast(Msg, State) ->
  io:format("Got cast ~p~n",[Msg]),
  {noreply,State}.
    
%% Catch menu clicks
handle_event(Ev = #wx{id=Id, event=#wxCommand{type=command_menu_selected}, userData=Tb}, 
             State = #state{pref_panel={Panel,Sz}, pref=Pref}) ->
  {_,Str,_,_,_} = proplists:lookup(Id,Tb),
  wxSizer:detach(Sz, Pref),
  wx_object:call(Pref, shutdown),
  NewPref = load_pref(Str, State),
  wxSizer:add(Sz, NewPref, [{proportion,1}, {flag, ?wxEXPAND}]),
  wxSizer:layout(Sz),  
  {noreply, State#state{pref=NewPref}};
    
%% Event catchall for testing
handle_event(Ev = #wx{}, State) ->
  io:format("Prefs event catchall: ~p\n", [Ev]),
  {noreply, State}.
    
code_change(_, _, State) ->
  {stop, not_yet_implemented, State}.

terminate(_Reason, _State) ->
  ok.
    

%% =====================================================================
%% @doc Load a preference pane.

load_pref(Pref, #state{frame=Frame, pref_panel={Panel,Sz}}) ->
  ModStr = "pref_" ++ string:to_lower(Pref),
  Mod = list_to_atom(ModStr),
  ModFile = ModStr ++ ".erl",
  Mod:start([{parent, Panel}]).
  

%% =====================================================================
%% @doc OTP behaviour callbacks