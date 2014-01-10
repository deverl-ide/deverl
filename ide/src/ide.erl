%% =====================================================================
%% @author Tom Richmond <tr201@kent.ac.uk>
%% @author Mike Quested <mdq3@kent.ac.uk>
%% @copyright 2014 Tom Richmond, Mike Quested
%% @version 1
%% @doc Starts an instance of the IDE. Builds all wx components.
%% @end
%% =====================================================================

-module(ide).

-include_lib("wx/include/wx.hrl").
-include("ide.hrl").

%% wx_object
-behaviour(wx_object).
-export([init/1, terminate/2,  code_change/3,
         handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).
	
%% API			 
-export([start/0,
         start/1,
         set_title/1,
         display_output_window/1,
         toggle_menu_group/2
         ]).

%% Server state
-record(state, {frame,
                workspace :: wxAuiNotebook:wxAuiNotebook(),    %% Notebook
                utilities,                                     %% The utilities pane
                util_tabbed, %% ide_tabbed_win_wx
                left_pane,                                     %% The test pane
                workspace_manager,                             %% Tabbed UI manager for editors
                splitter_sidebar :: wxSplitterWindow:wxSplitterWindow(), %% The vertical splitter
                splitter_utilities :: wxSplitterWindow:wxSplitterWindow(), %% The horizontal splitter
                splitter_sidebar_pos :: integer(),
                splitter_utilities_pos :: integer(),
                splitter_output_pos :: integer(),
                splitter_output_active :: wxWindow:wxWindow(),
                menu_ets
                }).
								
%% Macros
-define(DEFAULT_FRAME_WIDTH,  1100).
-define(DEFAULT_FRAME_HEIGHT, 680).
-define(DEFAULT_UTIL_HEIGHT,  200).
-define(DEFAULT_TEST_WIDTH,   200).
-define(SPLITTER_SIDEBAR, 100).
-define(SPLITTER_UTILITIES, 101).
-define(SPLIITER_LOG, 102).
-define(SPLITTER_SIDEBAR_SASH_POS_DEFAULT, 215).
-define(SPLITTER_UTILITIES_SASH_POS_DEFAULT, -200).
-define(SPLITTER_LOG_SASH_POS_DEFAULT, -500).
-define(FRAME_TITLE, "Erlang IDE").
-define(BUTTON_HIDE_OUTPUT, 0).
-define(BUTTON_LOG, 1).
-define(BUTTON_COMPILER_OUTPUT, 2).


%% =====================================================================
%% Client API
%% =====================================================================

%% =====================================================================
%% @doc Start the erlang IDE.
%% For debug Options @see wx:debug/1

-spec start() -> wx_object:wx_object().

start() ->
  start([]).
  
-spec start([Option]) -> wx_object:wx_object() when
  Option :: {debug, list() | atom()} |
            {silent_start, boolean()}.
start(Options) ->
	WxObj = wx_object:start({local, ?MODULE}, ?MODULE, Options, [{debug, [log]}]),
  Pid = wx_object:get_pid(WxObj),
  {ok, Pid}.


%% =====================================================================
%% @doc Update the frame's title.

-spec set_title(string()) -> ok.

set_title(Title) ->
	wx_object:cast(?MODULE, {title, Title}).


%% =====================================================================
%% @doc Enable/disable a menu group.
%% Menu items can be associated to one or more menu groups (bit 
%% flags, defined in ide.hrl). The Mask is compared using bitwise AND to
%% every menu item. If the menu item has the group bit set then it will
%% be enabled when Toggle is true and disabled when Toggle is false.

-spec toggle_menu_group(integer(), boolean()) -> ok.

toggle_menu_group(Mask, Toggle) ->
	wx_object:cast(?MODULE, {toggle_menu_group, Mask, Toggle}).


%% =====================================================================
%% @doc Show the window identified by WinId in the output window, hiding the
%% window currently displayed. 

-spec display_output_window(integer()) -> ok.

display_output_window(WinId) ->
  wx_object:cast(?MODULE, {output_display, WinId}).		
  
  
%% =====================================================================
%% Callback functions
%% =====================================================================
%% @hidden
init(Options) ->
	wx:new(Options),
	WxEnv = wx:get_env(),
	process_flag(trap_exit, true),
  
  %% Set small window variant globally
  % wxSystemOptions:setOption("window-default-variant", ?wxWINDOW_VARIANT_SMALL),
  
  Xrc = wxXmlResource:get(),
  wxXmlResource:initAllHandlers(Xrc),
  true = wxXmlResource:load(Xrc, ide_lib_widgets:rc_dir("dlgs.xrc")),
  true = wxXmlResource:load(Xrc, ide_lib_widgets:rc_dir("derivdlg.xrc")),

	Frame = wxFrame:new(wx:null(), ?wxID_ANY, ?FRAME_TITLE, [{size,{?DEFAULT_FRAME_WIDTH,?DEFAULT_FRAME_HEIGHT}}]),
	wxFrame:connect(Frame, close_window),
	wxFrame:setMinSize(Frame, {300,200}),
	
	%% Load modules that should be started by OTP Application and not here
  ide_sys_pref_gen:start([{wx_env, WxEnv}]),
  ProjDir = ide_sys_pref_gen:get_preference(project_directory),
  case filelib:is_dir(ProjDir) of
    false ->
      file:make_dir(ProjDir);
    true ->
      ok
  end,
	ide_proj_man:start([{frame, Frame}, {wx_env, WxEnv}]),

	FrameSizer = wxBoxSizer:new(?wxVERTICAL),
	wxWindow:setSizer(Frame, FrameSizer),
	
	SplitterStyle = case os:type() of
		{_, darwin} -> ?wxSP_3DSASH bor ?wxSP_LIVE_UPDATE;
    {win32, _} -> ?wxSP_3DSASH bor ?wxSP_LIVE_UPDATE bor ?wxBORDER_THEME;
		_ -> ?wxSP_3DSASH
	end,
	SplitterUtilities = wxSplitterWindow:new(Frame, [{id, ?SPLITTER_UTILITIES}, %% Primary splitter
		{style, SplitterStyle}]),
	SplitterSidebar = wxSplitterWindow:new(SplitterUtilities, [{id, ?SPLITTER_SIDEBAR},
		{style, SplitterStyle}]),
    
	wxSplitterWindow:setSashGravity(SplitterUtilities, 0.5),
	wxSplitterWindow:setSashGravity(SplitterSidebar, 0.60),

	wxSizer:add(FrameSizer, SplitterUtilities, [{flag, ?wxEXPAND}, {proportion, 1}]),

	%% Status bar %%
	StatusBar = ide_sb_wx:start([{parent, Frame}]),

	%% Menubar %%
  MenuEts = ide_menu:create([{parent, Frame}]),

	wxSizer:add(FrameSizer, StatusBar, [{flag, ?wxEXPAND},
                                        {proportion, 0}]),
                                        
	Workspace = create_workspace(SplitterSidebar),

	%% The left window
	LeftWindow = create_left_window(Frame, SplitterSidebar),

	%% The bottom pane/utility window
	{Utilities, TabbedWindow, ActiveLogWindow} = create_utils(SplitterUtilities),

	wxSplitterWindow:splitVertically(SplitterSidebar, LeftWindow, Workspace,
	                  [{sashPosition, ?SPLITTER_SIDEBAR_SASH_POS_DEFAULT}]),

	wxSplitterWindow:splitHorizontally(SplitterUtilities, SplitterSidebar, Utilities,
                  [{sashPosition, ?SPLITTER_UTILITIES_SASH_POS_DEFAULT}]),

	wxSizer:layout(FrameSizer),
	wxFrame:center(Frame),
	wxFrame:show(Frame),

	wxSplitterWindow:setSashGravity(SplitterUtilities, 1.0), % Only the top window grows on resize
	wxSplitterWindow:setSashGravity(SplitterSidebar, 0.0), % Only the right window grows

  wxSplitterWindow:connect(Frame, command_splitter_sash_pos_changed),
  wxSplitterWindow:connect(Frame, command_splitter_doubleclicked),

	%% Testing accelerator table
  % AccelTab = wxAcceleratorTable:new(1,
  %   [wxAcceleratorEntry:new([{flags, ?wxACCEL_NORMAL}, {keyCode, ?WXK_SPACE}, {cmd, ?MENU_ID_FONT}])]),
  % wxFrame:setAcceleratorTable(Frame, AccelTab),
  
  %% Toggle menu defaults
  toggle_menu_group(?MENU_GROUP_NOTEBOOK_EMPTY bor 
                    ?MENU_GROUP_PROJECTS_EMPTY, false),

  {Frame, #state{
						frame=Frame,
            left_pane=LeftWindow,
            utilities=Utilities,
            util_tabbed=TabbedWindow,
            splitter_sidebar_pos=?SPLITTER_SIDEBAR_SASH_POS_DEFAULT,
            splitter_utilities_pos=?SPLITTER_UTILITIES_SASH_POS_DEFAULT,
            splitter_output_pos=?SPLITTER_LOG_SASH_POS_DEFAULT,
            splitter_sidebar=SplitterSidebar,
            splitter_utilities=SplitterUtilities,
            splitter_output_active=ActiveLogWindow,
						workspace=Workspace,
            menu_ets=MenuEts
            }}.

%% Deal with trapped exit signals
%% @hidden
handle_info({'EXIT',_, shutdown}, State) ->
  io:format("Got Info 2~n"),
  {noreply,State};
handle_info({'EXIT',A, normal}, State) ->
  io:format("Got Info 3~n~p~n", [A]),
  {noreply,State};
handle_info(Msg, State) ->
  io:format("Got Info ~p~n",[Msg]),
  {noreply,State}.

%% @hidden
handle_call(frame, _From, State) ->
	{reply, State#state.frame, State}.

%% @hidden
handle_cast({toggle_menu_group, Mask, Toggle}, State=#state{frame=Frame, menu_ets=MenuEts}) ->
  MenuBar = wxFrame:getMenuBar(Frame),
  ets:foldl(
  fun({_Id,_Options}, DontCare) ->
    DontCare;
  ({Id,_,Options}, DontCare) ->
    case proplists:get_value(group, Options) of
      undefined -> ok;
      Groups ->
        toggle_menu_item(MenuBar, wxFrame:getToolBar(Frame), Mask, Id, Groups, Toggle)
    end,
    DontCare
  end, notused, MenuEts),
	{noreply, State};
handle_cast({title, Title}, State=#state{frame=Frame}) ->
	Str = case Title of
		[] -> ?FRAME_TITLE;
		_ -> Title ++ " - " ++ ?FRAME_TITLE
	end,
	wxFrame:setTitle(Frame, Str),
  {noreply, State};
handle_cast({output_display, Window}, State=#state{splitter_output_pos=Pos}) ->
  Id = case Window of
    output -> ?WINDOW_OUTPUT;
    log -> ?WINDOW_LOG
  end,
  Splitter = wx:typeCast(wxWindow:findWindowById(?SPLIITER_LOG), wxSplitterWindow),
  Win = wxWindow:findWindowById(Id),
  replace_output_window(Splitter, Win, Pos),
  {noreply, State}.

%% @hidden  
code_change(_, _, State) ->
  {ok, State}.

%% @hidden
terminate(_Reason, #state{frame=Frame}) ->
  wxFrame:destroy(Frame),
  wx:destroy().

%% =====================================================================
%% Event handlers
%%
%% =====================================================================

%% Window close event
handle_event(#wx{event=#wxClose{}}, State) ->
  case ide_doc_man_wx:close_all() of
    cancelled ->
      {noreply, State};
    _ ->
      {stop, normal, State}
  end;

%% =====================================================================
%% Sash drag handlers
%%
%% =====================================================================

handle_event(#wx{id=?SPLITTER_SIDEBAR, event=#wxSplitter{type=command_splitter_sash_pos_changed}}, State) ->
  % Pos = wxSplitterWindow:getSashPosition(State#state.splitter_sidebar),
  %% Don't save the pos if the sash is dragged to zero, as the sash
  %% will revert to the middle when shown again (on mac definitely,
  %% probably on all platforms, THIS SHOULD BE CHECKED AGAIN on R16B01 and wxWidgets 2.9.4)
  Pos = case wxSplitterWindow:getSashPosition(State#state.splitter_sidebar) of
    0 ->
      State#state.splitter_sidebar_pos;
    N ->
      N
  end,
  {noreply, State#state{splitter_sidebar_pos=Pos}};

handle_event(#wx{id=?SPLITTER_UTILITIES, event=#wxSplitter{type=command_splitter_sash_pos_changed}}, State) ->
  Pos = case wxSplitterWindow:getSashPosition(State#state.splitter_utilities) of
    0 ->
      State#state.splitter_utilities_pos;
    N ->
      N
  end,
  {noreply, State#state{splitter_utilities_pos=Pos}};

handle_event(#wx{id=?SPLIITER_LOG, event=#wxSplitter{type=command_splitter_sash_pos_changed}}, State=#state{frame=Frame}) ->
  Pos = wxSplitterWindow:getSashPosition(wx:typeCast(wxWindow:findWindow(Frame, ?SPLIITER_LOG), wxSplitterWindow)),
  {noreply, State#state{splitter_output_pos=Pos}};
  
handle_event(#wx{event=#wxSplitter{type=command_splitter_doubleclicked}}, State) ->
  {noreply, State};


%% =====================================================================
%% Menu handlers
%%
%% =====================================================================
handle_event(#wx{id=?MENU_ID_SEARCH_DOC}, State) ->
  wx_misc:launchDefaultBrowser("http://www.erlang.org/erldoc"),
  {noreply, State};
  
handle_event(#wx{id=?wxID_EXIT}, State) ->
  case ide_doc_man_wx:close_all() of
    cancelled ->
      {noreply, State};
    _ ->
      {stop, normal, State}
  end;
  
handle_event(#wx{id=?MENU_ID_AUTO_INDENT=Id}, State=#state{frame=Frame}) ->
  Bool = wxMenuItem:isChecked(wxMenuBar:findItem(wxFrame:getMenuBar(Frame), Id)),
  ide_sys_pref_gen:set_preference(auto_indent, Bool),
  {noreply, State};
  
handle_event(#wx{id=Id}, State=#state{left_pane=LeftPane}) 
    when (Id >= ?MENU_ID_PROJECTS_WINDOW) and (Id =< ?MENU_ID_FUNC_WINDOW) ->
  Idx = case Id of
    ?MENU_ID_PROJECTS_WINDOW -> 1;
    ?MENU_ID_TESTS_WINDOW -> 2;
    ?MENU_ID_FUNC_WINDOW -> 3
  end,
  ide_tabbed_win_img_wx:set_selection(LeftPane, Idx), %% Default to projects
  {noreply, State};

handle_event(#wx{id=Id}, State=#state{util_tabbed=Utils}) 
    when (Id >= ?MENU_ID_CONSOLE_WINDOW) and (Id =< ?MENU_ID_DEBUGGER_WINDOW) ->
  Idx = case Id of
    ?MENU_ID_CONSOLE_WINDOW -> 1;
    ?MENU_ID_DIALYSER_WINDOW -> 2;
    ?MENU_ID_DEBUGGER_WINDOW -> 3
  end,
  ide_tabbed_win_wx:set_selection(Utils, Idx), %% Default to projects
  {noreply, State};
  
%% Handle copy/paste
% Currently on MSW and Linux the menu event is not caught by the in focus control even when
% it has a registered handler for it. On OSX the event is caught as expected (and will 
% propagate up through the hierarchy). Not sure which of
% these behaviours is correct. Due to this we have to catch the copy/paste/cut etc. menu
% events here, find out which control was in focus and then execute the correct function on it.
% We have tried but failed to find an wxErlang alternative to the wxWidget 
% wxEventHandler->proccess_event() function that would allow us to pass the event to the in-focus 
% control, and so this is the workaround we have adopted.
handle_event(#wx{id=?wxID_PASTE}, State) ->
  Fw = wxWindow:findFocus(),
  Id = wxWindow:getId(Fw),
  case Id of
    ?WINDOW_CONSOLE -> 
      ide_console_wx:paste(wx:typeCast(Fw, wxStyledTextCtrl));
    Tc when Tc =:= ?WINDOW_OUTPUT; Tc =:= ?WINDOW_FUNCTION_SEARCH ->
       wxTextCtrl:paste(wx:typeCast(Fw, wxTextCtrl));
    _ -> 
      wxStyledTextCtrl:paste(wx:typeCast(Fw, wxStyledTextCtrl))
  end,
  {noreply, State};
handle_event(#wx{id=?wxID_COPY}, State) ->
  Fw = wxWindow:findFocus(),
  Id = wxWindow:getId(Fw),
  case Id of
    Tc when Tc =:= ?WINDOW_OUTPUT; Tc =:= ?WINDOW_FUNCTION_SEARCH ->
      wxTextCtrl:copy(wx:typeCast(Fw, wxTextCtrl));
    _ -> wxStyledTextCtrl:copy(wx:typeCast(Fw, wxStyledTextCtrl))
  end,
  {noreply, State};
%% First handle the sub-menus
handle_event(#wx{userData={theme_menu,Menu}, event=#wxCommand{type=command_menu_selected}},
             State) ->
	ide_editor_ops:set_theme(Menu),
	{noreply, State};

handle_event(#wx{id=Id, userData=Menu, event=#wxCommand{type=command_menu_selected}},
             State) when Id >= ?MENU_ID_TAB_WIDTH_LOWEST,
						 Id =< ?MENU_ID_TAB_WIDTH_HIGHEST  ->
  ide_doc_man_wx:apply_to_all_documents(fun ide_editor_wx:set_tab_width/2, [list_to_integer(wxMenu:getLabel(Menu, Id))]),
  ide_sys_pref_gen:set_preference(tab_width, wxMenu:getLabel(Menu, Id)),
	{noreply, State};
handle_event(#wx{event=#wxCommand{type=command_menu_selected},id=?MENU_ID_FULLSCREEN=Id},
						 State=#state{frame=Frame}) ->
	IsFullScreen = wxFrame:isFullScreen(Frame),
	wxFrame:showFullScreen(Frame, not IsFullScreen, [{style, ?wxFULLSCREEN_NOBORDER}]),
	Label = case IsFullScreen of
		true -> "Enter Fullscreen";
		false -> "Exit Fullscreen"
	end,
	ide_menu:update_label(wxFrame:getMenuBar(Frame), Id, Label ++ "\tCtrl+Alt+F"),
	{noreply, State};	
handle_event(#wx{event=#wxCommand{type=command_menu_selected},id=?MENU_ID_HIDE_TEST},
						 State=#state{splitter_sidebar=V, left_pane=LeftPane, workspace=Ws, splitter_sidebar_pos=VPos}) ->
   wxWindow:freeze(V),
   case wxSplitterWindow:isSplit(V) of
       true -> wxSplitterWindow:unsplit(V,[{toRemove, LeftPane}]);
       false -> wxSplitterWindow:splitVertically(V, LeftPane, Ws, [{sashPosition, VPos}])
   end,
   wxWindow:thaw(V),
	{noreply, State};
handle_event(#wx{event=#wxCommand{type=command_menu_selected},id=?MENU_ID_HIDE_UTIL},
						 State=#state{splitter_utilities=H, splitter_sidebar=V, utilities=Utils, splitter_utilities_pos=HPos}) ->
	IsShown = wxSplitterWindow:isShown(Utils),
	case wxSplitterWindow:isSplit(H) of
		true -> ok;
		false ->
			wxSplitterWindow:splitHorizontally(H, V, Utils, [{sashPosition, HPos}])
	end,
	case IsShown of
		true ->
			wxSplitterWindow:unsplit(H,[{toRemove, Utils}]);
		false -> ok
	end,
	{noreply, State};
handle_event(#wx{event=#wxCommand{type=command_menu_selected},id=?MENU_ID_MAX_EDITOR},
						 State=#state{splitter_utilities=H, splitter_sidebar=V, utilities=Utils, left_pane=LeftPane,
						 							splitter_utilities_pos=HPos, splitter_sidebar_pos=VPos, workspace=Ws, frame=Frame}) ->
  wxFrame:freeze(Frame),
	Fun = fun(false, false, false) -> %% Restore
		wxSplitterWindow:splitHorizontally(H, V, Utils, [{sashPosition, HPos}]),
		wxSplitterWindow:splitVertically(V, LeftPane, Ws, [{sashPosition, VPos}]);
	(false, _, true) ->
		wxSplitterWindow:splitHorizontally(H, V, Utils, [{sashPosition, HPos}]),
		maximise;
	(_,_,_) -> maximise %% Unsplit both, even if already unsplit
	end,
	case Fun(wxSplitterWindow:isSplit(H), wxSplitterWindow:isSplit(V), wxSplitterWindow:isShown(Utils)) of
		maximise -> 
			wxSplitterWindow:unsplit(H,[{toRemove, Utils}]),
			wxSplitterWindow:unsplit(V,[{toRemove, LeftPane}]);
		_ -> ok
	end,
  wxFrame:thaw(Frame),
	{noreply, State};
handle_event(#wx{event=#wxCommand{type=command_menu_selected},id=?MENU_ID_MAX_UTIL},
						 State=#state{frame=Frame, splitter_utilities=H, splitter_sidebar=V, utilities=Utils,
						 							splitter_utilities_pos=HPos}) ->
  wxFrame:freeze(Frame),
	IsSplit = wxSplitterWindow:isSplit(H),
	IsShown = wxSplitterWindow:isShown(Utils),
	case IsSplit of
		false -> wxSplitterWindow:splitHorizontally(H, V, Utils, [{sashPosition, HPos}]);
		true -> ok
	end,
	case IsSplit =:= IsShown of
		true -> wxSplitterWindow:unsplit(H,[{toRemove, V}]);
		false -> ok
	end,
  wxFrame:thaw(Frame),
	{noreply, State};
%% The menu items from the ETS table
handle_event(E=#wx{id=Id, userData={ets_table, TabId}, event=#wxCommand{type=command_menu_selected}},
             State) ->
	Result = case ets:lookup(TabId, Id) of
		[{_MenuItemID, {Mod, Func, Args}, Options}] ->
			case proplists:get_value(send_event, Options) of
				undefined -> {ok, {Mod,Func,Args}};
				true -> {ok, {Mod,Func,[E]}}
			end;
		[{_,{Mod,Func,Args}}] ->
			{ok, {Mod,Func,Args}};
		_ -> nomatch
	end,
	case Result of
		{ok,{M,F,A}} -> 
			erlang:apply(M,F,A);
		nomatch -> 
			io:format("Not yet implemented~n")
	end,
  {noreply, State};

%% =====================================================================
%% Other handlers
%%
%% =====================================================================

% Output windows (log, output etc.)
handle_event(#wx{userData={Splitter, Window}, event=#wxCommand{type=command_button_clicked}}, 
             State=#state{splitter_output_pos=Pos}) ->
  replace_output_window(Splitter, Window, Pos),
  {noreply, State};
handle_event(#wx{id=?BUTTON_HIDE_OUTPUT=Id, userData=Splitter, event=#wxCommand{type=command_button_clicked}}, 
             State=#state{splitter_output_active=Aw, splitter_output_pos=Pos}) ->
  Window2 = wxSplitterWindow:getWindow2(Splitter),
  Bmp = case wxSplitterWindow:isSplit(Splitter) of
    true ->
      wxSplitterWindow:unsplit(Splitter),
      wxArtProvider:getBitmap("wxART_GO_DOWN", [{size, {16,16}}]);
    _ ->
      wxSplitterWindow:splitVertically(Splitter, wxSplitterWindow:getWindow1(Splitter), Aw, [{sashPosition, Pos}]),
      wxArtProvider:getBitmap("wxART_GO_BACK", [{size, {16,16}}])
  end,
  Btn = wx:typeCast(wxWindow:findWindowById(Id), wxBitmapButton),
  wxBitmapButton:setBitmapLabel(Btn, Bmp),
  {noreply, State#state{splitter_output_active=Window2}};
     
%% Event catchall for testing
handle_event(Ev, State) ->
  io:format("IDE event catchall: ~p\n", [Ev]),
  {noreply, State}.
    
  
%% =====================================================================
%% Internal functions
%% =====================================================================

%% =====================================================================
%% @doc Create the utilities panel
%% @private

-spec create_utils(Parent) -> {UtilWin, TabWin, OutWin} when
	Parent :: wxWindow:wxWindow(),
	UtilWin :: wxPanel:wxPanel(),
  TabWin :: ide_tabbed_win_wx:ide_tabbed_win_wx(),
  OutWin :: wxPanel:wxPanel().

create_utils(ParentA) ->
  Parent = wxPanel:new(ParentA),
  Sz = wxBoxSizer:new(?wxHORIZONTAL),
  wxPanel:setSizer(Parent, Sz),
     
 	SplitterStyle = case os:type() of
 		{_, darwin} -> ?wxSP_3DSASH bor ?wxSP_LIVE_UPDATE;
 		_ -> ?wxSP_3DSASH
 	end,
	Splitter = wxSplitterWindow:new(Parent, [{id, ?SPLIITER_LOG}, {style, SplitterStyle}]),
  wxSplitterWindow:setSashGravity(Splitter, 0.5),
  
  %% Splitter window 1
	TabbedWindow = ide_tabbed_win_wx:new([{parent, Splitter}]),
	
	%% Start the port that communicates with the external ERTs
	Console = case ide_console_sup:start_link([]) of
		{error, _E} ->
			ide_lib_widgets:placeholder(TabbedWindow, "Oops, the console could not be loaded.", [{fgColour, ?wxRED}]);
			%% Disable console menu/toolbar items
		_Port ->
			ide_console_wx:new([{parent, TabbedWindow}])
	end,
	ide_tabbed_win_wx:add_page(TabbedWindow, Console, "Console"),

  % Observer = ide_observer:start([{parent, TabbedWindow}]),
  % ide_tabbed_win_wx:add_page(TabbedWindow, Observer, "Observer"),
  
  Dialyser = ide_lib_widgets:placeholder(TabbedWindow, "Not Implemented"),
	ide_tabbed_win_wx:add_page(TabbedWindow, Dialyser, "Dialyser"),
	
  Debugger = ide_lib_widgets:placeholder(TabbedWindow, "Not Implemented"),
	ide_tabbed_win_wx:add_page(TabbedWindow, Debugger, "Debugger"),
	
	ide_tabbed_win_wx:set_selection(TabbedWindow, 1),
  
  %% Splitter window 2
  
  CreateWindow = fun(P, WindowModule, Id) ->
    W = wxPanel:new(P, [{winid, Id}]),
    WSz = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:add(WSz, WindowModule:new([{parent, W}]), [{flag, ?wxEXPAND}, {proportion, 1}]),  
    wxPanel:setSizer(W, WSz),
    wxPanel:hide(W),
    W
  end,

  Log = CreateWindow(Splitter, ide_log_out_wx, ?WINDOW_LOG),
  CompilerOutput = CreateWindow(Splitter, ide_compiler_out_wx, ?WINDOW_OUTPUT),
  
  wxSplitterWindow:splitVertically(Splitter, TabbedWindow, Log, [{sashPosition, ?SPLITTER_LOG_SASH_POS_DEFAULT}]),
  wxSizer:add(Sz, Splitter, [{flag, ?wxEXPAND}, {proportion, 1}]),

  %% Button toolbar
  ToolBar = wxPanel:new(Parent),
  ToolBarSz = wxBoxSizer:new(?wxVERTICAL),
  wxPanel:setSizer(ToolBar, ToolBarSz),
  
  % ButtonFlags = [{style, ?wxBORDER_SIMPLE}],
  ButtonFlags = [{style, ?wxBORDER_NONE}],
  % ButtonFlags = [],
  Button1 = wxBitmapButton:new(ToolBar, ?BUTTON_LOG, wxArtProvider:getBitmap("wxART_FIND", [{size, {16,16}}]), ButtonFlags),
  Button2 = wxBitmapButton:new(ToolBar, ?BUTTON_COMPILER_OUTPUT, wxArtProvider:getBitmap("wxART_WARNING", [{size, {16,16}}]), ButtonFlags),
  Button3 = wxBitmapButton:new(ToolBar, ?BUTTON_HIDE_OUTPUT, wxArtProvider:getBitmap("wxART_GO_BACK", [{size, {16,16}}]), ButtonFlags),
  
  %% Connect button handlers
  wxPanel:connect(Button1, command_button_clicked, [{userData, {Splitter, Log}}]),
  wxPanel:connect(Button2, command_button_clicked, [{userData, {Splitter, CompilerOutput}}]),
  wxPanel:connect(Button3, command_button_clicked, [{userData, Splitter}]),
  
  SzFlags = [{border, 3}, {flag, ?wxALL}],
  wxSizer:add(ToolBarSz, Button1, SzFlags),
  wxSizer:add(ToolBarSz, Button2, SzFlags),
  
  wxSizer:addStretchSpacer(ToolBarSz),
  wxSizer:add(ToolBarSz, Button3, SzFlags),
    
  wxSizer:add(Sz, ToolBar, [{flag, ?wxEXPAND}, {proportion, 0}]),
  
  ide_log_out_wx:message("Application started."),
  
	{Parent, TabbedWindow, Log}.


%% =====================================================================
%% @doc Create the left window, and its child components.

-spec create_left_window(wxFrame:wxFrame(), wxWindow:wxWindow()) -> ide_tabbed_win_img_wx:ide_tabbed_win_img_wx().

create_left_window(Frame, Parent) ->
	ImgList = wxImageList:new(16,16),
	wxImageList:add(ImgList, wxBitmap:new(wxImage:new(ide_lib_widgets:rc_dir("books-stack.png")))),
	wxImageList:add(ImgList, wxBitmap:new(wxImage:new(ide_lib_widgets:rc_dir("clipboard-task.png")))),
	wxImageList:add(ImgList, wxBitmap:new(wxImage:new(ide_lib_widgets:rc_dir("function.png")))),
	
	Toolbook = ide_tabbed_win_img_wx:new([{parent, Parent}]),
	ide_tabbed_win_img_wx:assign_image_list(Toolbook, ImgList),

	ProjectTrees = ide_proj_tree_wx:start([{parent, Toolbook}, {frame, Frame}]),
	ide_tabbed_win_img_wx:add_page(Toolbook, ProjectTrees, "Browser", [{imageId, 0}]),
	
  TestPanel = ide_lib_widgets:placeholder(Toolbook, "No Tests"),
	ide_tabbed_win_img_wx:add_page(Toolbook, TestPanel, " Tests ", [{imageId, 1}]),
	
	FunctionsPanel = ide_sl_wx:start([{parent, Toolbook}]),
	ide_tabbed_win_img_wx:add_page(Toolbook, FunctionsPanel, "Functions", [{imageId, 2}]),
	
	ide_tabbed_win_img_wx:set_selection(Toolbook, 1), %% Default to projects
	Toolbook.


%% =====================================================================
%% @doc Create the editor workspace.

-spec create_workspace(wxWindow:wxWindow()) -> wx_object:wx_object().

create_workspace(Parent) ->
	ide_doc_man_wx:start([{parent, Parent}]).


%% =====================================================================
%% @doc Enable/disable a menu item.
%% For the menu item identified by Id, Groups is compared to Mask using bitwise AND. 
%% If the item has the group bit set then it will be enabled when
%% Enabled is set to true, and disabled when false.
%% @see toggle_menu_group
%% @private

-spec toggle_menu_item(MenuBar, ToolBar, Mask, Id, Groups, Enable) -> ok when
	MenuBar :: wxMenuBar:wxMenuBar(),
  ToolBar :: wxToolBar:wxToolBar(),
	Mask :: integer(), % Defines which groups to affect
	Id :: integer(),	% The menu item id
	Groups :: integer(), % The groups associated to the menu item with Id
	Enable :: boolean().

toggle_menu_item(MenuBar, ToolBar, Mask, Id, Groups, Enable) ->
	case (Mask band Groups) of
		0 -> ok;
		_ ->
			wxMenuItem:enable(wxMenuBar:findItem(MenuBar, Id), [{enable, Enable}]),
      wxToolBar:enableTool(ToolBar, Id, Enable)
	end.


%% =====================================================================
%% @doc Change the currently shown window in the output window.
  
-spec replace_output_window(wxWindow:wxWindow(), wxWindow:wxWindow(), integer()) -> ok | boolean().

replace_output_window(Splitter, Window, Pos) ->
  case wxSplitterWindow:isSplit(Splitter) of
    true ->
      case wxWindow:isShown(Window) of
        true -> ok;
        false ->
          OldWindow = wxSplitterWindow:getWindow2(Splitter),
          wxSplitterWindow:replaceWindow(Splitter, OldWindow, Window),
          wxWindow:hide(OldWindow),
          wxWindow:show(Window)
      end;
    _ ->
      Window1 = wxSplitterWindow:getWindow1(Splitter),
      wxSplitterWindow:splitVertically(Splitter, Window1, Window, [{sashPosition, Pos}])
  end.