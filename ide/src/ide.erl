%% =====================================================================
%% @author
%% @copyright
%% @version
%% @doc This module is responsible for building all of the GUI components. 
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
         set_title/1,
         display_output_window/1,
         toggle_menu_group/2]).

%% Server state
-record(state, {frame,
                workspace :: wxAuiNotebook:wxAuiNotebook(),    %% Notebook
                utilities,                                     %% The utilities pane
                util_tabbed, %% tabbed_book
                left_pane,                                     %% The test pane
                workspace_manager,                             %% Tabbed UI manager for editors
                splitter_sidebar :: wxSplitterWindow:wxSplitterWindow(), %% The vertical splitter
                splitter_utilities :: wxSplitterWindow:wxSplitterWindow(), %% The horizontal splitter
                splitter_sidebar_pos :: integer(),
                splitter_utilities_pos :: integer(),
                splitter_log_pos :: integer(),
                splitter_log_active_window :: wxWindow:wxWindow(),
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
%% @doc Start the erlang IDE

start() ->
  start([]).

start(Args) ->
	wx_object:start({local, ?MODULE}, ?MODULE, Args, [{debug, [log]}]).


%% =====================================================================
%% @doc Update the frame's title.

set_title(Title) ->
	wx_object:cast(?MODULE, {title, Title}).


%% =====================================================================
%% @doc Enable/disable a menu group

toggle_menu_group(Mask, Toggle) ->
	wx_object:cast(?MODULE, {toggle_menu_group, Mask, Toggle}).


%% =====================================================================
%% @doc

display_output_window(Window) ->
  wx_object:cast(?MODULE, {output_display, Window}).		
  
  
%% =====================================================================
%% Callback functions
%% =====================================================================

init(Options) ->
	wx:new(Options),
	WxEnv = wx:get_env(),
	process_flag(trap_exit, true),
  
  %% Set small window variant globally
  % wxSystemOptions:setOption("window-default-variant", ?wxWINDOW_VARIANT_SMALL),

	Frame = wxFrame:new(wx:null(), ?wxID_ANY, "Erlang IDE", [{size,{?DEFAULT_FRAME_WIDTH,?DEFAULT_FRAME_HEIGHT}}]),
	wxFrame:connect(Frame, close_window),
	wxFrame:setMinSize(Frame, {300,200}),
	
	%% Load modules that should be started by OTP Application and not here
  sys_pref_manager:start([{wx_env, WxEnv}]),
  ProjDir = sys_pref_manager:get_preference(project_directory),
  case filelib:is_dir(ProjDir) of
    false ->
      file:make_dir(ProjDir);
    true ->
      ok
  end,
	project_manager:start([{frame, Frame}, {wx_env, WxEnv}]),

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
	StatusBar = ide_status_bar:start([{parent, Frame}]),

	%% Menubar %%
  {Menu, MenuEts} = ide_menu:create([{parent, Frame}]),

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
            splitter_log_pos=?SPLITTER_LOG_SASH_POS_DEFAULT,
            splitter_sidebar=SplitterSidebar,
            splitter_utilities=SplitterUtilities,
            splitter_log_active_window=ActiveLogWindow,
						workspace=Workspace,
            menu_ets=MenuEts
            }}.

%% Deal with trapped exit signals
handle_info({'EXIT',_, wx_deleted}, State) ->
  io:format("Got Info 1~n"),
  {noreply,State};
handle_info({'EXIT',_, shutdown}, State) ->
  io:format("Got Info 2~n"),
  {noreply,State};
handle_info({'EXIT',A, normal}, State) ->
  io:format("Got Info 3~n~p~n", [A]),
  {noreply,State};
handle_info(Msg, State) ->
  io:format("Got Info ~p~n",[Msg]),
  {noreply,State}.

handle_call(frame, _From, State) ->
	{reply, State#state.frame, State}.

handle_cast({toggle_menu_group, Mask, Toggle}, State=#state{frame=Frame, menu_ets=MenuEts}) ->
  MenuBar = wxFrame:getMenuBar(Frame),
  ets:foldl(
  fun({Id,_}, DontCare) ->
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
		T -> Title ++ " - " ++ ?FRAME_TITLE
	end,
	wxFrame:setTitle(Frame, Str),
  {noreply, State};
handle_cast({output_display, Window}, State=#state{frame=Frame, splitter_log_pos=Pos}) ->
  Id = case Window of
    output -> ?WINDOW_OUTPUT;
    log -> ?WINDOW_LOG
  end,
  Splitter = wx:typeCast(wxWindow:findWindowById(?SPLIITER_LOG), wxSplitterWindow),
  Win = wxWindow:findWindowById(Id),
  replaceOutputWindow(Splitter, Win, Pos),
  {noreply, State}.
  
code_change(_, _, State) ->
  {stop, not_yet_implemented, State}.

terminate(_Reason, #state{frame=Frame, workspace_manager=Manager}) ->
  wxFrame:destroy(Frame),
  wx:destroy().

%% =====================================================================
%% Event handlers
%%
%% =====================================================================

%% Window close event
handle_event(#wx{event=#wxClose{}}, State) ->
  case doc_manager:close_all() of
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
  {noreply, State#state{splitter_log_pos=Pos}};
  
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
  case doc_manager:close_all() of
    cancelled ->
      {noreply, State};
    _ ->
      {stop, normal, State}
  end;
  
handle_event(#wx{id=?MENU_ID_AUTO_INDENT=Id}, State=#state{frame=Frame}) ->
  Bool = wxMenuItem:isChecked(wxMenuBar:findItem(wxFrame:getMenuBar(Frame), Id)),
  sys_pref_manager:set_preference(auto_indent, Bool),
  {noreply, State};
  
handle_event(#wx{id=Id}, State=#state{frame=Frame, left_pane=LeftPane}) 
    when (Id >= ?MENU_ID_PROJECTS_WINDOW) and (Id =< ?MENU_ID_FUNC_WINDOW) ->
  Idx = case Id of
    ?MENU_ID_PROJECTS_WINDOW -> 1;
    ?MENU_ID_TESTS_WINDOW -> 2;
    ?MENU_ID_FUNC_WINDOW -> 3
  end,
  tabbed_book_img:set_selection(LeftPane, Idx), %% Default to projects
  {noreply, State};

handle_event(#wx{id=Id}, State=#state{frame=Frame, util_tabbed=Utils}) 
    when (Id >= ?MENU_ID_CONSOLE_WINDOW) and (Id =< ?MENU_ID_DEBUGGER_WINDOW) ->
  Idx = case Id of
    ?MENU_ID_CONSOLE_WINDOW -> 1;
    ?MENU_ID_DIALYSER_WINDOW -> 2;
    ?MENU_ID_DEBUGGER_WINDOW -> 3
  end,
  tabbed_book:set_selection(Utils, Idx), %% Default to projects
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
  Ctrl = case Id of
    ?WINDOW_CONSOLE -> 
      console_wx:paste(wx:typeCast(Fw, wxStyledTextCtrl));
    Tc when Tc =:= ?WINDOW_OUTPUT; Tc =:= ?WINDOW_FUNCTION_SEARCH ->
       wxTextCtrl:paste(wx:typeCast(Fw, wxTextCtrl));
    Else -> 
      wxStyledTextCtrl:paste(wx:typeCast(Fw, wxStyledTextCtrl))
  end,
  {noreply, State};
handle_event(#wx{id=?wxID_COPY}, State) ->
  Fw = wxWindow:findFocus(),
  Id = wxWindow:getId(Fw),
  case Id of
    Tc when Tc =:= ?WINDOW_OUTPUT; Tc =:= ?WINDOW_FUNCTION_SEARCH ->
      wxTextCtrl:copy(wx:typeCast(Fw, wxTextCtrl));
    Else -> wxStyledTextCtrl:copy(wx:typeCast(Fw, wxStyledTextCtrl))
  end,
  {noreply, State};
%% First handle the sub-menus
handle_event(E=#wx{id=Id, userData={theme_menu,Menu}, event=#wxCommand{type=command_menu_selected}},
             State) ->
	editor_ops:set_theme(Menu),
	{noreply, State};

handle_event(E=#wx{id=Id, userData=Menu, event=#wxCommand{type=command_menu_selected}},
             State) when Id >= ?MENU_ID_TAB_WIDTH_LOWEST,
						 Id =< ?MENU_ID_TAB_WIDTH_HIGHEST  ->
  doc_manager:apply_to_all_documents(fun editor:set_tab_width/2, [list_to_integer(wxMenu:getLabel(Menu, Id))]),
  sys_pref_manager:set_preference(tab_width, wxMenu:getLabel(Menu, Id)),
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
handle_event(#wx{event=#wxCommand{type=command_menu_selected},id=?MENU_ID_HIDE_TEST=Id},
						 State=#state{frame=Frame, splitter_sidebar=V, left_pane=LeftPane, workspace=Ws, splitter_sidebar_pos=VPos}) ->
   wxWindow:freeze(V),
   case wxSplitterWindow:isSplit(V) of
       true -> wxSplitterWindow:unsplit(V,[{toRemove, LeftPane}]);
       false -> wxSplitterWindow:splitVertically(V, LeftPane, Ws, [{sashPosition, VPos}])
   end,
   wxWindow:thaw(V),
	{noreply, State};
handle_event(#wx{event=#wxCommand{type=command_menu_selected},id=?MENU_ID_HIDE_UTIL=Id},
						 State=#state{frame=Frame, splitter_utilities=H, splitter_sidebar=V, utilities=Utils, splitter_utilities_pos=HPos}) ->
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
						 State=#state{frame=Frame, splitter_utilities=H, splitter_sidebar=V, utilities=Utils, left_pane=LeftPane,
						 							splitter_utilities_pos=HPos, splitter_sidebar_pos=VPos, workspace=Ws}) ->
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
             State=#state{frame=Frame}) ->
	Result = case ets:lookup(TabId, Id) of
		[{MenuItemID, {Mod, Func, Args}, Options}] ->
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
handle_event(#wx{obj=Button, userData={Splitter, Window}, event=#wxCommand{type=command_button_clicked}}, 
             State=#state{splitter_log_pos=Pos}) ->
  replaceOutputWindow(Splitter, Window, Pos),
  {noreply, State};
handle_event(#wx{id=?BUTTON_HIDE_OUTPUT, userData=Splitter, event=#wxCommand{type=command_button_clicked}}, State) ->
  Window2 = wxSplitterWindow:getWindow2(Splitter),
  wxSplitterWindow:unsplit(Splitter, [{toRemove, Window2}]),
  {noreply, State};
     
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

-spec create_utils(Parent) -> Result when
	Parent :: wxWindow:wxWindow(),
	Result :: wxPanel:wxPanel().

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
	TabbedWindow = tabbed_book:new([{parent, Splitter}]),
	
	%% Start the port that communicates with the external ERTs
	Console = case console_sup:start_link([]) of
		{error, E} ->
			lib_widgets:placeholder(TabbedWindow, "Oops, the console could not be loaded.", [{fgColour, ?wxRED}]);
			%% Disable console menu/toolbar items
		Port ->
			console_wx:new([{parent, TabbedWindow}])
	end,
	tabbed_book:add_page(TabbedWindow, Console, "Console"),

  % Observer = ide_observer:start([{parent, TabbedWindow}]),
  % tabbed_book:add_page(TabbedWindow, Observer, "Observer"),
  
  % Dialyser = wxPanel:new(TabbedWindow, []),
  Dialyser = lib_widgets:placeholder(TabbedWindow, "Not Implemented"),
	tabbed_book:add_page(TabbedWindow, Dialyser, "Dialyser"),
	
  % Debugger = wxPanel:new(TabbedWindow, []),
  Debugger = lib_widgets:placeholder(TabbedWindow, "Not Implemented"),
	tabbed_book:add_page(TabbedWindow, Debugger, "Debugger"),
	
	tabbed_book:set_selection(TabbedWindow, 1),
  
  
  %% Splitter window 2
  CreateWindow = fun(P, WindowModule, Id) ->
    W = wxPanel:new(P, [{winid, Id}]),
    WSz = wxBoxSizer:new(?wxHORIZONTAL),
    %% Add toolbar
    Tb = wxPanel:new(W),
    wxWindow:setBackgroundColour(Tb, lib_widgets:colour_shade(wxSystemSettings:getColour(?wxSYS_COLOUR_WINDOW), 0.8)),
    TbSz = wxBoxSizer:new(?wxVERTICAL),
    Style = case os:type() of
      {_,darwin} -> []; %%[{style, ?wxBORDER_NONE}];
      _ -> [] %%[{style, ?wxBORDER_DEFAULT}]
    end,
    Btn = wxBitmapButton:new(Tb, ?BUTTON_HIDE_OUTPUT, wxBitmap:new(wxImage:new("../icons/10x10/137.png")), Style),
    wxSizer:add(TbSz, Btn, [{flag, ?wxALL}, {border, 0}]),
    wxPanel:setSizer(Tb, TbSz), 
    wxSizer:add(WSz, Tb, [{flag, ?wxEXPAND}]),
    wxSizer:add(WSz, WindowModule:new([{parent, W}]), [{flag, ?wxEXPAND}, {proportion, 1}]),  
    wxPanel:setSizer(W, WSz),
    wxPanel:hide(W),
    {W, Tb, TbSz}
  end,

  {Log, _, _} = CreateWindow(Splitter, log, ?WINDOW_LOG),
  {CompilerOutput, _, _} = CreateWindow(Splitter, compiler_output, ?WINDOW_OUTPUT),
  
  wxPanel:connect(Parent, command_button_clicked, [{userData, Splitter}]), %% Minimise button on each window
  wxSplitterWindow:splitVertically(Splitter, TabbedWindow, Log, [{sashPosition, ?SPLITTER_LOG_SASH_POS_DEFAULT}]),
  wxSizer:add(Sz, Splitter, [{flag, ?wxEXPAND}, {proportion, 1}]),

  %% Button toolbar
  ToolBar = wxPanel:new(Parent),
  ToolBarSz = wxBoxSizer:new(?wxVERTICAL),
  wxPanel:setSizer(ToolBar, ToolBarSz),
  
  % ButtonFlags = [{style, ?wxBORDER_SUNKEN}],
  ButtonFlags = [],  
  Button1 = wxBitmapButton:new(ToolBar, ?BUTTON_LOG, wxArtProvider:getBitmap("wxART_FIND", [{size, {16,16}}]), ButtonFlags),
  Button2 = wxBitmapButton:new(ToolBar, ?BUTTON_COMPILER_OUTPUT, wxArtProvider:getBitmap("wxART_WARNING", [{size, {16,16}}]), ButtonFlags),
  
  %% Connect button handlers
  wxPanel:connect(Button1, command_button_clicked, [{userData, {Splitter, Log}}]),
  wxPanel:connect(Button2, command_button_clicked, [{userData, {Splitter, CompilerOutput}}]),
  
  % SzFlags = [{flag, ?wxBOTTOM}, {border, 1}],
  SzFlags = [],
  wxSizer:add(ToolBarSz, Button1, SzFlags),
  wxSizer:add(ToolBarSz, Button2, SzFlags),
    
  wxSizer:add(Sz, ToolBar, [{flag, ?wxEXPAND}, {proportion, 0}]),
  
  log:message("Application started."),
  
	{Parent, TabbedWindow, Log}.


%% =====================================================================
%% @doc

create_left_window(Frame, Parent) ->
	ImgList = wxImageList:new(16,16),
	wxImageList:add(ImgList, wxBitmap:new(wxImage:new("../icons/books-stack.png"))),
	wxImageList:add(ImgList, wxBitmap:new(wxImage:new("../icons/clipboard-task.png"))),
	wxImageList:add(ImgList, wxBitmap:new(wxImage:new("../icons/function.png"))),
	
	Toolbook = tabbed_book_img:new([{parent, Parent}]),
	tabbed_book_img:assign_image_list(Toolbook, ImgList),

	ProjectTrees = ide_projects_tree:start([{parent, Toolbook}, {frame, Frame}]),
	tabbed_book_img:add_page(Toolbook, ProjectTrees, "Browser", [{imageId, 0}]),
	
  TestPanel = lib_widgets:placeholder(Toolbook, "No Tests"),
	tabbed_book_img:add_page(Toolbook, TestPanel, " Tests ", [{imageId, 1}]),
	
	FunctionsPanel = func_list:start([{parent, Toolbook}]),
	tabbed_book_img:add_page(Toolbook, FunctionsPanel, "Functions", [{imageId, 2}]),
	
	tabbed_book_img:set_selection(Toolbook, 1), %% Default to projects
	Toolbook.


%% =====================================================================
%% @doc

create_workspace(Parent) ->
	doc_manager:start([{parent, Parent}]).


%% =====================================================================
%% @doc Enable/disable a menu item
%% @private

-spec toggle_menu_item(MenuBar, ToolBar, Mask, Id, Groups, Enable) -> 'ok' when
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
%% @doc Change the current window in the output window.
  
replaceOutputWindow(Splitter, Window, Pos) ->
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
