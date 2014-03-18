%% =====================================================================
%% @author Tom Richmond <tr201@kent.ac.uk>
%% @author Mike Quested <mdq3@kent.ac.uk>
%% @copyright 2014 Tom Richmond, Mike Quested
%%
%% @version 1
%% @doc Starts an instance of the IDE. Builds all wx components.
%% @end
%% =====================================================================

-module(ide).

-include_lib("wx/include/wx.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("ide.hrl").

%% wx_object
-behaviour(wx_object).
-export([init/1, terminate/2, code_change/3, handle_info/2,
         handle_call/3, handle_cast/2, handle_event/2]).

%% API
-export([start/0,
         start/1,
         set_title/1,
         display_output_window/1,
         toggle_menu_group/2,
         toggle_menu_items/2,
         restart_console/0
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
                menu_groups :: {integer(), [integer()]},
                console_sup_pid :: pid()
                }).

%% Macros
-define(DEFAULT_UTIL_HEIGHT,  200).
-define(DEFAULT_TEST_WIDTH,   200).
-define(SPLITTER_SIDEBAR, 100).
-define(SPLITTER_UTILITIES, 101).
-define(SPLITTER_OUTPUT, 102).
-define(FRAME_TITLE, "Erlang IDE").
-define(BUTTON_HIDE_OUTPUT, 0).
-define(ID_TOGGLE_LOG, 1).
-define(ID_TOGGLE_OUTPUT, 2).

%% Dynamic bitmaps
-define(BITMAP_LOG_ON,  "log_on.png").
-define(BITMAP_LOG_OFF,  "log_off.png").
-define(BITMAP_OUTPUT_ON, "output_on.png").
-define(BITMAP_OUTPUT_OFF, "output_off.png").
-define(BITMAP_OUTPUT_SHOWN, "output_shown.png").
-define(BITMAP_OUTPUT_HIDDEN, "output_hidden.png").

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
%% Menu items can be associated to one or more menu groups (in ide_menu).
%% Takes a list of menu group ids (defined in ide.hrl), and enables
%% disables all menu items belonging to that group(s).

-spec toggle_menu_group([integer()], boolean()) -> ok.

toggle_menu_group(Groups, Toggle) ->
  wx_object:cast(?MODULE, {toggle_menu_group, Groups, Toggle}).


-spec toggle_menu_items([integer()], boolean()) -> ok.

toggle_menu_items(Items, Toggle) ->
  wx_object:cast(?MODULE, {toggle_menu_items, Items, Toggle}).


%% =====================================================================
%% @doc Show the window identified by WinId in the output window, hiding 
%%      the window currently displayed.
%% ?WINDOW_LOG or ?WINDOW_OUTPUT

-spec display_output_window(integer()) -> ok.

display_output_window(WinId) ->
  wx_object:cast(?MODULE, {output_display, WinId}).


%% =====================================================================
%% @doc Restart the console (remove the placeholder).

restart_console() ->
  wx_object:cast(?MODULE, restart_console).
  

%% =====================================================================
%% Callback functions
%% =====================================================================
%% @hidden
init(Options) ->
  Wx = wx:new(Options),
  WxEnv = wx:get_env(),
  process_flag(trap_exit, true),

  %% Initialise XRC
  Xrc = wxXmlResource:get(),
  wxXmlResource:initAllHandlers(Xrc),
  true = wxXmlResource:load(Xrc, ide_lib_widgets:rc_dir("dlgs.xrc")),

  %% Load modules that should be started by OTP Application and not here
  ide_sys_pref_gen:start([{wx_env, WxEnv}]),

  %% UI saved pref
  UIPrefs = ide_sys_pref_gen:get_preference(ui_prefs),

  Frame = wxFrame:new(Wx, ?wxID_ANY, ?FRAME_TITLE, [{size, UIPrefs#ui_prefs.frame_size}]),
  wxFrame:connect(Frame, close_window),
  wxFrame:setMinSize(Frame, {300,200}),

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
  %%wxSplitterWindow:setSashGravity(SplitterUtilities, 0.5),
  %%wxSplitterWindow:setSashGravity(SplitterSidebar, 0.5),
  wxSizer:add(FrameSizer, SplitterUtilities, [{flag, ?wxEXPAND}, {proportion, 1}]),

  %% Status bar
  StatusBar = ide_sb_wx:start([{parent, Frame}]),
  %% Menubar
  MenuGroups = ide_menu:create([{parent, Frame}]),
  wxSizer:add(FrameSizer, StatusBar, [{flag, ?wxEXPAND},{proportion, 0}]),
  Workspace = create_workspace(SplitterSidebar, Frame),

  %% The left window
  LeftWindow = create_left_window(Frame, SplitterSidebar),

  %% The bottom pane/utility window
  {Utilities, TabbedWindow, ActiveLogWindow, ConsoleSup} = create_utils(SplitterUtilities),

  wxSplitterWindow:splitVertically(SplitterSidebar, LeftWindow, Workspace,
                    [{sashPosition, UIPrefs#ui_prefs.sash_vert_1}]),

  wxSplitterWindow:splitHorizontally(SplitterUtilities, SplitterSidebar, Utilities,
                  [{sashPosition, UIPrefs#ui_prefs.sash_horiz}]),

  wxSizer:layout(FrameSizer),
  wxFrame:center(Frame),
  wxFrame:setSize(Frame, UIPrefs#ui_prefs.frame_size),
  %wxSplitterWindow:setSashPosition(SplitterUtilities, UIPrefs#ui_prefs.sash_horiz),
  %wxSplitterWindow:setSashPosition(SplitterSidebar, UIPrefs#ui_prefs.sash_vert_1),

  wxFrame:show(Frame),

  wxSplitterWindow:setSashGravity(SplitterUtilities, 1.0), % Only the top window grows on resize
  wxSplitterWindow:setSashGravity(SplitterSidebar, 0.0), % Only the right window grows

  wxSplitterWindow:connect(Frame, command_splitter_sash_pos_changed),
  wxSplitterWindow:connect(Frame, command_splitter_doubleclicked),

  % wxFrame:connect(Frame, size, [{skip, true}]),

  %% Testing accelerator table
  % AccelTab = wxAcceleratorTable:new(1,
  %   [wxAcceleratorEntry:new([{flags, ?wxACCEL_NORMAL}, {keyCode, ?WXK_SPACE}, {cmd, ?MENU_ID_FONT}])]),
  % wxFrame:setAcceleratorTable(Frame, AccelTab),

  %% Toggle menu defaults
  toggle_menu_group([?MENU_GROUP_NOTEBOOK_EMPTY,
                     ?MENU_GROUP_PROJECTS_EMPTY,
                     ?MENU_GROUP_TEXT,
                     ?MENU_GROUP_NOTEBOOK_KILL_FOCUS], false),

  {Frame, #state{
            frame=Frame,
            left_pane=LeftWindow,
            utilities=Utilities,
            util_tabbed=TabbedWindow,
            splitter_sidebar_pos=UIPrefs#ui_prefs.sash_vert_1,
            splitter_utilities_pos=UIPrefs#ui_prefs.sash_horiz,
            splitter_output_pos=UIPrefs#ui_prefs.sash_vert_2,
            splitter_sidebar=SplitterSidebar,
            splitter_utilities=SplitterUtilities,
            splitter_output_active=ActiveLogWindow,
            workspace=Workspace,
            menu_groups=MenuGroups,
            console_sup_pid=ConsoleSup
            }}.

%% Deal with trapped exit signals
%% Console supervisor has exited, replace the console window
handle_info({'EXIT', Pid, shutdown}, State=#state{console_sup_pid=Pid}) ->
  toggle_menu_group([?MENU_GROUP_ERL], false),
  Splitter = wx:typeCast(wxWindow:findWindowById(?SPLITTER_OUTPUT), wxSplitterWindow),
  Window1 = wxSplitterWindow:getWindow1(Splitter),
  wxSplitterWindow:replaceWindow(Splitter, Window1, 
    ide_lib_widgets:placeholder(Splitter, "Oops, the console could not be loaded.", [{fgColour, ?wxRED}])),
  ide_console_wx:destroy(),
  {noreply, State#state{console_sup_pid=undefined}};
handle_info({'EXIT', A, shutdown}, State) ->
  io:format("STATE: ~p~n", [State]),
  io:format("Got Info 2 ~p~n", [A]),
  {noreply,State};
handle_info({'EXIT', A, normal}, State) ->
  io:format("Got Info 3 ~p~n", [A]),
  {noreply,State};
  
handle_info(Msg, State) ->
  io:format("Got Info ~p~n",[Msg]),
  {noreply,State}.

%% @hidden
handle_call(frame, _From, State) ->
  {reply, State#state.frame, State}.


handle_cast(restart_console, State) ->
  Splitter = wx:typeCast(wxWindow:findWindowById(?SPLITTER_OUTPUT), wxSplitterWindow),
  case ide_console_sup:start_link([]) of
    {ok, Pid} ->
      Window1 = wxSplitterWindow:getWindow1(Splitter),
      wxSplitterWindow:replaceWindow(Splitter, Window1, ide_console_wx:new([{parent, Splitter}])),
      toggle_menu_group([?MENU_GROUP_ERL], true),
      {noreply, State#state{console_sup_pid=Pid}};
    _NotStarted ->
      {noreply, State}
  end;
  
handle_cast({toggle_menu_group, Groups, Toggle}, State) ->
  Mb = wxFrame:getMenuBar(State#state.frame),
  Tb = wxFrame:getToolBar(State#state.frame),
  wx:foreach(fun(Group) ->
    MenuIds0 = proplists:get_value(Group, State#state.menu_groups),
    MenuIds1 = case State#state.console_sup_pid of
      undefined when Group /= ?MENU_GROUP_ERL -> %% No console present, don't switch on any menu items in group MENU_GROUP_ERL.
        ErlMenus = proplists:get_value(?MENU_GROUP_ERL, State#state.menu_groups),
        MenuIds0 -- ErlMenus;
      _ -> 
        MenuIds0
    end,
    lists:map(fun(MenuId) ->
      toggle_menu_item(Mb, Tb, MenuId, Toggle)
    end, MenuIds1)
  end, Groups),
  {noreply, State};

handle_cast({toggle_menu_items, Items, Toggle}, State) ->
  Mb = wxFrame:getMenuBar(State#state.frame),
  Tb = wxFrame:getToolBar(State#state.frame),
  wx:foreach(fun(Item) ->
      toggle_menu_item(Mb, Tb, Item, Toggle)
  end, Items),
  {noreply, State};

handle_cast({title, Title}, State=#state{frame=Frame}) ->
  Str = case Title of
    [] -> ?FRAME_TITLE;
    _ -> Title ++ " - " ++ ?FRAME_TITLE
  end,
  wxFrame:setTitle(Frame, Str),
  {noreply, State};

handle_cast({output_display, Id}, State) ->
  Splitter = wx:typeCast(wxWindow:findWindowById(?SPLITTER_OUTPUT), wxSplitterWindow),
  replace_output_window(Splitter, wxWindow:findWindowById(Id),
    State#state.splitter_output_active, State#state.splitter_output_pos),
  {noreply, State}.

%% @hidden
code_change(_, _, State) ->
  {ok, State}.

%% @hidden
terminate(_Reason, #state{frame=Frame}) ->
  ide_doc_man_wx:destroy(),
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
      %% Save frame size
      Size = wxFrame:getSize(State#state.frame),
      Prefs = ide_sys_pref_gen:get_preference(ui_prefs),
      ide_sys_pref_gen:set_preference(ui_prefs, Prefs#ui_prefs{frame_size=Size}),
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
  Prefs = ide_sys_pref_gen:get_preference(ui_prefs),
  ide_sys_pref_gen:set_preference(ui_prefs, Prefs#ui_prefs{sash_vert_1=Pos}),
  {noreply, State#state{splitter_sidebar_pos=Pos}};

handle_event(#wx{id=?SPLITTER_UTILITIES, event=#wxSplitter{type=command_splitter_sash_pos_changed}}, State) ->
  Pos = case wxSplitterWindow:getSashPosition(State#state.splitter_utilities) of
    0 ->
      State#state.splitter_utilities_pos;
    N ->
      N
  end,
  Prefs = ide_sys_pref_gen:get_preference(ui_prefs),
  ide_sys_pref_gen:set_preference(ui_prefs, Prefs#ui_prefs{sash_horiz=Pos}),
  {noreply, State#state{splitter_utilities_pos=Pos}};

handle_event(#wx{id=?SPLITTER_OUTPUT, event=#wxSplitter{type=command_splitter_sash_pos_changed}}, State=#state{frame=Frame}) ->
  Pos = wxSplitterWindow:getSashPosition(wx:typeCast(wxWindow:findWindow(Frame, ?SPLITTER_OUTPUT), wxSplitterWindow)),
  Prefs = ide_sys_pref_gen:get_preference(ui_prefs),
  ide_sys_pref_gen:set_preference(ui_prefs, Prefs#ui_prefs{sash_vert_2=Pos}),
  {noreply, State#state{splitter_output_pos=Pos}};

handle_event(#wx{id=?SPLITTER_OUTPUT, event=#wxSplitter{type=command_splitter_unsplit}}, State) ->
  Win = wxWindow:findWindow(State#state.frame, ?WINDOW_CONSOLE),
  case wxWindow:isShown(Win) of
    true ->
      Btn = wx:typeCast(wxWindow:findWindowById(?BUTTON_HIDE_OUTPUT), wxBitmapButton),
      Bmp = wxBitmap:new(wxImage:new(ide_lib_widgets:rc_dir(?BITMAP_OUTPUT_HIDDEN))),
      wxBitmapButton:setBitmapLabel(Btn, Bmp);
    false ->
      ok
  end,
  {noreply, State};

handle_event(#wx{event=#wxSplitter{type=command_splitter_doubleclicked}}, State) ->
  {noreply, State};


%% =====================================================================
%% Other handlers
%%
%% =====================================================================

% Output windows (log, output etc.)
handle_event(#wx{userData={Splitter, Window}, event=#wxCommand{type=command_button_clicked}},
             State=#state{splitter_output_pos=Pos}) ->
  replace_output_window(Splitter, Window,
    State#state.splitter_output_active, Pos),
  Btn = wx:typeCast(wxWindow:findWindowById(?BUTTON_HIDE_OUTPUT), wxBitmapButton),
  Bmp = wxBitmap:new(wxImage:new(ide_lib_widgets:rc_dir(?BITMAP_OUTPUT_SHOWN))),
  wxBitmapButton:setBitmapLabel(Btn, Bmp),
  {noreply, State};

handle_event(#wx{userData=Splitter, event=#wxCommand{type=command_button_clicked}},
             State=#state{splitter_output_active=PrevOutput, splitter_output_pos=Pos}) ->
  CurrOutput = show_hide_output(Splitter, PrevOutput, Pos),
  {noreply, State#state{splitter_output_active=CurrOutput}};


%% =====================================================================
%% Menu handlers
%%
%% =====================================================================

handle_event(#wx{id=?wxID_NEW}, State) ->
  ide_doc_man_wx:new_document(State#state.frame),
  {noreply, State};

handle_event(#wx{id=?MENU_ID_NEW_PROJECT}, State) ->
  ide_proj_man:new_project(State#state.frame),
  {noreply, State};

handle_event(#wx{id=?MENU_ID_OPEN_PROJECT}, State) ->
  ide_proj_man:open_project_dialog(State#state.frame),
  {noreply, State};

handle_event(#wx{id=?wxID_OPEN}, State) ->
  ide_doc_man_wx:open_document_dialog(State#state.frame),
  {noreply, State};

handle_event(#wx{id=?wxID_SAVE}, State) ->
  ide_doc_man_wx:save_active_document(),
  {noreply, State};

handle_event(#wx{id=?wxID_SAVEAS}, State) ->
  ide_doc_man_wx:save_as(),
  {noreply, State};

handle_event(#wx{id=?MENU_ID_SAVE_ALL}, State) ->
  {noreply, State};

handle_event(#wx{id=?MENU_ID_SAVE_PROJECT}, State) ->
  ide_doc_man_wx:save_active_project(),
  {noreply, State};

handle_event(#wx{id=?wxID_PRINT}, State) ->
  {noreply, State};

handle_event(#wx{id=?wxID_CLOSE}, State) ->
  ide_doc_man_wx:close_active_document(),
  {noreply, State};

handle_event(#wx{id=?wxID_CLOSE_ALL}, State) ->
  ide_doc_man_wx:close_all(),
  {noreply, State};

handle_event(#wx{id=?MENU_ID_CLOSE_PROJECT}, State) ->
  ide_proj_man:close_active_project(),
  {noreply, State};

handle_event(#wx{id=?MENU_ID_IMPORT_FILE}, State) ->
  {noreply, State};

handle_event(#wx{id=?MENU_ID_IMPORT_PROJECT}, State) ->
  ide_proj_man:import(State#state.frame),
  {noreply, State};

handle_event(#wx{id=?wxID_UNDO}, State) -> %% only enabled when an editor is in focus
  wxStyledTextCtrl:undo(wx:typeCast(wxWindow:findFocus(), wxStyledTextCtrl)),
  {noreply, State};

handle_event(#wx{id=?wxID_REDO}, State) ->
  wxStyledTextCtrl:redo(wx:typeCast(wxWindow:findFocus(), wxStyledTextCtrl)),
  {noreply, State};

%% The following text functions use default handlers for wxTextCtrl,
%% (?wxID_CUT, ?wxID_COPY, ?wxID_PASTE, ?wxID_SELECTALL, ?wxID_DELETE), so only the
%% wxStyledTextCtrl functions (editor, console, log) need to be implemented manually. So the following will
%% only be called for stc's (and then only when one is enabled)
handle_event(#wx{id=?wxID_PASTE}, State) ->
  Fw = wxWindow:findFocus(),
  Id = wxWindow:getId(Fw),
  case Id of
    ?CONSOLE -> %% Special paste implementation
      ide_console_wx:paste(wx:typeCast(Fw, wxStyledTextCtrl));
    _ ->
      wxStyledTextCtrl:paste(wx:typeCast(Fw, wxStyledTextCtrl))
  end,
  {noreply, State};

handle_event(#wx{id=?wxID_COPY}, State) ->
  catch wxStyledTextCtrl:copy(wx:typeCast(wxWindow:findFocus(), wxStyledTextCtrl)),
  {noreply, State};

handle_event(#wx{id=?wxID_CUT}, State) ->
  catch wxStyledTextCtrl:cut(wx:typeCast(wxWindow:findFocus(), wxStyledTextCtrl)),
  {noreply, State};

handle_event(#wx{id=?wxID_DELETE}, State) ->
  catch wxStyledTextCtrl:clear(wx:typeCast(wxWindow:findFocus(), wxStyledTextCtrl)),
  {noreply, State};

handle_event(#wx{id=?wxID_SELECTALL}, State) ->
  catch wxStyledTextCtrl:selectAll(wx:typeCast(wxWindow:findFocus(), wxStyledTextCtrl)),
  {noreply, State};

handle_event(#wx{id=?MENU_ID_PROJECT_CONFIG}, State) ->
  ide_proj_man:set_project_configuration(State#state.frame),
  {noreply, State};

handle_event(#wx{id=?wxID_PREFERENCES}, State) ->
  ide_dlg_prefs_wx:start([{parent, State#state.frame}]),
  {noreply, State};

handle_event(#wx{id=?MENU_ID_QUICK_FIND}, State) ->
  ide_doc_man_wx:apply_to_active_document(fun ide_editor_wx:quick_find/1, []),
  {noreply, State};

handle_event(#wx{id=?wxID_FIND}, State) ->
  FindData = ide_dlg_data_find_wx:new(),
  ide_dlg_data_find_wx:set_options(FindData, ?IGNORE_CASE bor ?WHOLE_WORD bor ?START_WORD),
  ide_dlg_data_find_wx:set_search_location(FindData, ?FIND_LOC_DOC),
  case erlang:whereis(ide_dlg_find_wx) of
    undefined ->
      ide_dlg_find_wx:show(ide_dlg_find_wx:new(State#state.frame, FindData));
    Pid ->
      wxDialog:raise(ide_dlg_find_wx:get_ref(Pid))
  end,
  {noreply, State};

handle_event(#wx{id=?MENU_ID_FONT}, State) ->
  %% Display the system font picker
  FD = wxFontData:new(),
  Dialog = wxFontDialog:new(State#state.frame, FD),
  case wxDialog:showModal(Dialog) of
    ?wxID_OK ->
      %% Get the user selected font, and update the editors
      Font = wxFontData:getChosenFont(wxFontDialog:getFontData(Dialog)),
      ide_sys_pref_gen:set_font(editor_font, Font),
      ide_doc_man_wx:apply_to_all_documents(fun ide_editor_wx:set_font/2, [Font]),
      ok;
    ?wxID_CANCEL ->
        ok
  end,
  {noreply, State};

handle_event(#wx{id=?MENU_ID_FONT_BIGGER}, State) ->
  ide_doc_man_wx:apply_to_active_document(fun ide_editor_wx:zoom_in/1, []),
  {noreply, State};

handle_event(#wx{id=?MENU_ID_FONT_SMALLER}, State) ->
  ide_doc_man_wx:apply_to_active_document(fun ide_editor_wx:zoom_out/1, []),
  {noreply, State};

handle_event(#wx{id=?MENU_ID_LINE_WRAP}, State) ->
  Bool = wxMenuItem:isChecked(wxMenuBar:findItem(wxFrame:getMenuBar(State#state.frame), ?MENU_ID_LINE_WRAP)),
  N = if Bool -> 1;
         true -> 0
  end,
  ide_doc_man_wx:apply_to_all_documents(fun ide_editor_wx:set_line_wrap/2, [Bool]),
  ide_sys_pref_gen:set_preference(line_wrap, N),
  {noreply, State};

handle_event(#wx{id=?MENU_ID_LN_TOGGLE}, State) ->
  Bool = wxMenuItem:isChecked(wxMenuBar:findItem(wxFrame:getMenuBar(State#state.frame), ?MENU_ID_LN_TOGGLE)),
  ide_doc_man_wx:apply_to_all_documents(fun ide_editor_wx:set_line_margin_visible/2, [Bool]),
  ide_sys_pref_gen:set_preference(show_line_no, Bool),
  {noreply, State};

handle_event(#wx{id=Id}, State) when Id =:= ?MENU_ID_INDENT_SPACES orelse Id =:= ?MENU_ID_INDENT_TABS ->
  Cmd = case Id of
    ?MENU_ID_INDENT_SPACES -> false;
    ?MENU_ID_INDENT_TABS -> true
  end,
  ide_doc_man_wx:apply_to_all_documents(fun ide_editor_wx:set_use_tabs/2, [Cmd]),
  ide_sys_pref_gen:set_preference(use_tabs, Cmd),
  {noreply, State};

handle_event(#wx{id=?MENU_ID_INDENT_GUIDES}, State) ->
  Bool = wxMenuItem:isChecked(wxMenuBar:findItem(wxFrame:getMenuBar(State#state.frame), ?MENU_ID_INDENT_GUIDES)),
  ide_doc_man_wx:apply_to_all_documents(fun ide_editor_wx:set_indent_guides/2, [Bool]),
  ide_sys_pref_gen:set_preference(indent_guides, Bool),
  {noreply, State};

handle_event(#wx{id=?MENU_ID_INDENT_RIGHT}, State) ->
  ide_doc_man_wx:apply_to_active_document(fun ide_editor_wx:indent_right/1, []),
  {noreply, State};

handle_event(#wx{id=?MENU_ID_INDENT_LEFT}, State) ->
  ide_doc_man_wx:apply_to_active_document(fun ide_editor_wx:indent_left/1, []),
  {noreply, State};

handle_event(#wx{id=?MENU_ID_TOGGLE_COMMENT}, State) ->
  ide_doc_man_wx:apply_to_active_document(fun ide_editor_wx:comment/1, []),
  {noreply, State};

handle_event(#wx{id=?MENU_ID_GOTO_LINE}, State) ->
  %% Process the user input
  Process = fun(Input) ->
    {Ln1, Col1} = case string:tokens(Input, ":") of
      [] ->  {0, 0};
      [Ln0 | []] -> {Ln0, 0};
      [Ln0, Col0 | _ ] -> {Ln0, Col0}
    end,
    Ln2 = try
        list_to_integer(Ln1)
      catch _:_ -> 0
    end,
    Col2 = try
        list_to_integer(Col1)
      catch _:_ -> 0
    end,
    ide_doc_man_wx:apply_to_active_document(fun ide_editor_wx:go_to_position/2, [{Ln2,Col2}])
  end,
  %% Show the dialog
  [{Ln, Col} | _T] = ide_doc_man_wx:apply_to_active_document(fun ide_editor_wx:get_current_pos/1, []),
  Value = integer_to_list(Ln)++":"++integer_to_list(Col),
  Dlg = wxTextEntryDialog:new(State#state.frame, "Enter line:", [{caption, "Go to Line:"}, {value, Value}]),
  case wxTextEntryDialog:showModal(Dlg) of
    ?wxID_OK ->
      V = wxTextEntryDialog:getValue(Dlg),
      Process(V);
    _ ->
      ok
  end,
  wxTextEntryDialog:destroy(Dlg),
   {noreply, State};

handle_event(#wx{id=Id}, State) when Id =:= ?MENU_ID_UC_SEL orelse Id =:= ?MENU_ID_LC_SEL ->
  Cmd = case Id of
    ?MENU_ID_UC_SEL -> uppercase;
    ?MENU_ID_LC_SEL -> lowercase
  end,
  ide_doc_man_wx:apply_to_active_document(fun ide_editor_wx:transform_selection/2, [{transform, Cmd}]),
  {noreply, State};

handle_event(#wx{id=?MENU_ID_STRIP_SPACES}, State) ->
  ide_doc_man_wx:apply_to_active_document(fun ide_editor_wx:strip_trailing_whitespace/1, []),
  {noreply, State};

handle_event(#wx{id=?MENU_ID_COMPILE_FILE}, State) ->
  ide_build:compile_file(),
  {noreply, State};

handle_event(#wx{id=?MENU_ID_MAKE_PROJECT}, State) ->
  ide_build:make_project(),
  {noreply, State};

handle_event(#wx{id=?MENU_ID_RUN}, State) ->
  ide_build:run_project(State#state.frame),
  {noreply, State};

handle_event(#wx{id=?MENU_ID_RUN_TESTS}, State) ->
  case ide_build:compile_file() of
    {error, _} -> % Compilation failed
      ide_log_out_wx:error("ERROR: Tests couldn't be run."),
      display_output_window(?WINDOW_LOG);
    ok ->
      Mod = ide_doc_man_wx:get_active_module(),
      ide_testpane:add_module_tests(Mod),
      Listener = ide_eunit_listener:start([{wx_env, wx:get_env()}]),
      eunit:test(Mod, [{report, Listener}]),
      ide_tabbed_win_img_wx:set_selection(State#state.left_pane, 2)
  end,
  {noreply, State};

handle_event(#wx{id=?MENU_ID_RUN_OBSERVER}, State) ->
  ide_console_port_gen:eval("observer:start().\n", false),
  {noreply, State};

handle_event(#wx{id=?MENU_ID_RUN_DEBUGGER}, State) ->
  ide_console_port_gen:eval("debugger:start().\n", false),
  {noreply, State};

handle_event(#wx{id=?MENU_ID_DIALYZER}, State) ->
  ide_dialyzer:run(State#state.frame),
  {noreply, State};

handle_event(#wx{id=?MENU_ID_ADD_TO_PLT}, State) ->
  {noreply, State};

handle_event(#wx{id=?MENU_ID_PLT_INFO}, State) ->
  {noreply, State};

handle_event(#wx{id=?MENU_ID_DIAL_WARN}, State) ->
  {noreply, State};


handle_event(#wx{id=?MENU_ID_NEXT_TAB}, State) ->
  ide_doc_man_wx:set_selection(right),
  {noreply, State};

handle_event(#wx{id=?MENU_ID_PREV_TAB}, State) ->
  ide_doc_man_wx:set_selection(left),
  {noreply, State};

handle_event(#wx{id=?wxID_ABOUT}, State) ->
  Dlg = ide_dlg_about_wx:new(State#state.frame),
  wxDialog:showModal(Dlg),
  ide_dlg_about_wx:destroy(Dlg),
  {noreply, State};

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

handle_event(#wx{id=Id0}, State)
    when (Id0 >= ?MENU_ID_OUTPUT_WINDOW) and (Id0 =< ?MENU_ID_LOG_WINDOW) ->
  Id1 = case Id0 of
    ?MENU_ID_OUTPUT_WINDOW -> ?WINDOW_OUTPUT;
    ?MENU_ID_LOG_WINDOW -> ?WINDOW_LOG
  end,
  display_output_window(Id1),
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

handle_event(#wx{event=#wxCommand{type=command_menu_selected},id=?MENU_ID_HIDE_OUTPUT},
             State=#state{splitter_output_active=PrevOutput, splitter_output_pos=Pos}) ->
  CurrOutput = show_hide_output(wx:typeCast(wxWindow:findWindowById(?SPLITTER_OUTPUT), wxSplitterWindow), PrevOutput, Pos),
  {noreply, State#state{splitter_output_active=CurrOutput}};

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

%% Sub-menus
handle_event(#wx{id=Id, userData=ThemeMenu, event=#wxCommand{type=command_menu_selected}},
             State) when (Id >= ?MENU_ID_THEME_LOWEST) and (Id =< ?MENU_ID_THEME_HIGHEST) ->
  {ok, Ckd} = ide_menu:get_checked_menu_item(wxMenu:getMenuItems(ThemeMenu)),
  ide_doc_man_wx:apply_to_all_documents(fun ide_editor_wx:set_theme/3, [wxMenuItem:getLabel(Ckd),
    ide_sys_pref_gen:get_font(editor_font)]),
  ide_sys_pref_gen:set_preference(theme, wxMenuItem:getLabel(Ckd)),
  {noreply, State};

handle_event(#wx{id=Id, userData=Menu, event=#wxCommand{type=command_menu_selected}},
             State) when Id >= ?MENU_ID_TAB_WIDTH_LOWEST,
             Id =< ?MENU_ID_TAB_WIDTH_HIGHEST  ->
  ide_doc_man_wx:apply_to_all_documents(fun ide_editor_wx:set_tab_width/2, [list_to_integer(wxMenu:getLabel(Menu, Id))]),
  ide_sys_pref_gen:set_preference(tab_width, wxMenu:getLabel(Menu, Id)),
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

-spec create_utils(Parent) -> {UtilWin, TabWin, OutWin} when
  Parent :: wxWindow:wxWindow(),
  UtilWin :: wxPanel:wxPanel(),
  TabWin :: ide_tabbed_win_wx:ide_tabbed_win_wx(),
  OutWin :: wxPanel:wxPanel().

create_utils(ParentA) ->
  Parent = wxPanel:new(ParentA),
  Sz = wxBoxSizer:new(?wxHORIZONTAL),
  wxSizer:addSpacer(Sz, 6),
  wxPanel:setSizer(Parent, Sz),

  SplitterStyle = case os:type() of
    {_, darwin} -> ?wxSP_3DSASH bor ?wxSP_LIVE_UPDATE;
    _ -> ?wxSP_3DSASH
  end,
  Splitter = wxSplitterWindow:new(Parent, [{id, ?SPLITTER_OUTPUT}, {style, SplitterStyle}]),

  %% Splitter window 1
  %% Start the port that communicates with the external ERTs
  {Console, ConsoleSup} = case ide_console_sup:start_link([]) of
    {ok, Pid} ->
      {ide_console_wx:new([{parent, Splitter}]), Pid};
    _NotStarted ->
      toggle_menu_group([?MENU_GROUP_ERL], false),
      {ide_lib_widgets:placeholder(Splitter, "Oops, the console could not be loaded.", [{fgColour, ?wxRED}]), undefined}
  end,

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
  CompilerOutput = CreateWindow(Splitter, ide_stdout_wx, ?WINDOW_OUTPUT),

  UIPrefs = ide_sys_pref_gen:get_preference(ui_prefs),
  wxSplitterWindow:splitVertically(Splitter, Console, Log, [{sashPosition, UIPrefs#ui_prefs.sash_vert_2}]),
  wxSizer:add(Sz, Splitter, [{flag, ?wxEXPAND}, {proportion, 1}]),

  %% Button toolbar
  ToolBar = wxPanel:new(Parent),
  ToolBarSz = wxBoxSizer:new(?wxVERTICAL),
  wxPanel:setSizer(ToolBar, ToolBarSz),

  ButtonFlags = [{style, ?wxBORDER_NONE}],
  Button1 = wxBitmapButton:new(ToolBar, ?ID_TOGGLE_LOG,
    wxBitmap:new(wxImage:new(ide_lib_widgets:rc_dir("log_on.png"))), ButtonFlags),
  wxWindow:setToolTip(Button1, "Log"),
  Button2 = wxBitmapButton:new(ToolBar, ?ID_TOGGLE_OUTPUT,
    wxBitmap:new(wxImage:new(ide_lib_widgets:rc_dir("output_off.png"))), ButtonFlags),
  wxWindow:setToolTip(Button2, "Output"),
  Button3 = wxBitmapButton:new(ToolBar, ?BUTTON_HIDE_OUTPUT,
    wxBitmap:new(wxImage:new(ide_lib_widgets:rc_dir("output_shown.png"))), ButtonFlags),
  wxWindow:setToolTip(Button3, "Show/Hide"),

  %% Connect button handlers
  wxPanel:connect(Button1, command_button_clicked, [{userData, {Splitter, Log}}]),
  wxPanel:connect(Button2, command_button_clicked, [{userData, {Splitter, CompilerOutput}}]),
  wxPanel:connect(Button3, command_button_clicked, [{userData, Splitter}]),

  wxSplitterWindow:connect(Splitter, command_splitter_unsplit),

  SzFlags = [{border, 3}, {flag, ?wxALL}],
  wxSizer:add(ToolBarSz, Button1, SzFlags),
  wxSizer:add(ToolBarSz, Button2, SzFlags),
  wxSizer:addStretchSpacer(ToolBarSz),
  wxSizer:add(ToolBarSz, Button3, SzFlags),

  wxSizer:add(Sz, ToolBar, [{flag, ?wxEXPAND}, {proportion, 0}]),

  ide_log_out_wx:message("Application started."),

  {Parent, ok, Log, ConsoleSup}.


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

  TestPanel = ide_testpane:new([{parent, Toolbook}]),
  ide_tabbed_win_img_wx:add_page(Toolbook, TestPanel, " Tests ", [{imageId, 1}]),

  % FunctionsPanel = ide_sl_wx:start([{parent, Toolbook}]),
  % ide_tabbed_win_img_wx:add_page(Toolbook, FunctionsPanel, "Functions", [{imageId, 2}]),

  ide_tabbed_win_img_wx:set_selection(Toolbook, 1), %% Default to projects
  Toolbook.


%% =====================================================================
%% @doc Create the editor workspace.

-spec create_workspace(wxWindow:wxWindow(), wxWindow:wxWindow()) -> wx_object:wx_object().

create_workspace(Parent, Frame) ->
  ide_doc_man_wx:start([{parent, Parent}, {frame, Frame}]).


%% =====================================================================
%% @doc Enable/disable a menu item.
%% @private

-spec toggle_menu_item(MenuBar, ToolBar, ItemId, Enable) -> ok when
  MenuBar :: wxMenuBar:wxMenuBar(),
  ToolBar :: wxToolBar:wxToolBar(),
  ItemId :: integer(), % The menu item's id
  Enable :: boolean().

toggle_menu_item(MenuBar, ToolBar, ItemId, Enable) ->
  wxMenuItem:enable(wxMenuBar:findItem(MenuBar, ItemId), [{enable, Enable}]),
  wxToolBar:enableTool(ToolBar, ItemId, Enable).


%% =====================================================================
%% @doc Replace the the old output window with Window.

-spec replace_output_window(wxSplitterWindow:wxSplitterWindow(), wxWindow:wxWindow(),
  wxWindow:wxWindow(), integer()) -> ok | boolean().

replace_output_window(Splitter, Window, OldWindow, Pos) ->
  {Old, New} = case wxSplitterWindow:isSplit(Splitter) of
    true ->
      case wxWindow:isShown(Window) of
        true -> {ok, ok};
        false ->
          Window2 = wxSplitterWindow:getWindow2(Splitter),
          wxSplitterWindow:replaceWindow(Splitter, Window2, Window),
          wxWindow:hide(Window2),
          wxWindow:show(Window),
          {Window2, Window}
      end;
    _ ->
      Window1 = wxSplitterWindow:getWindow1(Splitter),
      case wxWindow:getId(Window1) of
        ?WINDOW_CONSOLE -> %% console currently maximised
          wxSplitterWindow:splitVertically(Splitter, Window1, Window, [{sashPosition, Pos}]),
          {OldWindow, Window};
        _ -> %% one of the output windows is maximised
          wxSplitterWindow:replaceWindow(Splitter, Window1, Window),
          wxWindow:hide(Window1),
          wxWindow:show(Window),
          {Window1, Window}
      end
  end,
  toggle_button(Old, New).


%% =====================================================================
%% @doc Toggle the output window.

-spec show_hide_output(wxSplitterWindow:wxSplitterWindow(), wxWindow:wxWindow(), integer()) ->
  wxWindow:wxWindow(). %% The new 'current' window

show_hide_output(Splitter, PrevOutput, Pos) ->
  Btn = wx:typeCast(wxWindow:findWindowById(?BUTTON_HIDE_OUTPUT), wxBitmapButton),
  case wxSplitterWindow:isSplit(Splitter) of
    true -> %% hide
      Window2 = wxSplitterWindow:getWindow2(Splitter),
      wxSplitterWindow:unsplit(Splitter),
      wxBitmapButton:setBitmapLabel(Btn, wxBitmap:new(wxImage:new(ide_lib_widgets:rc_dir(?BITMAP_OUTPUT_HIDDEN)))),
      Window2;
    _ ->
      Window1 = wxSplitterWindow:getWindow1(Splitter),
      case wxWindow:getId(Window1) of
        ?WINDOW_CONSOLE -> %% console currently maximised
          wxSplitterWindow:splitVertically(Splitter, Window1, PrevOutput, [{sashPosition, Pos}]),
          wxBitmapButton:setBitmapLabel(Btn, wxBitmap:new(wxImage:new(ide_lib_widgets:rc_dir(?BITMAP_OUTPUT_SHOWN))));
        _ -> %% one of the output windows is maximised, hide it
          Console = wxWindow:findWindowById(?WINDOW_CONSOLE),
          wxSplitterWindow:replaceWindow(Splitter, Window1, Console),
          wxWindow:show(Console),
          wxWindow:hide(Window1),
          wxBitmapButton:setBitmapLabel(Btn, wxBitmap:new(wxImage:new(ide_lib_widgets:rc_dir(?BITMAP_OUTPUT_HIDDEN))))
      end,
      PrevOutput
  end.


toggle_button(ok, ok) ->  ok;
toggle_button(Old, New) ->
  Win0 = wxWindow:getId(Old),
  Win1 = wxWindow:getId(New),
  Toggle = fun(Id, Bool) ->
    case Id of
      ?WINDOW_LOG when Bool ->
        {wx:typeCast(wxWindow:findWindowById(?ID_TOGGLE_LOG), wxBitmapButton), ?BITMAP_LOG_ON};
      ?WINDOW_LOG ->
        {wx:typeCast(wxWindow:findWindowById(?ID_TOGGLE_LOG), wxBitmapButton), ?BITMAP_LOG_OFF};
      ?WINDOW_OUTPUT when Bool ->
        {wx:typeCast(wxWindow:findWindowById(?ID_TOGGLE_OUTPUT), wxBitmapButton), ?BITMAP_OUTPUT_ON};
      ?WINDOW_OUTPUT ->
        {wx:typeCast(wxWindow:findWindowById(?ID_TOGGLE_OUTPUT), wxBitmapButton), ?BITMAP_OUTPUT_OFF}
    end
  end,
  {BmpBtn0, IconName0} = Toggle(Win0, false),
  {BmpBtn1, IconName1} = Toggle(Win1, true),
  wxBitmapButton:setBitmapLabel(BmpBtn0, wxBitmap:new(wxImage:new(ide_lib_widgets:rc_dir(IconName0)))),
  wxBitmapButton:setBitmapLabel(BmpBtn1, wxBitmap:new(wxImage:new(ide_lib_widgets:rc_dir(IconName1)))).
