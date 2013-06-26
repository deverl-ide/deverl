-module(ide_menubar).
-export([new/1]).
-export([start/0, start/1, init/1, handle_event/2, code_change/3,
         terminate/2]).
-include_lib("wx/include/wx.hrl").

-define(wxID_FONT,              6000).
-define(wxID_LN_TOGGLE,         6001).
-define(wxID_INDENT_TYPE,       6002).
-define(wxID_INDENT_WIDTH,      6003).
-define(wxID_FULLSCREEN,        6004).
-define(wxID_HIDE_TEST,         6005).
-define(wxID_HIDE_UTIL,         6006).
-define(wxID_LINE_WRAP,         6007).
-define(wxID_AUTO_INDENT,       6008).
-define(wxID_INDENT_SELECTION,  6009).
-define(wxID_COMMENT_SELECTION, 6010).
-define(wxID_FOLD_ALL,          6011).
-define(wxID_UNFOLD_ALL,        6012).
-define(wxID_WRANGLER,          6013).
-define(wxID_COMPILE,           6014).
-define(wxID_RUN,               6015).
-define(wxID_DIALYZER,          6016).
-define(wxID_TESTS,             6017).
-define(wxID_DEBUGGER,          6018).
-define(wxID_SHORTCUTS,         6019).
-define(wxID_SEARCH_DOC,        6020).
-define(wxID_MANUAL,            6021).
-define(wxID_INDENT_TABS,       6022).
-define(wxID_INDENT_SPACES,     6023).

-record(state, {file, edit, view, document, wrangler, tools, help}).

new(Frame) ->
    start(Frame).

start() ->
    start([]).

start(Config) ->
    wx_object:start_link(?MODULE, Config, []).

init(Config) ->
    Frame = Config,
    MenuBar     = wxMenuBar:new(),
  
    File        = wxMenu:new([]),
    wxMenu:append(File, ?wxID_NEW, "New"),
    wxMenu:append(File, ?wxID_OPEN, "Open"),
    wxMenu:append(File, ?wxID_SEPARATOR, []),
    wxMenu:append(File, ?wxID_SAVE, "Save"),
    wxMenu:append(File, ?wxID_SAVEAS, "Save As"),
    wxMenu:append(File, ?wxID_SEPARATOR, []),
    wxMenu:append(File, ?wxID_PRINT, "Print"),
    wxMenu:append(File, ?wxID_SEPARATOR, []),
    wxMenu:append(File, ?wxID_CLOSE, "Close"),
    wxMenu:append(File, ?wxID_CLOSE_ALL, "Close All"),
    wxMenu:append(File, ?wxID_SEPARATOR, []),
    wxMenu:append(File, ?wxID_EXIT, "Exit"),
  
    Edit        = wxMenu:new([]),
    wxMenu:append(Edit, ?wxID_UNDO, "Undo"),
    wxMenu:append(Edit, ?wxID_REDO, "Redo"),
    wxMenu:append(Edit, ?wxID_SEPARATOR, []),
    wxMenu:append(Edit, ?wxID_CUT, "Cut"),
    wxMenu:append(Edit, ?wxID_COPY, "Copy"),
    wxMenu:append(Edit, ?wxID_PASTE, "Paste"),
    wxMenu:append(Edit, ?wxID_DELETE, "Delete"),
  
    View        = wxMenu:new([]),
    wxMenu:append(View, ?wxID_FONT, "Font"),
    wxMenu:append(View, ?wxID_SEPARATOR, []),
    wxMenu:append(View, ?wxID_LN_TOGGLE, "Toggle Line Numbers", [{kind, ?wxITEM_CHECK}]),
      wxMenu:check(View, ?wxID_LN_TOGGLE, true),         %% REPLACE WITH DEFAULT SETTINGS (OVERRIDDEN BY USER SETTINGS)
    wxMenu:append(View, ?wxID_SEPARATOR, []),
      IndentType  = wxMenu:new([]),  % Submenu
      wxMenu:appendRadioItem(IndentType, ?wxID_INDENT_TABS, "Tabs"), 
      wxMenu:appendRadioItem(IndentType, ?wxID_INDENT_SPACES, "Spaces"), 
    wxMenu:append(View, ?wxID_INDENT_TYPE, "Indent Type", IndentType),
      IndentWidth = wxMenu:new([]),  % Submenu
      add_tab_width_menu(IndentWidth, 1),
      wxMenu:check(IndentWidth, 7004, true),             %% REPLACE WITH DEFAULT SETTINGS (OVERRIDDEN BY USER SETTINGS)
    wxMenu:append(View, ?wxID_INDENT_WIDTH, "Indent Width", IndentWidth),
    wxMenu:append(View, ?wxID_SEPARATOR, []),
    wxMenu:append(View, ?wxID_FULLSCREEN, "Fullscreen", [{kind, ?wxITEM_CHECK}]),
    wxMenu:append(View, ?wxID_SEPARATOR, []),
    wxMenu:append(View, ?wxID_HIDE_TEST, "Hide Test Pane", [{kind, ?wxITEM_CHECK}]),
    wxMenu:append(View, ?wxID_HIDE_UTIL, "Hide Utilities Pane", [{kind, ?wxITEM_CHECK}]),
  
    Document    = wxMenu:new([]),
    wxMenu:append(Document, ?wxID_LINE_WRAP, "Line Wrap", [{kind, ?wxITEM_CHECK}]),
    wxMenu:append(Document, ?wxID_AUTO_INDENT, "Auto-Indent", [{kind, ?wxITEM_CHECK}]),
      wxMenu:check(Document, ?wxID_AUTO_INDENT, true),   %% REPLACE WITH DEFAULT SETTINGS (OVERRIDDEN BY USER SETTINGS)
    wxMenu:append(Document, ?wxID_SEPARATOR, []),
    wxMenu:append(Document, ?wxID_INDENT_SELECTION, "Indent Selection"),
    wxMenu:append(Document, ?wxID_COMMENT_SELECTION, "Comment Selection"),
    wxMenu:append(Document, ?wxID_SEPARATOR, []),
    wxMenu:append(Document, ?wxID_FOLD_ALL, "Fold All"),
    wxMenu:append(Document, ?wxID_UNFOLD_ALL, "Unfold All"),
  
    Wrangler    = wxMenu:new([]),
    wxMenu:append(Wrangler, ?wxID_WRANGLER, "WRANGLER"),
  
    Tools       = wxMenu:new([]),
    wxMenu:append(Tools, ?wxID_COMPILE, "Compile"),
    wxMenu:append(Tools, ?wxID_SEPARATOR, []),
    wxMenu:append(Tools, ?wxID_RUN, "Run Module"),
    wxMenu:append(Tools, ?wxID_DIALYZER, "Run Dialyzer"),
    wxMenu:append(Tools, ?wxID_TESTS, "Run Tests"),
    wxMenu:append(Tools, ?wxID_DEBUGGER, "Run Debugger"),
  
    Help        = wxMenu:new([]),
    wxMenu:append(Help, ?wxID_HELP, "Help"),
    wxMenu:append(Help, ?wxID_SHORTCUTS, "Keyboard Shortcuts"),
    wxMenu:append(Help, ?wxID_SEPARATOR, []),
    wxMenu:append(Help, ?wxID_SEARCH_DOC, "Search Erlang API"),
    wxMenu:append(Help, ?wxID_MANUAL, "IDE Manual"),
    wxMenu:append(Help, ?wxID_SEPARATOR, []),
      wxMenu:append(Help, ?wxID_ABOUT, "About"),
  
    wxMenuBar:append(MenuBar, File, "File"),
    wxMenuBar:append(MenuBar, Edit, "Edit"),
    wxMenuBar:append(MenuBar, View, "View"),
    wxMenuBar:append(MenuBar, Document, "Document"),
    wxMenuBar:append(MenuBar, Wrangler, "Wrangler"),
    wxMenuBar:append(MenuBar, Tools, "Tools"),
    wxMenuBar:append(MenuBar, Help, "Help"),
  
    wxMenuBar:connect(Frame, command_menu_selected),
    wxFrame:setMenuBar(Frame,MenuBar),
    {Frame, State=#state{file=File}}. %% Not complete, obvs.
    
%%%%% Call Backs %%%%%
%% @doc Handles all menu events
handle_event(#wx{id = Id, event = #wxCommand{type = command_menu_selected}},
	     State = #state{}) ->
    case Id of
        ?wxID_NEW ->
            io:format("new~n");
        ?wxID_OPEN ->
            io:format("open~n");
        ?wxID_SAVE ->
            io:format("save~n");
        ?wxID_SAVEAS ->
            io:format("save as~n");
        ?wxID_PRINT ->
            io:format("print~n");
        ?wxID_CLOSE ->
            io:format("close~n");
        ?wxID_CLOSE_ALL ->
            io:format("close all~n");
        ?wxID_EXIT ->
            io:format("exit~n");
        ?wxID_UNDO ->
            io:format("undo~n");
        ?wxID_REDO ->
            io:format("redo~n");
        ?wxID_CUT ->
            io:format("cut~n");
        ?wxID_COPY ->
            io:format("copy~n");
        ?wxID_PASTE ->
            io:format("paste~n");
        ?wxID_DELETE ->
            io:format("delete~n");
        ?wxID_FONT ->
            io:format("font~n");
        ?wxID_LN_TOGGLE ->
            io:format("line toggle~n");
        ?wxID_INDENT_TABS ->
            io:format("indent tabs~n");
        ?wxID_INDENT_SPACES ->
            io:format("indent spaces~n");
        7001 ->
            io:format("1");
        7002 ->
            io:format("2");
        7003 ->
            io:format("3");
        7004 ->
            io:format("4");
        7005 ->
            io:format("5");
        7006 ->
            io:format("6");
        7007 ->
            io:format("7");
        7008 ->
            io:format("8");
        ?wxID_FULLSCREEN ->
            io:format("fullscreen~n");
        ?wxID_HIDE_TEST ->
            io:format("hide test~n");
        ?wxID_HIDE_UTIL ->
            io:format("hide util~n");
        ?wxID_LINE_WRAP ->
            io:format("line wrap~n");
        ?wxID_AUTO_INDENT ->
            io:format("auto indent~n");
        ?wxID_INDENT_SELECTION ->
            io:format("indent selection~n");
        ?wxID_COMMENT_SELECTION ->
            io:format("comment selection~n");
        ?wxID_FOLD_ALL ->
            io:format("fold all~n");
        ?wxID_UNFOLD_ALL ->
            io:format("unfold all~n");
        ?wxID_WRANGLER ->
            io:format("wrangler~n");
        ?wxID_COMPILE ->
            io:format("compile~n");
        ?wxID_RUN ->
            io:format("run~n");
        ?wxID_DIALYZER ->
            io:format("run dialyzer~n");
        ?wxID_TESTS ->
            io:format("run tests~n");
        ?wxID_DEBUGGER ->
            io:format("run debugger~n");
        ?wxID_HELP ->
            io:format("help~n");
        ?wxID_SHORTCUTS ->
            io:format("shortcuts~n");
        ?wxID_SEARCH_DOC ->
            io:format("search doc~n");
        ?wxID_MANUAL ->
            io:format("manual~n");
        ?wxID_ABOUT ->
            io:format("about~n")
    end,
    {noreply, State}.

code_change(_, _, State) ->
    {stop, not_yet_implemented, State}.

terminate(_Reason, _State) ->
    wx:destroy().
  
add_tab_width_menu(TabMenu, 8) -> 
    wxMenu:appendRadioItem(TabMenu, 7008, integer_to_list(8));
add_tab_width_menu(TabMenu, Width) ->
    wxMenu:appendRadioItem(TabMenu, 7000 + Width, integer_to_list(Width)),
    add_tab_width_menu(TabMenu, Width + 1).
