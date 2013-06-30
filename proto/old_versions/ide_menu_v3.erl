-module(ide_menu_v3).
-export([new/1]).

-include_lib("wx/include/wx.hrl").

-define(STATUS_BAR_HELP_DEFAULT, "").

%% Menubar/toolbar macros             
-define(MENU_ID_FONT,              6000).
-define(MENU_ID_LN_TOGGLE,         6001).
-define(MENU_ID_INDENT_TYPE,       6002).
-define(MENU_ID_INDENT_WIDTH,      6003).
-define(MENU_ID_FULLSCREEN,        6004).
-define(MENU_ID_HIDE_TEST,         6005).
-define(MENU_ID_HIDE_UTIL,         6006).
-define(MENU_ID_LINE_WRAP,         6007).
-define(MENU_ID_AUTO_INDENT,       6008).
-define(MENU_ID_INDENT_SELECTION,  6009).
-define(MENU_ID_COMMENT_SELECTION, 6010).
-define(MENU_ID_FOLD_ALL,          6011).
-define(MENU_ID_UNFOLD_ALL,        6012).
-define(MENU_ID_WRANGLER,          6013).
-define(MENU_ID_COMPILE,           6014).
-define(MENU_ID_RUN,               6015).
-define(MENU_ID_DIALYZER,          6016).
-define(MENU_ID_TESTS,             6017).
-define(MENU_ID_DEBUGGER,          6018).
-define(MENU_ID_SHORTCUTS,         6019).
-define(MENU_ID_SEARCH_DOC,        6020).
-define(MENU_ID_MANUAL,            6021).
-define(MENU_ID_INDENT_TABS,       6022).
-define(MENU_ID_INDENT_SPACES,     6023).
-define(MENU_ID_MAX_EDITOR,        6024).
-define(MENU_ID_MAX_UTIL,          6025).

new(Frame) ->
    init(Frame).

init(Config) ->
    Frame = proplists:get_value(parent, Config),
    Sb = proplists:get_value(sb, Config),
            
    TabId = ets:new(myTable, []),
    ets:insert(TabId,{?wxID_NEW, "New", "Create a new file."}),
    ets:insert(TabId,{?wxID_OPEN, "Open", "Open a new file."}),
    ets:insert(TabId,{?wxID_SAVE, "Save", "Save the current file."}),
    ets:insert(TabId,{?wxID_SAVEAS, "Save As", "Save the file with a new name."}),
    ets:insert(TabId,{?wxID_PRINT, "Print", "Print the current file."}),
    ets:insert(TabId,{?wxID_CLOSE, "Close", "Close the current file."}),
    ets:insert(TabId,{?wxID_CLOSE_ALL, "Close All", "Close all open files."}),
    ets:insert(TabId,{?wxID_EXIT, "New", "Quit the application."}),
    
    ets:insert(TabId,{?wxID_UNDO, "Undo", "Undo the last change."}),
    ets:insert(TabId,{?wxID_REDO, "Redo", "Redo the last change."}),
    ets:insert(TabId,{?wxID_CUT, "Cut", "Cut the selected text."}),
    ets:insert(TabId,{?wxID_COPY, "Copy", "Copy."}),
    ets:insert(TabId,{?wxID_PASTE, "Paste", "Paste from clipboard."}),
    ets:insert(TabId,{?wxID_DELETE, "Delete", "Delete the current selection."}),
    
    ets:insert(TabId,{?MENU_ID_FONT, "New", "Create a new file."}),
    ets:insert(TabId,{?MENU_ID_LN_TOGGLE, "New", "Create a new file."}),
    ets:insert(TabId,{?MENU_ID_LN_TOGGLE, "New", "Create a new file."}),
    ets:insert(TabId,{?MENU_ID_INDENT_TABS, "New", "Create a new file."}),
    ets:insert(TabId,{?MENU_ID_INDENT_SPACES, "New", "Create a new file."}),
    ets:insert(TabId,{?MENU_ID_INDENT_WIDTH, "New", "Create a new file."}),
    ets:insert(TabId,{?MENU_ID_FULLSCREEN, "New", "Create a new file."}),
    ets:insert(TabId,{?MENU_ID_HIDE_TEST, "New", "Create a new file."}),
    ets:insert(TabId,{?MENU_ID_HIDE_UTIL, "New", "Create a new file."}),
    
    ets:insert(TabId,{?MENU_ID_LINE_WRAP, "New", "Create a new file."}),
    ets:insert(TabId,{?MENU_ID_AUTO_INDENT, "New", "Create a new file."}),
    ets:insert(TabId,{?MENU_ID_AUTO_INDENT, "New", "Create a new file."}),
    ets:insert(TabId,{?MENU_ID_INDENT_SELECTION, "New", "Create a new file."}),
    ets:insert(TabId,{?MENU_ID_COMMENT_SELECTION, "New", "Create a new file."}),
    ets:insert(TabId,{?MENU_ID_FOLD_ALL, "New", "Create a new file."}),
    ets:insert(TabId,{?MENU_ID_UNFOLD_ALL, "New", "Create a new file."}),
    ets:insert(TabId,{?MENU_ID_WRANGLER, "New", "Create a new file."}),
    ets:insert(TabId,{?MENU_ID_COMPILE, "New", "Create a new file."}),
    ets:insert(TabId,{?MENU_ID_RUN, "New", "Create a new file."}),
    ets:insert(TabId,{?MENU_ID_TESTS, "New", "Create a new file."}),
    ets:insert(TabId,{?MENU_ID_DEBUGGER, "New", "Create a new file."}),
    ets:insert(TabId,{?wxID_HELP, "New", "Create a new file."}),
    ets:insert(TabId,{?MENU_ID_SHORTCUTS, "New", "Create a new file."}),
    ets:insert(TabId,{?MENU_ID_SEARCH_DOC, "New", "Create a new file."}),
    ets:insert(TabId,{?MENU_ID_MANUAL, "New", "Create a new file."}),
    ets:insert(TabId,{?wxID_ABOUT, "New", "Create a new file."}),
    
    %%%%%%%%%%%%%%%%%%%
    %%%%% Menubar %%%%%
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
  
    View = wxMenu:new([]),
    wxMenu:append(View, ?MENU_ID_FONT, "Font"),
    wxMenu:append(View, ?wxID_SEPARATOR, []),
    wxMenu:append(View, ?MENU_ID_LN_TOGGLE, "Toggle Line Numbers", [{kind, ?wxITEM_CHECK}]),
    wxMenu:check(View, ?MENU_ID_LN_TOGGLE, true),         %% REPLACE WITH DEFAULT SETTINGS (OVERRIDDEN BY USER SETTINGS)
    wxMenu:append(View, ?wxID_SEPARATOR, []),
    IndentType  = wxMenu:new([]),  % Submenu
    wxMenu:appendRadioItem(IndentType, ?MENU_ID_INDENT_TABS, "Tabs"), 
    wxMenu:appendRadioItem(IndentType, ?MENU_ID_INDENT_SPACES, "Spaces"), 
    wxMenu:append(View, ?MENU_ID_INDENT_TYPE, "Indent Type", IndentType),
    IndentWidth = wxMenu:new([]),  % Submenu
    add_tab_width_menu(IndentWidth, 1),
    wxMenu:check(IndentWidth, 7004, true),             %% REPLACE WITH DEFAULT SETTINGS (OVERRIDDEN BY USER SETTINGS)
    wxMenu:append(View, ?MENU_ID_INDENT_WIDTH, "Indent Width", IndentWidth),
    wxMenu:append(View, ?wxID_SEPARATOR, []),
    wxMenu:append(View, ?MENU_ID_FULLSCREEN, "Fullscreen", [{kind, ?wxITEM_CHECK}]),
    wxMenu:append(View, ?wxID_SEPARATOR, []),
    wxMenu:append(View, ?MENU_ID_HIDE_TEST, "Hide Test Pane", [{kind, ?wxITEM_CHECK}]),
    wxMenu:append(View, ?MENU_ID_HIDE_UTIL, "Hide Utilities Pane", [{kind, ?wxITEM_CHECK}]),
  
    Document = wxMenu:new([]),
    wxMenu:append(Document, ?MENU_ID_LINE_WRAP, "Line Wrap", [{kind, ?wxITEM_CHECK}]),
    wxMenu:append(Document, ?MENU_ID_AUTO_INDENT, "Auto-Indent", [{kind, ?wxITEM_CHECK}]),
    wxMenu:check(Document, ?MENU_ID_AUTO_INDENT, true),   %% REPLACE WITH DEFAULT SETTINGS (OVERRIDDEN BY USER SETTINGS)
    wxMenu:append(Document, ?wxID_SEPARATOR, []),
    wxMenu:append(Document, ?MENU_ID_INDENT_SELECTION, "Indent Selection"),
    wxMenu:append(Document, ?MENU_ID_COMMENT_SELECTION, "Comment Selection"),
    wxMenu:append(Document, ?wxID_SEPARATOR, []),
    wxMenu:append(Document, ?MENU_ID_FOLD_ALL, "Fold All"),
    wxMenu:append(Document, ?MENU_ID_UNFOLD_ALL, "Unfold All"),
  
    Wrangler = wxMenu:new([]),
    wxMenu:append(Wrangler, ?MENU_ID_WRANGLER, "WRANGLER"),
  
    ToolMenu = wxMenu:new([]),
    wxMenu:append(ToolMenu, ?MENU_ID_COMPILE, "Compile"),
    wxMenu:append(ToolMenu, ?wxID_SEPARATOR, []),
    wxMenu:append(ToolMenu, ?MENU_ID_RUN, "Run Module"),
    wxMenu:append(ToolMenu, ?MENU_ID_DIALYZER, "Run Dialyzer"),
    wxMenu:append(ToolMenu, ?MENU_ID_TESTS, "Run Tests"),
    wxMenu:append(ToolMenu, ?MENU_ID_DEBUGGER, "Run Debugger"),
  
    Help  = wxMenu:new([]),
    wxMenu:append(Help, ?wxID_HELP, "Help"),
    wxMenu:append(Help, ?MENU_ID_SHORTCUTS, "Keyboard Shortcuts"),
    wxMenu:append(Help, ?wxID_SEPARATOR, []),
    wxMenu:append(Help, ?MENU_ID_SEARCH_DOC, "Search Erlang API"),
    wxMenu:append(Help, ?MENU_ID_MANUAL, "IDE Manual"),
    wxMenu:append(Help, ?wxID_SEPARATOR, []),
    wxMenu:append(Help, ?wxID_ABOUT, "About"),
  
    wxMenuBar:append(MenuBar, File, "File"),
    wxMenuBar:append(MenuBar, Edit, "Edit"),
    wxMenuBar:append(MenuBar, View, "View"),
    wxMenuBar:append(MenuBar, Document, "Document"),
    wxMenuBar:append(MenuBar, Wrangler, "Wrangler"),
    wxMenuBar:append(MenuBar, ToolMenu, "Tools"),
    wxMenuBar:append(MenuBar, Help, "Help"),
    
    %%%%%%%%%%%%%%%%%%%
    %%%%% Toolbar %%%%%
  	ToolBar = wxFrame:createToolBar(Frame, []),
    wxToolBar:setToolBitmapSize(ToolBar, {48,48}),
	  %% Id, StatusBar help, filename, args, add seperator
    Tools = [{?wxID_NEW,        "ToolTip", "icons/document-new.png",    [{shortHelp, "Create a new file"}],        			false},
             {?wxID_OPEN,       "ToolTip", "icons/document-open.png",   [{shortHelp, "Open existing document"}],   			false},
             {?wxID_SAVE,       "ToolTip", "icons/document-save.png",   [{shortHelp, "Save the current file"}],    			true}, 
             {?wxID_CLOSE,      "ToolTip", "icons/document-close.png",  [{shortHelp, "Close the current file"}],   			true},
             {?MENU_ID_COMPILE,    "ToolTip", "icons/module-compile.png",  [{shortHelp, "Compile the current file"}], 			false},
             {?MENU_ID_RUN,        "ToolTip", "icons/module-run.png",      [{shortHelp, "Run the current file"}],     			true},
             {?MENU_ID_HIDE_TEST,  "ToolTip", "icons/hide-test.png",       [{shortHelp, "Hide the test pane"}],       			false},
             {?MENU_ID_HIDE_UTIL,  "ToolTip", "icons/hide-util.png",       [{shortHelp, "Hide the utilities pane"}],  			false},
             {?MENU_ID_MAX_EDITOR, "ToolTip", "icons/maximise-editor.png", [{shortHelp, "Maximise/minimise the text editor"}], false},
             {?MENU_ID_MAX_UTIL,   "ToolTip", "icons/maximise-util.png",   [{shortHelp, "Maximise/minimise the utilities"}],   false}],
	
    AddTool = fun({Id, Tooltip, Filename, Args, true}) ->
		          wxToolBar:addTool(ToolBar, Id, Tooltip, wxBitmap:new(wxImage:new(Filename)), Args),
		          wxToolBar:addSeparator(ToolBar);
		         ({Id, Tooltip, Filename, Args, _}) ->
		          wxToolBar:addTool(ToolBar, Id, Tooltip, wxBitmap:new(wxImage:new(Filename)), Args)
              end,       

    [AddTool(Tool) || Tool <- Tools],

    wxToolBar:realize(ToolBar),

    %%%%%%%%%%%%%%%%%%%%%%%%%%
    %%%%% Event Handlers %%%%%
    
    Fun = fun (E,O) -> 
             handle_event(E, O)
           end,
    wxFrame:connect(Frame, menu_highlight,  [{callback, Fun},{userData, {Sb,TabId}}]),
    wxFrame:connect(Frame, menu_close,  [{callback, Fun},{userData, Sb}]),
    wxFrame:connect(Frame, command_menu_selected, [{callback, Fun}, {userData, TabId}]),
    
    wxFrame:setMenuBar(Frame, MenuBar),
    ok.

%%%%%%%%%%%%%%%%%%%%%%
%%%%% Call Backs %%%%%
%% Menubar/Toolbar events
% handle_event(#wx{id=Id, event=#wxCommand{type=command_menu_selected}},
%              State) ->
%     io:format("handle_event"),
%     case Id of
%         ?wxID_NEW ->
%             aui:add_editor();
%         ?wxID_OPEN ->
%             io:format("open~n");
%         ?wxID_SAVE ->
%             io:format("save~n");
%         ?wxID_SAVEAS ->
%             io:format("save as~n");
%         ?wxID_PRINT ->
%             io:format("print~n");
%         ?wxID_CLOSE ->
%             io:format("close~n");
%         ?wxID_CLOSE_ALL ->
%             io:format("close all~n");
%         ?wxID_EXIT ->
%             io:format("exit~n");
%         ?wxID_UNDO ->
%             io:format("undo~n");
%         ?wxID_REDO ->
%             io:format("redo~n");
%         ?wxID_CUT ->
%             io:format("cut~n");
%         ?wxID_COPY ->
%             io:format("copy~n");
%         ?wxID_PASTE ->
%             io:format("paste~n");
%         ?wxID_DELETE ->
%             io:format("delete~n");
%         ?MENU_ID_FONT ->
%             io:format("font~n");
%         ?MENU_ID_LN_TOGGLE ->
%             io:format("line toggle~n");
%         ?MENU_ID_INDENT_TABS ->
%             io:format("indent tabs~n");
%         ?MENU_ID_INDENT_SPACES ->
%             io:format("indent spaces~n");
%         7001 ->
%             io:format("1");
%         7002 ->
%             io:format("2");
%         7003 ->
%             io:format("3");
%         7004 ->
%             io:format("4");
%         7005 ->
%             io:format("5");
%         7006 ->
%             io:format("6");
%         7007 ->
%             io:format("7");
%         7008 ->
%             io:format("8");
%         ?MENU_ID_FULLSCREEN ->
%             io:format("fullscreen~n");
%         ?MENU_ID_HIDE_TEST ->
%       aui:show_hide("test");
%         ?MENU_ID_HIDE_UTIL ->
%             aui:show_hide("util");  
%         ?MENU_ID_MAX_EDITOR ->
%       aui:show_hide("editor");
%     ?MENU_ID_MAX_UTIL ->
%       io:format("max util~n");
%         ?MENU_ID_LINE_WRAP ->
%             io:format("line wrap~n");
%         ?MENU_ID_AUTO_INDENT ->
%             io:format("auto indent~n");
%         ?MENU_ID_INDENT_SELECTION ->
%             io:format("indent selection~n");
%         ?MENU_ID_COMMENT_SELECTION ->
%             io:format("comment selection~n");
%         ?MENU_ID_FOLD_ALL ->
%             io:format("fold all~n");
%         ?MENU_ID_UNFOLD_ALL ->
%             io:format("unfold all~n");
%         ?MENU_ID_WRANGLER ->
%             io:format("wrangler~n");
%         ?MENU_ID_COMPILE ->
%             io:format("compile~n");
%         ?MENU_ID_RUN ->
%             io:format("run~n");
%         ?MENU_ID_DIALYZER ->
%             io:format("run dialyzer~n");
%         ?MENU_ID_TESTS ->
%             io:format("run tests~n");
%         ?MENU_ID_DEBUGGER ->
%             io:format("run debugger~n");
%         ?wxID_HELP ->
%             io:format("help~n");
%         ?MENU_ID_SHORTCUTS ->
%             io:format("shortcuts~n");
%         ?MENU_ID_SEARCH_DOC ->
%             io:format("search doc~n");
%         ?MENU_ID_MANUAL ->
%             io:format("manual~n");
%         ?wxID_ABOUT ->
%             io:format("about~n")
%     end,
    % {noreply, State};
%% Here:
%% http://forums.wxwidgets.org/viewtopic.php?f=1&t=30059&p=128994&hilit=statusbar+refresh&#p128994
%% Handle menu closed event    
handle_event(#wx{userData=Sb, event=#wxMenu{type=menu_close}},
	           State) ->
       customStatusBar:set_text(Sb, {field, help}, ?STATUS_BAR_HELP_DEFAULT),
       {noreply, State};
%% Handle menu highlight events    
handle_event(#wx{id=Id, obj=Frame, userData={Sb,Tab}, event=#wxMenu{type=menu_highlight}},
	           State) ->
       [{_,_,HelpString}] = ets:lookup(Tab,Id),
       customStatusBar:set_text(Sb, {field, help}, HelpString),
       {noreply, State};
% handle_event(#wx{id=Id, obj=Frame, userData=Tab, event=#wxCommand{type=command_menu_selected}},
%              State) ->
%        io:format("menu_selected~n"),
%        [{_,_,HelpString,{Module,Function,Args}}] = ets:lookup(Tab,Id),
%        % erlang:apply(Module, Function, Args),
%        % Module:Function(),
%        aui:add_editor(),
%        % io:format("~p~n",[HelpString]),
%        {noreply, State};
    % case Id of
  %        ?wxID_NEW ->
  %            customStatusBar:set_text(Sb, {field, help}, "Whatever"),
  %            customStatusBar:set_text(Sb, {field, help}, "Create a new file.");
  %        ?wxID_OPEN ->
  %            customStatusBar:set_text(Sb, {field, help}, "Open a new file.");
  %        ?wxID_SAVE ->
  %            customStatusBar:set_text(Sb, {field, help}, "Save the current file.");
  %        ?wxID_SAVEAS ->
  %            customStatusBar:set_text(Sb, {field, help}, "Save.");
  %        ?wxID_PRINT ->
  %            customStatusBar:set_text(Sb, {field, help}, "Print.");
  %        ?wxID_CLOSE ->
  %            customStatusBar:set_text(Sb, {field, help}, "Close the current file.");
  %        ?wxID_CLOSE_ALL ->
  %            customStatusBar:set_text(Sb, {field, help}, "Close all open files.");
  %        ?wxID_EXIT ->
  %            customStatusBar:set_text(Sb, {field, help}, "Quit.");
  %        ?wxID_UNDO ->
  %            io:format("undo~n");
  %        ?wxID_REDO ->
  %            io:format("redo~n");
  %        ?wxID_CUT ->
  %            io:format("cut~n");
  %        ?wxID_COPY ->
  %            io:format("copy~n");
  %        ?wxID_PASTE ->
  %            io:format("paste~n");
  %        ?wxID_DELETE ->
  %            io:format("delete~n");
  %        ?wxID_FONT ->
  %            io:format("font~n");
  %        ?wxID_LN_TOGGLE ->
  %            io:format("line toggle~n");
  %        ?wxID_INDENT_TABS ->
  %            io:format("indent tabs~n");
  %        ?wxID_INDENT_SPACES ->
  %            io:format("indent spaces~n");
  %        7001 ->
  %            io:format("1");
  %        7002 ->
  %            io:format("2");
  %        7003 ->
  %            io:format("3");
  %        7004 ->
  %            io:format("4");
  %        7005 ->
  %            io:format("5");
  %        7006 ->
  %            io:format("6");
  %        7007 ->
  %            io:format("7");
  %        7008 ->
  %            io:format("8");
  %        ?wxID_FULLSCREEN ->
  %            io:format("fullscreen~n");
  %        ?wxID_HIDE_TEST ->
  %            io:format("hide test~n");      
  %        ?wxID_HIDE_UTIL ->
  %            io:format("hide util~n");        
  %        ?wxID_LINE_WRAP ->
  %            io:format("line wrap~n");
  %        ?wxID_AUTO_INDENT ->
  %            io:format("auto indent~n");
  %        ?wxID_INDENT_SELECTION ->
  %            io:format("indent selection~n");
  %        ?wxID_COMMENT_SELECTION ->
  %            io:format("comment selection~n");
  %        ?wxID_FOLD_ALL ->
  %            io:format("fold all~n");
  %        ?wxID_UNFOLD_ALL ->
  %            io:format("unfold all~n");
  %        ?wxID_WRANGLER ->
  %            io:format("wrangler~n");
  %        ?wxID_COMPILE ->
  %            io:format("compile~n");
  %        ?wxID_RUN ->
  %            io:format("run~n");
  %        ?wxID_DIALYZER ->
  %            io:format("run dialyzer~n");
  %        ?wxID_TESTS ->
  %            io:format("run tests~n");
  %        ?wxID_DEBUGGER ->
  %            io:format("run debugger~n");
  %        ?wxID_HELP ->
  %            io:format("help~n");
  %        ?wxID_SHORTCUTS ->
  %            io:format("shortcuts~n");
  %        ?wxID_SEARCH_DOC ->
  %            io:format("search doc~n");
  %        ?wxID_MANUAL ->
  %            io:format("manual~n");
  %        ?wxID_ABOUT ->
  %            io:format("about~n")
  %    end,
    % {noreply, State};
handle_event(E,O) ->
  io:format("TRACE: In menubar handle_event ~p~n~p~n", [E,O]),
  {noreply, O}.

  
add_tab_width_menu(TabMenu, 8) -> 
    wxMenu:appendRadioItem(TabMenu, 7008, integer_to_list(8));
add_tab_width_menu(TabMenu, Width) ->
    wxMenu:appendRadioItem(TabMenu, 7000 + Width, integer_to_list(Width)),
    add_tab_width_menu(TabMenu, Width + 1).
