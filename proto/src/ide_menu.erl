-module(ide_menu).

%% Client API
-export([new/1]). 

%% Callbacks
-export([start/1, init/1, handle_event/2, code_change/3,
         terminate/2]).
         
-include_lib("wx/include/wx.hrl").

-behaviour(wx_object).

-define(STATUS_BAR_HELP_DEFAULT, "").

%% Menubar/toolbar macros   
-define(MENU_ID_SAVE_ALL,          4001).          
-define(MENU_ID_FONT,              6000).
-define(MENU_FONT_BIGGER,          4002).
-define(MENU_FONT_SMALLER,         4003).
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


-record(state, {file, edit, view, document, wrangler, tools, help}).

new(Frame) ->
    start(Frame).

start(Config) ->
    wx_object:start(?MODULE, Config, []).

init(Config) ->
    Frame = proplists:get_value(parent, Config),
    Sb = proplists:get_value(sb, Config),
   
    %%%%%%%%%%%%%%%%%%%%%
    %%%%% ETS Table %%%%%
    TabId = ets:new(myTable, []),
    ets:insert(TabId,{?wxID_NEW, "New", "Create a new file.", {ide,add_editor,[]}}),
    ets:insert(TabId,{?wxID_OPEN, "Open", "Open an existing file.", {ide,get_selected_editor,[]}}),
    ets:insert(TabId,{?wxID_SAVE, "Save", "Save the current file.", {ide, save_current_file, []}}),
    ets:insert(TabId,{?wxID_SAVEAS, "Save As", "Save the file with a new name.", {ide,apply_to_all_editors,[]}}),
    ets:insert(TabId,{?MENU_ID_SAVE_ALL, "Save All", "Save all open files.", {}}),
    ets:insert(TabId,{?wxID_PRINT, "Print", "Print the current file.", {}}),
    ets:insert(TabId,{?wxID_CLOSE, "Close", "Close the current file.", {}}),
    ets:insert(TabId,{?wxID_CLOSE_ALL, "Close All", "Close all open files.", {}}),
    ets:insert(TabId,{?wxID_EXIT, "Exit", "Quit the application.", {}}),
    
    ets:insert(TabId,{?wxSTC_CMD_UNDO, "Undo", "Undo the last change.", {}}),
    ets:insert(TabId,{?wxSTC_CMD_REDO, "Redo", "Redo the last change.", {}}),
    ets:insert(TabId,{?wxSTC_CMD_CUT, "Cut", "Cut the selected text.", {}}),
    ets:insert(TabId,{?wxSTC_CMD_COPY, "Copy", "Copy the selected text to the clipboard.", {}}),
    ets:insert(TabId,{?wxSTC_CMD_PASTE, "Paste", "Paste from clipboard.", {}}),
    ets:insert(TabId,{?wxID_DELETE, "Delete", "Delete the current selection.", {}}),
    
    ets:insert(TabId,{?MENU_ID_FONT, "Font", "Select font.", {ide,update_styles,[Frame]}}),
    ets:insert(TabId,{?MENU_FONT_BIGGER, "Bigger", "Increase the font size.", {}}),
    ets:insert(TabId,{?MENU_FONT_SMALLER, "Smaller", "Decrease the font size.", {}}),
    ets:insert(TabId,{?MENU_ID_LN_TOGGLE, "Toggle line numbers", "Toggle line numbers on/off.", {}}),
    ets:insert(TabId,{?MENU_ID_INDENT_TYPE, "Indent Type", "Indent type: tabs/spaces.", {}}),
    ets:insert(TabId,{?MENU_ID_INDENT_TABS, "Tabs", "Indent using tabs.", {}}),
    ets:insert(TabId,{?MENU_ID_INDENT_SPACES, "Spaces", "Indent using spaces.", {}}),
    ets:insert(TabId,{?MENU_ID_INDENT_WIDTH, "Indent width", "Width of indent in spaces.", {}}),
    ets:insert(TabId,{?MENU_ID_FULLSCREEN, "Fullscreen", "Toggle fullscreen.", {}}),
    ets:insert(TabId,{?MENU_ID_HIDE_TEST, "Hide Test Pane", "Hide/Show the test pane.",           {ide,toggle_pane,[test]}, {update_label,Frame,2}}),
    ets:insert(TabId,{?MENU_ID_HIDE_UTIL, "Hide Utilities Pane", "Hide/Show the utilities pane.", {ide,toggle_pane,[util]}, {update_label,Frame,2}}),
    ets:insert(TabId,{?MENU_ID_MAX_EDITOR, "Maximise editor", "Maximise the editor pane.",        {ide,toggle_pane,[editor]}}),
    ets:insert(TabId,{?MENU_ID_MAX_UTIL, "Maximise utilities", "Maximise the utilities pane.",    {ide,toggle_pane,[maxutil]}}),
    
    
    ets:insert(TabId,{?MENU_ID_LINE_WRAP, "Line Wrap", "Line wrap.", {}}),
    ets:insert(TabId,{?MENU_ID_AUTO_INDENT, "Auto indent", "Auto indent.", {}}),
    ets:insert(TabId,{?MENU_ID_INDENT_SELECTION, "Indent selection", "Indent selected text.", {}}),
    ets:insert(TabId,{?MENU_ID_COMMENT_SELECTION, "Comment selection", "Comment the selected text.", {}}),
    ets:insert(TabId,{?MENU_ID_FOLD_ALL, "Fold all", "Fold all code.", {}}),
    ets:insert(TabId,{?MENU_ID_UNFOLD_ALL, "Unfold all", "Unfold all code.", {}}),
    ets:insert(TabId,{?MENU_ID_WRANGLER, "Wrangler", "Wrangler.", {}}),
    ets:insert(TabId,{?MENU_ID_COMPILE, "Compile", "Compile the current file.", {}}),
    ets:insert(TabId,{?MENU_ID_RUN, "Run", "Run the current file.", {}}),
    ets:insert(TabId,{?MENU_ID_DIALYZER, "Dialyzer", "Run Dialyzer.", {}}),
    ets:insert(TabId,{?MENU_ID_TESTS, "Run tests", "Run tests for current file.", {}}),
    ets:insert(TabId,{?MENU_ID_DEBUGGER, "Run debugger", "Run debugger.", {}}),
    ets:insert(TabId,{?wxID_HELP, "Help", "View Help.", {}}),
    ets:insert(TabId,{?MENU_ID_SHORTCUTS, "Keyboard shortcuts", "View keyboard shortcuts.", {}}),
    ets:insert(TabId,{?MENU_ID_SEARCH_DOC, "Search doc", "Search the Erlang documentation.", {}}),
    ets:insert(TabId,{?MENU_ID_MANUAL, "Manual", "View the IDE manual.", {}}),
    ets:insert(TabId,{?wxID_ABOUT, "About", "About.", {about, new, [{parent, Frame}]}}),
    
    %%%%%%%%%%%%%%%%%%%
    %%%%% Menubar %%%%%
    MenuBar     = wxMenuBar:new(),
      
    File        = wxMenu:new([]),
    wxMenu:append(File, ?wxID_NEW, "New"),
    wxMenu:append(File, ?wxID_OPEN, "Open"),
    wxMenu:append(File, ?wxID_SEPARATOR, []),
    wxMenu:append(File, ?wxID_SAVE, "Save"),
    wxMenu:append(File, ?wxID_SAVEAS, "Save As"),
    wxMenu:append(File, ?MENU_ID_SAVE_ALL, "Save All"),
    wxMenu:append(File, ?wxID_SEPARATOR, []),
    wxMenu:append(File, ?wxID_PRINT, "Print"),
    wxMenu:append(File, ?wxID_SEPARATOR, []),
    wxMenu:append(File, ?wxID_CLOSE, "Close"),
    wxMenu:append(File, ?wxID_CLOSE_ALL, "Close All"),
    wxMenu:append(File, ?wxID_SEPARATOR, []),
    wxMenu:append(File, ?wxID_EXIT, "Exit"),
  
    Edit        = wxMenu:new([]),
    wxMenu:append(Edit, ?wxSTC_CMD_UNDO, "Undo"),
    wxMenu:append(Edit, ?wxSTC_CMD_REDO, "Redo"),
    wxMenu:append(Edit, ?wxID_SEPARATOR, []),
    wxMenu:append(Edit, ?wxSTC_CMD_CUT, "Cut"),
    wxMenu:append(Edit, ?wxSTC_CMD_COPY, "Copy"),
    wxMenu:append(Edit, ?wxSTC_CMD_PASTE, "Paste"),
    wxMenu:append(Edit, ?wxID_DELETE, "Delete"),
  
    Font = wxMenu:new([]), %% Sub-menu
    wxMenu:append(Font, ?MENU_ID_FONT, "Font Picker"),
    wxMenu:appendSeparator(Font),
    wxMenu:append(Font, ?MENU_FONT_BIGGER, "&Bigger\tCtrl++"),
    wxMenu:append(Font, ?MENU_FONT_SMALLER, "Smaller\tCtrl+-"),
    wxMenu:appendSeparator(Font),
    
    View        = wxMenu:new([]),
    wxMenu:append(View, ?wxID_ANY, "Font", Font),
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
    wxMenu:append(View, ?MENU_ID_HIDE_TEST, "Hide Test Pane", []),
    wxMenu:append(View, ?MENU_ID_HIDE_UTIL, "Hide Utilities Pane", []),
    wxMenu:append(View, ?MENU_ID_MAX_EDITOR, "Maximise Editor", []),
    wxMenu:append(View, ?MENU_ID_MAX_UTIL, "Maximise Utilities", []),
  
    Document    = wxMenu:new([]),
    wxMenu:append(Document, ?MENU_ID_LINE_WRAP, "Line Wrap", [{kind, ?wxITEM_CHECK}]),
    wxMenu:append(Document, ?MENU_ID_AUTO_INDENT, "Auto-Indent", [{kind, ?wxITEM_CHECK}]),
    wxMenu:check(Document, ?MENU_ID_AUTO_INDENT, true),   %% REPLACE WITH DEFAULT SETTINGS (OVERRIDDEN BY USER SETTINGS)
    wxMenu:append(Document, ?wxID_SEPARATOR, []),
    wxMenu:append(Document, ?MENU_ID_INDENT_SELECTION, "Indent Selection"),
    wxMenu:append(Document, ?MENU_ID_COMMENT_SELECTION, "Comment Selection"),
    wxMenu:append(Document, ?wxID_SEPARATOR, []),
    wxMenu:append(Document, ?MENU_ID_FOLD_ALL, "Fold All"),
    wxMenu:append(Document, ?MENU_ID_UNFOLD_ALL, "Unfold All"),
  
    Wrangler    = wxMenu:new([]),
    wxMenu:append(Wrangler, ?MENU_ID_WRANGLER, "WRANGLER"),
  
    ToolMenu    = wxMenu:new([]),
    wxMenu:append(ToolMenu, ?MENU_ID_COMPILE, "Compile"),
    wxMenu:append(ToolMenu, ?wxID_SEPARATOR, []),
    wxMenu:append(ToolMenu, ?MENU_ID_RUN, "Run Module"),
    wxMenu:append(ToolMenu, ?MENU_ID_DIALYZER, "Run Dialyzer"),
    wxMenu:append(ToolMenu, ?MENU_ID_TESTS, "Run Tests"),
    wxMenu:append(ToolMenu, ?MENU_ID_DEBUGGER, "Run Debugger"),
  
    Help        = wxMenu:new([]),
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
    Tools = [{?wxID_NEW,           "ToolTip", "icons/document-new.png",    [{shortHelp, "Create a new file"}],        		   false},
             {?wxID_OPEN,          "ToolTip", "icons/document-open.png",   [{shortHelp, "Open existing document"}],   		   false},
             {?wxID_SAVE,          "ToolTip", "icons/document-save.png",   [{shortHelp, "Save the current file"}],    		   true}, 
             {?wxID_CLOSE,         "ToolTip", "icons/document-close.png",  [{shortHelp, "Close the current file"}],   		   true},
             {?MENU_ID_COMPILE,    "ToolTip", "icons/module-compile.png",  [{shortHelp, "Compile the current file"}], 		   false},
             {?MENU_ID_RUN,        "ToolTip", "icons/module-run.png",      [{shortHelp, "Run the current file"}],     		   true},
             {?MENU_ID_HIDE_TEST,  "ToolTip", "icons/hide-test.png",       [{shortHelp, "Hide the test pane"}],       		   false},
             {?MENU_ID_HIDE_UTIL,  "ToolTip", "icons/hide-util.png",       [{shortHelp, "Hide the utilities pane"}],  		   false},
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

    Fun = fun (E,O) -> 
             handle_event(E, O)
           end,
    wxFrame:connect(Frame, menu_highlight,  [{callback, Fun},{userData, {Sb,TabId}}]),
    wxFrame:connect(Frame, menu_close,  [{userData, Sb}]),
    wxFrame:connect(Frame, command_menu_selected, [{userData, {Sb,TabId}}]),
    
    wxFrame:setMenuBar(Frame, MenuBar),
    {Frame, #state{file=File}}.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Event Handlers %%%%%
%% Handle menu closed event    
handle_event(#wx{userData=Sb, event=#wxMenu{type=menu_close}},
	           State) ->
       customStatusBar:set_text(Sb, {field, help}, ?STATUS_BAR_HELP_DEFAULT),
       {noreply, State};
%% Handle menu highlight events    
handle_event(#wx{id=Id, userData={Sb,Tab}, event=#wxMenu{type=menu_highlight}},
	           State) ->
       Result = ets:lookup(Tab,Id),
       Fun = fun([{_,_,HelpString,_}]) ->
               customStatusBar:set_text(Sb, {field, help}, HelpString);
             (_) ->
               customStatusBar:set_text(Sb, {field, help}, "Help not available.")
             end,
       Fun(Result),
       {noreply, State};
handle_event(#wx{id=Id, userData={Sb,Tab}, event=#wxCommand{type=command_menu_selected}},
             State) ->
       Result = ets:lookup(Tab,Id),
       Fun = fun([{MenuItem,_,_,{Module, Function, Args},{update_label, Frame, Pos}}]) ->
			   erlang:apply(Module,Function,Args),
			   MenuBar = wxFrame:getMenuBar(Frame),
			   Menu = wxMenuBar:getMenu(MenuBar, Pos),
			   update_label(MenuItem, Menu);
			 ([{_,_,_,{Module,Function,[]}}]) ->
               Module:Function();
             ([{_,_,_,{Module,Function,Args}}]) ->
               erlang:apply(Module,Function,Args);
             (_) ->
               customStatusBar:set_text(Sb, {field, help}, "Not yet implemented.")
             end,
       Fun(Result),
       {noreply, State};
handle_event(E,O) ->
	io:format("TRACE: In menubar handle_event ~p~n~p~n", [E,O]),
	{noreply, O}.

code_change(_, _, State) ->
    {stop, not_yet_implemented, State}.

terminate(_Reason, _State) ->
    wx:destroy().

add_tab_width_menu(TabMenu, 8) ->
    wxMenu:appendRadioItem(TabMenu, 7008, integer_to_list(8));
add_tab_width_menu(TabMenu, Width) ->
    wxMenu:appendRadioItem(TabMenu, 7000 + Width, integer_to_list(Width)),
    add_tab_width_menu(TabMenu, Width + 1).

update_label(MenuItem, Menu) ->
	case wxMenu:getLabel(Menu, MenuItem) of
		"Hide Test Pane" ->
			wxMenu:setLabel(Menu, MenuItem, "Show Test Pane");
		"Show Test Pane" ->
			wxMenu:setLabel(Menu, MenuItem, "Hide Test Pane");
		"Hide Utilities Pane" ->
			wxMenu:setLabel(Menu, MenuItem, "Show Utilities Pane");
		"Show Utilities Pane" ->
			wxMenu:setLabel(Menu, MenuItem, "Hide Utilities Pane")
	end.
		
		
		
		
		
			
