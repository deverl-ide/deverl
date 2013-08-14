-module(ide_menu).

%% Client API
-export([create/1, update_label/2]). 
         
-include_lib("wx/include/wx.hrl").
-include("../include/ide.hrl").


%% =====================================================================
%% @doc Start/add a new menubar/toolbar to the frame

create(Config) ->
    init(Config).

    
%% =====================================================================
%% @doc Initialise the menubar/toolbar

init(Config) ->
    Frame = proplists:get_value(parent, Config),
    
		%% =====================================================================
		%% Menubar
		%% 
		%% =====================================================================

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
    wxMenu:append(File, ?wxID_PREFERENCES, "Preferences"),
  
    Edit        = wxMenu:new([]),
    wxMenu:append(Edit, ?wxSTC_CMD_UNDO, "Undo"),
    wxMenu:append(Edit, ?wxSTC_CMD_REDO, "Redo"),
    wxMenu:append(Edit, ?wxID_SEPARATOR, []),
    wxMenu:append(Edit, ?wxSTC_CMD_CUT, "Cut"),
    wxMenu:append(Edit, ?wxSTC_CMD_COPY, "Copy"),
    wxMenu:append(Edit, ?wxSTC_CMD_PASTE, "Paste"),
    wxMenu:append(Edit, ?wxID_DELETE, "Delete"),
    wxMenu:appendSeparator(Edit),
    wxMenu:append(Edit, ?wxID_FIND, "Find\tCtrl+F"),
  
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
		
		{Theme, LastId} = generate_radio_submenu(wxMenu:new([]),
			theme:get_theme_names(), user_prefs:get_user_pref({pref, theme}), 9000),
		wxMenu:connect(Theme, command_menu_selected, 
			[{id, 9000}, {lastId, LastId}, {userData, theme}, {skip,false}]),
		
    wxMenu:append(View, ?MENU_ID_INDENT_WIDTH, "Indent Width", IndentWidth),
    wxMenu:append(View, ?wxID_SEPARATOR, []),
    wxMenu:append(View, ?MENU_ID_THEME_SELECT, "Theme", Theme),
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
    
		%% =====================================================================
		%% Toolbar
		%% 
		%% =====================================================================

		ToolBar = wxFrame:createToolBar(Frame, []),
		wxToolBar:setMargins(ToolBar, 10, 10),
		wxToolBar:setToolBitmapSize(ToolBar, {24,24}),
		%% Id, StatusBar help, filename/art id, args, add seperator
		Tools = [
			{?wxID_NEW,           "ToolTip", {default, "wxART_NEW"},    
				[{shortHelp, "Create a new file"}],               false},
			{?wxID_OPEN,          "ToolTip", {default, "wxART_FILE_OPEN"},   
				[{shortHelp, "Open existing document"}],          false},
			{?wxID_SAVE,          "ToolTip", {default, "wxART_FILE_SAVE"},   
				[{shortHelp, "Save the current file"}],           false}, 
			{?wxID_CLOSE,         "ToolTip", {default, "wxART_CLOSE"},  
				[{shortHelp, "Close the current file"}],          true},
			{?MENU_ID_COMPILE,    "ToolTip", {custom, "../icons/module-compile.png"},  
				[{shortHelp, "Compile the current file"}],        false},
			{?MENU_ID_RUN,        "ToolTip", {custom, "../icons/module-run.png"},      
				[{shortHelp, "Run the current file"}],            true},
			{?MENU_ID_HIDE_TEST,  "ToolTip", {custom, "../icons/hide-test.png"},       
				[{shortHelp, "Hide the test pane"}],              false},
			{?MENU_ID_HIDE_UTIL,  "ToolTip", {custom, "../icons/hide-util.png"},       
				[{shortHelp, "Hide the utilities pane"}],         false},
			{?MENU_ID_MAX_EDITOR, "ToolTip", {custom, "../icons/maximise-editor.png"}, 
				[{shortHelp, "Maximise/minimise the text editor"}], false},
			{?MENU_ID_MAX_UTIL,   "ToolTip", {custom, "../icons/maximise-util.png"},   
				[{shortHelp, "Maximise/minimise the utilities"}],   false}],

		AddTool = fun({Id, Tooltip, {custom, Path}, Args, true}) ->
		        		wxToolBar:addTool(ToolBar, Id, Tooltip, wxBitmap:new(wxImage:new(Path)), Args),
		        		wxToolBar:addSeparator(ToolBar);
							({Id, Tooltip, {custom, Path}, Args, false}) ->
								wxToolBar:addTool(ToolBar, Id, Tooltip, wxBitmap:new(wxImage:new(Path)), Args);
							({Id, Tooltip, {default, Art}, Args, true}) ->
								wxToolBar:addTool(ToolBar, Id, Tooltip, wxArtProvider:getBitmap(Art), Args),
								wxToolBar:addSeparator(ToolBar);
							({Id, Tooltip, {default, Art}, Args, false}) ->
								wxToolBar:addTool(ToolBar, Id, Tooltip, wxArtProvider:getBitmap(Art), Args)
		          end,       

		[AddTool(Tool) || Tool <- Tools],

		wxToolBar:realize(ToolBar),
		
		%% =====================================================================
		%% ETS table used to store the function called when a menu item is 
		%% clicked.
		%% Record format: {Menu item Id, Tooltip, Statusbar halp string, 
		%%								{Module, Function, Args}}
		%% =====================================================================
    
    TabId = ets:new(myTable, []),
    ets:insert(TabId,{?wxID_NEW, "New", "Create a new file.", {ide,add_editor,[]}}),
    ets:insert(TabId,{?wxID_OPEN, "Open", "Open an existing file.", {ide,open_file,[Frame]}}),
    ets:insert(TabId,{?wxID_SAVE, "Save", "Save the current file.", {ide,save_current_file,[]}}),
    ets:insert(TabId,{?wxID_SAVEAS, "Save As", "Save the file with a new name.", {ide,save_new,[]}}),
    ets:insert(TabId,{?MENU_ID_SAVE_ALL, "Save All", "Save all open files.", {}}),
    ets:insert(TabId,{?wxID_PRINT, "Print", "Print the current file.", {}}),
    ets:insert(TabId,{?wxID_CLOSE, "Close", "Close the current file.", {ide, close_selected_editor, []}}),
    ets:insert(TabId,{?wxID_CLOSE_ALL, "Close All", "Close all open files.", {ide, close_all_editors, []}}),
    ets:insert(TabId,{?wxID_EXIT, "Exit", "Quit the application.", {}}),
    ets:insert(TabId,{?wxID_PREFERENCES, "Preferences", "Application preferences.", {ide_prefs, start, [[{parent,Frame}]]}}),
    
    ets:insert(TabId,{?wxSTC_CMD_UNDO, "Undo", "Undo the last change.", {}}),
    ets:insert(TabId,{?wxSTC_CMD_REDO, "Redo", "Redo the last change.", {}}),
    ets:insert(TabId,{?wxSTC_CMD_CUT, "Cut", "Cut the selected text.", {}}),
    ets:insert(TabId,{?wxSTC_CMD_COPY, "Copy", "Copy the selected text to the clipboard.", {}}),
    ets:insert(TabId,{?wxSTC_CMD_PASTE, "Paste", "Paste from clipboard.", {}}),
    ets:insert(TabId,{?wxID_DELETE, "Delete", "Delete the current selection.", {}}),
    ets:insert(TabId,{?wxID_FIND, "Find", "Find and replace.", {ide,find_replace,[Frame]}}),
    
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
    ets:insert(TabId,{?MENU_ID_COMPILE, "Compile", "Compile the current file.", {ide, open_dialog, [Frame]}}),
    ets:insert(TabId,{?MENU_ID_RUN, "Run", "Run the current file.", {}}),
    ets:insert(TabId,{?MENU_ID_DIALYZER, "Dialyzer", "Run Dialyzer.", {}}),
    ets:insert(TabId,{?MENU_ID_TESTS, "Run tests", "Run tests for current file.", {}}),
    ets:insert(TabId,{?MENU_ID_DEBUGGER, "Run debugger", "Run debugger.", {}}),
    ets:insert(TabId,{?wxID_HELP, "Help", "View Help.", {}}),
    ets:insert(TabId,{?MENU_ID_SHORTCUTS, "Keyboard shortcuts", "View keyboard shortcuts.", {}}),
    ets:insert(TabId,{?MENU_ID_SEARCH_DOC, "Search doc", "Search the Erlang documentation.", {}}),
    ets:insert(TabId,{?MENU_ID_MANUAL, "Manual", "View the IDE manual.", {}}),
    ets:insert(TabId,{?wxID_ABOUT, "About", "About.", {about, new, [{parent, Frame}]}}),
        
    {MenuBar, TabId}.


%% =====================================================================
%% @doc Generate a radio menu given a list of labels.
%% The menu item Ids will be automatically generated, starting from
%% StartId. The menu item whose label = ToCheck will be checked.
%% @private

-spec generate_radio_submenu(Menu, Items, ToCheck, StartId) -> Result when
	Menu :: wxMenu:wxMenu(),
	Items :: [string()],
	ToCheck :: string(),
	StartId :: integer(),
	Result :: {wxMenu:wxMenu(), integer()}. %% The complete menu and id of the last item added

generate_radio_submenu(Menu, [], _, Id) -> 
	{Menu, Id -1};
	
generate_radio_submenu(Menu, [Label|T], Label, StartId) ->
	wxMenu:appendRadioItem(Menu, StartId, Label),
	wxMenu:check(Menu, StartId, true),
	generate_radio_submenu(Menu, T, Label, StartId+1);
	
generate_radio_submenu(Menu, [Label|T], ToCheck, StartId) ->
	wxMenu:appendRadioItem(Menu, StartId, Label),
	generate_radio_submenu(Menu, T, ToCheck, StartId+1).

%% =====================================================================
%% @doc Add all tab width options to menu
%%
%% @private

add_tab_width_menu(TabMenu, 8) ->
  wxMenu:appendRadioItem(TabMenu, 7008, integer_to_list(8));
add_tab_width_menu(TabMenu, Width) ->
  wxMenu:appendRadioItem(TabMenu, 7000 + Width, integer_to_list(Width)),
  add_tab_width_menu(TabMenu, Width + 1).
  
  
%% =====================================================================
%% @doc Update the label of a menu item


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
