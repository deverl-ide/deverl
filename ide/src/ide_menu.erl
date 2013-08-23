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
    wxMenu:append(File, ?MENU_ID_NEW_PROJECT, "New Project"),
    wxMenu:append(File, ?wxID_SEPARATOR, []),
    wxMenu:append(File, ?wxID_OPEN, "Open"),
    wxMenu:append(File, ?MENU_ID_OPEN_PROJECT, "Open Project"),
    wxMenu:append(File, ?wxID_SEPARATOR, []),
    wxMenu:append(File, ?wxID_SAVE, "Save"),
    wxMenu:append(File, ?wxID_SAVEAS, "Save As"),
    wxMenu:append(File, ?MENU_ID_SAVE_ALL, "Save All"),
    wxMenu:append(File, ?wxID_SEPARATOR, []),
    wxMenu:append(File, ?wxID_CLOSE, "Close"),
    wxMenu:append(File, ?wxID_CLOSE_ALL, "Close All"),
    wxMenu:append(File, ?wxID_SEPARATOR, []),
    wxMenu:append(File, ?wxID_EXIT, "Exit"),
    wxMenu:append(File, ?wxID_PREFERENCES, "Preferences"),
    wxMenu:append(File, ?wxID_PRINT, "Print"),
  
    Edit        = wxMenu:new([]),
    wxMenu:append(Edit, ?wxID_UNDO, "Undo"),
    wxMenu:append(Edit, ?wxID_REDO, "Redo"),
    wxMenu:append(Edit, ?wxID_SEPARATOR, []),
    wxMenu:append(Edit, ?wxID_CUT, "Cut"),
    wxMenu:append(Edit, ?wxID_COPY, "Copy"), 
    wxMenu:append(Edit, ?wxID_PASTE, "Paste"),
    wxMenu:append(Edit, ?wxID_DELETE, "Delete"),
    wxMenu:append(Edit, ?wxID_SEPARATOR, []),
    wxMenu:append(Edit, ?wxID_SELECTALL, "Select All"),
    wxMenu:appendSeparator(Edit),
    wxMenu:append(Edit, ?wxID_FIND, "Find\tCtrl+F"),
  
    Font = wxMenu:new([]), %% Sub-menu
    wxMenu:append(Font, ?MENU_ID_FONT, "Choose Font"),
    wxMenu:appendSeparator(Font),
    wxMenu:append(Font, ?MENU_ID_FONT_BIGGER, "Zoom In\tCtrl++"),
    wxMenu:append(Font, ?MENU_ID_FONT_SMALLER, "Zoom Out\tCtrl+-"),
    wxMenu:appendSeparator(Font),
    
    View        = wxMenu:new([]),
    wxMenu:append(View, ?wxID_ANY, "Font", Font),
    wxMenu:append(View, ?wxID_SEPARATOR, []),
    wxMenu:append(View, ?MENU_ID_LINE_WRAP, "Line Wrap", [{kind, ?wxITEM_CHECK}]),
		Pref = case user_prefs:get_user_pref({pref, line_wrap}) of
			0 -> false;
			_ -> true
		end,
    wxMenu:check(View, ?MENU_ID_LINE_WRAP, Pref),
    wxMenu:append(View, ?wxID_SEPARATOR, []),
    wxMenu:append(View, ?MENU_ID_LN_TOGGLE, "Toggle Line Numbers", [{kind, ?wxITEM_CHECK}]),
    wxMenu:check(View, ?MENU_ID_LN_TOGGLE, user_prefs:get_user_pref({pref, show_line_no})),
    wxMenu:append(View, ?wxID_SEPARATOR, []),

		{IndentType, _} = generate_radio_submenu(wxMenu:new([]), ["Tabs", "Spaces"],
			user_prefs:get_user_pref({pref, use_tabs}), ?MENU_ID_INDENT_TABS),
		
    wxMenu:append(View, ?MENU_ID_INDENT_TYPE, "Indent Type", IndentType),
		
		{IndentWidth, IndentMax} = generate_radio_submenu(wxMenu:new([]),
			[integer_to_list(Width) || Width <- lists:seq(2, 8)], 
			user_prefs:get_user_pref({pref, tab_width}), ?MENU_ID_TAB_WIDTH_LOWEST),
			
    wxMenu:append(View, ?MENU_ID_TAB_WIDTH, "Tab Width", IndentWidth),
		
		{Theme, ThemeMax} = generate_radio_submenu(wxMenu:new([]),
			theme:get_theme_names(), user_prefs:get_user_pref({pref, theme}), ?MENU_ID_THEME_LOWEST),
		
    wxMenu:append(View, ?MENU_ID_INDENT_GUIDES, "Indent Guides", [{kind, ?wxITEM_CHECK}]),
    wxMenu:check(View, ?MENU_ID_INDENT_GUIDES, user_prefs:get_user_pref({pref, indent_guides})),
    wxMenu:append(View, ?wxID_SEPARATOR, []),
    wxMenu:append(View, ?MENU_ID_THEME_SELECT, "Theme", Theme),
    wxMenu:append(View, ?wxID_SEPARATOR, []),
    wxMenu:append(View, ?MENU_ID_SCROLL_END, "Scroll Past Last Line", [{kind, ?wxITEM_CHECK}]),
    wxMenu:check(View, ?MENU_ID_SCROLL_END, user_prefs:get_user_pref({pref, scroll_past_end})),
    wxMenu:append(View, ?wxID_SEPARATOR, []),
    wxMenu:append(View, ?MENU_ID_FULLSCREEN, "Fullscreen", [{kind, ?wxITEM_CHECK}]),
    wxMenu:append(View, ?wxID_SEPARATOR, []),
    wxMenu:append(View, ?MENU_ID_HIDE_TEST, "Hide Test Pane", []),
    wxMenu:append(View, ?MENU_ID_HIDE_UTIL, "Hide Utilities Pane", []),
    wxMenu:append(View, ?MENU_ID_MAX_EDITOR, "Maximise Editor", []),
    wxMenu:append(View, ?MENU_ID_MAX_UTIL, "Maximise Utilities", []),
  
    Document    = wxMenu:new([]),	
    wxMenu:append(Document, ?MENU_ID_AUTO_INDENT, "Auto-Indent", [{kind, ?wxITEM_CHECK}]),
    wxMenu:check(Document, ?MENU_ID_AUTO_INDENT, user_prefs:get_user_pref({pref, auto_indent})),
    wxMenu:append(Document, ?wxID_SEPARATOR, []),
    wxMenu:append(Document, ?MENU_ID_INDENT_RIGHT, "Indent Right \tCtrl+]"),
    wxMenu:append(Document, ?MENU_ID_INDENT_LEFT, "Indent Left \tCtrl+["),
    wxMenu:append(Document, ?wxID_SEPARATOR, []),
    wxMenu:append(Document, ?MENU_ID_TOGGLE_COMMENT, "Comment \tCtrl+/"),
    wxMenu:append(Document, ?wxID_SEPARATOR, []),
    wxMenu:append(Document, ?MENU_ID_UC_SEL, "Uppercase Selection"),
    wxMenu:append(Document, ?MENU_ID_LC_SEL, "Lowercase Selection"),
    wxMenu:append(Document, ?wxID_SEPARATOR, []),
    wxMenu:append(Document, ?MENU_ID_FOLD_ALL, "Fold All"),
    wxMenu:append(Document, ?MENU_ID_UNFOLD_ALL, "Unfold All"),
    wxMenu:append(Document, ?wxID_SEPARATOR, []),	
    wxMenu:append(Document, ?MENU_ID_GOTO_LINE, "Go to Line.. \tCtrl+L"),
  
    Wrangler    = wxMenu:new([]),
    wxMenu:append(Wrangler, ?MENU_ID_WRANGLER, "WRANGLER"),
  
    ToolMenu    = wxMenu:new([]),
    wxMenu:append(ToolMenu, ?MENU_ID_COMPILE, "Compile"),
    wxMenu:append(ToolMenu, ?wxID_SEPARATOR, []),
    wxMenu:append(ToolMenu, ?MENU_ID_RUN, "Run Module"),
    wxMenu:append(ToolMenu, ?MENU_ID_DIALYZER, "Run Dialyzer"),
    wxMenu:append(ToolMenu, ?MENU_ID_TESTS, "Run Tests"),
    wxMenu:append(ToolMenu, ?MENU_ID_DEBUGGER, "Run Debugger"),
  
	  Window        = wxMenu:new([]),
	  wxMenu:append(Window, ?MENU_ID_PROJECTS_WINDOW, "Projects \tCtrl+1"),
	  wxMenu:append(Window, ?MENU_ID_TESTS_WINDOW, "Tests \tCtrl+2"),
	  wxMenu:append(Window, ?wxID_SEPARATOR, []),
	  wxMenu:append(Window, ?MENU_ID_CONSOLE_WINDOW, "Console \tCtrl+3"),
	  wxMenu:append(Window, ?MENU_ID_OBSERVER_WINDOW, "Observer \tCtrl+4"),
	  wxMenu:append(Window, ?MENU_ID_DIALYSER_WINDOW, "Dialyser \tCtrl+5"),
	  wxMenu:append(Window, ?MENU_ID_DEBUGGER_WINDOW, "Debugger \tCtrl+6"),
	  wxMenu:append(Window, ?wxID_SEPARATOR, []),
	  wxMenu:append(Window, ?MENU_ID_NEXT_TAB, "Next Tab \tCtrl+}"),
	  wxMenu:append(Window, ?MENU_ID_PREV_TAB, "Previous Tab \tCtrl+{"),
	
    Help        = wxMenu:new([]),
    wxMenu:append(Help, ?wxID_HELP, "Help"),
    wxMenu:append(Help, ?MENU_ID_HOTKEYS, "Keyboard Shortcuts"),
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
    wxMenuBar:append(MenuBar, Window, "Window"),
    wxMenuBar:append(MenuBar, Help, "Help"),
		
	  wxFrame:setMenuBar(Frame, MenuBar),
    
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
		%% ETS menu events table.
		%% Record format: {Id, {Module, Function, [Args]}, [Options]}
		%% =====================================================================
    
    TabId = ets:new(myTable, []),
    ets:insert(TabId, [
			{?wxID_NEW,{ide,add_editor,[]}},
    	{?wxID_OPEN, {ide,open_file,[Frame]}},
    	{?wxID_SAVE, {ide,save_current_file,[]}},
    	{?wxID_SAVEAS, {ide,save_new,[]}},
    	{?MENU_ID_SAVE_ALL, {}},
    	{?wxID_PRINT, {}},
    	{?wxID_CLOSE, {ide,close_selected_editor,[]} },
    	{?wxID_CLOSE_ALL, {ide,close_all_editors,[]} },
    	{?wxID_EXIT, {}},
    	{?wxID_PREFERENCES, {ide_prefs,start,[[{parent,Frame}]] }},
    
    	{?wxID_UNDO, {}},
    	{?wxID_REDO, {}},
    	{?wxID_CUT, {}},
    	{?wxID_COPY, {}},
    	{?wxID_PASTE, {}},
    	{?wxID_DELETE, {}},
    	{?wxID_FIND, {ide,find_replace,[Frame]}},
    
    	{?MENU_ID_FONT, {ide,update_styles,[Frame]}},
    	{?MENU_ID_FONT_BIGGER, {ide,zoom_in,[]}},
    	{?MENU_ID_FONT_SMALLER, {ide,zoom_out,[]}},
    	{?MENU_ID_LINE_WRAP, {ide,set_line_wrap,[View]}},
    	{?MENU_ID_LN_TOGGLE, {ide,set_line_margin_visible,[View]}},
    	{?MENU_ID_INDENT_TABS, {ide,set_indent_tabs,[View]}},
    	{?MENU_ID_INDENT_SPACES, {ide,set_indent_tabs,[View]}},
    	{?MENU_ID_INDENT_GUIDES, {ide,set_indent_guides,[View]}},		
    	{?MENU_ID_FULLSCREEN, {}},
    	{?MENU_ID_HIDE_TEST, {ide,toggle_pane,[test]}, [{update_label,2}] },
    	{?MENU_ID_HIDE_UTIL, {ide,toggle_pane,[util]}, [{update_label,2}] },
    	{?MENU_ID_MAX_EDITOR, {ide,toggle_pane,[editor]}},
    	{?MENU_ID_MAX_UTIL, {ide,toggle_pane,[maxutil]}},
      
			{?MENU_ID_INDENT_RIGHT, {ide, indent_line_right,[]}},
			{?MENU_ID_INDENT_LEFT, {ide, indent_line_left,[]}},
			{?MENU_ID_TOGGLE_COMMENT, {ide, comment,[]}},
			{?MENU_ID_GOTO_LINE, {ide,go_to_line,[Frame]}},
			{?MENU_ID_FOLD_ALL, {}},
			{?MENU_ID_UNFOLD_ALL, {}},
		
    	{?MENU_ID_WRANGLER, {}},
		
    	{?MENU_ID_COMPILE, {}, [{group, ?MENU_GROUP_ED bor ?MENU_GROUP_WS}]},
    	{?MENU_ID_RUN, {}, [{group, ?MENU_GROUP_ED bor ?MENU_GROUP_WS}]},
    	{?MENU_ID_DIALYZER, {}, [{group, ?MENU_GROUP_ED bor ?MENU_GROUP_WS}]},
    	{?MENU_ID_TESTS, {}, [{group, ?MENU_GROUP_ED bor ?MENU_GROUP_GL}]},
    	{?MENU_ID_DEBUGGER, {}, [{group, ?MENU_GROUP_ED bor ?MENU_GROUP_GL}]},
		
    	{?wxID_HELP, {}},
    	{?MENU_ID_HOTKEYS, {}},
    	{?MENU_ID_SEARCH_DOC, {}},
    	{?MENU_ID_MANUAL, {}},
    	{?wxID_ABOUT, {about, new, [{parent, Frame}]}}
		]),
		
		%% This seems to imply a bug with erlang.
		io:format("Value: X=~p, Y=~p~n", [?MENU_GROUP_WS, ?MENU_GROUP_ED]),
		io:format("X bor Y = ~p~n", [?MENU_GROUP_WS bor ?MENU_GROUP_ED]),
		io:format("Y bor X = ~p~n", [?MENU_GROUP_ED bor ?MENU_GROUP_WS]),
		io:format("2 bor 1 = ~p~n", [2 bor 1]),
		io:format("1 bor 2 = ~p~n", [1 bor 2]),
				
		%% Connect event handlers
	  wxFrame:connect(Frame, menu_highlight,  
			[{userData, {ets_table,TabId}}, {id,?wxID_LOWEST}, {lastId, ?MENU_ID_HIGHEST}]),
	  wxFrame:connect(Frame, menu_close,  [{id,?wxID_LOWEST}, {lastId, ?MENU_ID_HIGHEST}, {callback, fun(_,_) -> io:format("MENU CLOSE~n") end}]),
		wxFrame:connect(Frame, command_menu_selected, 
			[{userData,{ets_table,TabId}}, {id,?wxID_LOWEST}, {lastId, ?MENU_ID_HIGHEST}]),
		%% Submenus
	  wxFrame:connect(Frame, command_menu_selected,  
			[{userData, {theme_menu,Theme}}, {id,?MENU_ID_THEME_LOWEST}, {lastId, ?MENU_ID_THEME_HIGHEST}]),	
	  wxFrame:connect(Frame, command_menu_selected,  
			[{userData, IndentWidth}, {id,?MENU_ID_TAB_WIDTH_LOWEST}, {lastId, ?MENU_ID_TAB_WIDTH_HIGHEST}]),        
  
	  {MenuBar, TabId}.


%% =====================================================================
%% @doc Generate a radio menu given a list of labels.
%% The menu item ids are automatically generated (within their reserved
%% range @see ide.hrl), starting from StartId.
%% The menu item whose label == ToCheck will be checked on.
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
