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
    wxMenu:append(Edit, ?wxID_UNDO, "Undo"),
    wxMenu:append(Edit, ?wxID_REDO, "Redo"),
    wxMenu:append(Edit, ?wxID_SEPARATOR, []),
    wxMenu:append(Edit, ?wxID_CUT, "Cut"),
    wxMenu:append(Edit, ?wxID_COPY, "Copy"), 
    wxMenu:append(Edit, ?wxID_PASTE, "Paste"),
    wxMenu:append(Edit, ?wxID_DELETE, "Delete"),
    wxMenu:appendSeparator(Edit),
    wxMenu:append(Edit, ?wxID_FIND, "Find\tCtrl+F"),
  
    Font = wxMenu:new([]), %% Sub-menu
    wxMenu:append(Font, ?MENU_ID_FONT, "Font Picker"),
    wxMenu:appendSeparator(Font),
    wxMenu:append(Font, ?MENU_ID_FONT_BIGGER, "&Bigger\tCtrl++"),
    wxMenu:append(Font, ?MENU_ID_FONT_SMALLER, "Smaller\tCtrl+-"),
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
			user_prefs:get_user_pref({pref, indent_width}), ?MENU_ID_INDENT_WIDTH_LOWEST),
			
    wxMenu:append(View, ?MENU_ID_INDENT_WIDTH, "Indent Width", IndentWidth),
		
		{Theme, ThemeMax} = generate_radio_submenu(wxMenu:new([]),
			theme:get_theme_names(), user_prefs:get_user_pref({pref, theme}), ?MENU_ID_THEME_LOWEST),
		
    wxMenu:append(View, ?MENU_ID_INDENT_GUIDES, "Indent Guides", [{kind, ?wxITEM_CHECK}]),
    wxMenu:check(View, ?MENU_ID_INDENT_GUIDES, user_prefs:get_user_pref({pref, indent_guides})),
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
    wxMenu:append(Document, ?MENU_ID_AUTO_INDENT, "Auto-Indent", [{kind, ?wxITEM_CHECK}]),
    wxMenu:check(Document, ?MENU_ID_AUTO_INDENT, user_prefs:get_user_pref({pref, auto_indent})),
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
		%% Record format: {Id, {Module, Function, Args}}
		%% =====================================================================
    
    TabId = ets:new(myTable, []),
    ets:insert(TabId,{?wxID_NEW,{ide,add_editor,[]}}),
    ets:insert(TabId,{?wxID_OPEN, {ide,open_file,[Frame]}}),
    ets:insert(TabId,{?wxID_SAVE, {ide,save_current_file,[]}}),
    ets:insert(TabId,{?wxID_SAVEAS, {ide,save_new,[]}}),
    ets:insert(TabId,{?MENU_ID_SAVE_ALL, {}}),
    ets:insert(TabId,{?wxID_PRINT, {}}),
    ets:insert(TabId,{?wxID_CLOSE, {ide, close_selected_editor, []}}),
    ets:insert(TabId,{?wxID_CLOSE_ALL, {ide, close_all_editors, []}}),
    ets:insert(TabId,{?wxID_EXIT, {}}),
    ets:insert(TabId,{?wxID_PREFERENCES, {ide_prefs, start, [[{parent,Frame}]]}}),
    
    ets:insert(TabId,{?wxID_UNDO, {}}),
    ets:insert(TabId,{?wxID_REDO, {}}),
    ets:insert(TabId,{?wxID_CUT, {}}),
    ets:insert(TabId,{?wxID_COPY, {}}),
    ets:insert(TabId,{?wxID_PASTE, {}}),
    ets:insert(TabId,{?wxID_DELETE, {}}),
    ets:insert(TabId,{?wxID_FIND, {ide,find_replace,[Frame]}}),
    
    ets:insert(TabId,{?MENU_ID_FONT, {ide,update_styles,[Frame]}}),
    ets:insert(TabId,{?MENU_ID_FONT_BIGGER, {}}),
    ets:insert(TabId,{?MENU_ID_FONT_SMALLER, {}}),
    ets:insert(TabId,{?MENU_ID_LINE_WRAP, {ide,set_line_wrap,[View]}}),
    ets:insert(TabId,{?MENU_ID_LN_TOGGLE, {ide,set_line_margin_visible,[View]}}),
    ets:insert(TabId,{?MENU_ID_INDENT_TABS, {ide,set_indent_tabs,[View]}}),
    ets:insert(TabId,{?MENU_ID_INDENT_SPACES, {ide,set_indent_tabs,[View]}}),
    ets:insert(TabId,{?MENU_ID_INDENT_GUIDES, {ide,set_indent_guides,[View]}}),		
    ets:insert(TabId,{?MENU_ID_FULLSCREEN, {}}),
    ets:insert(TabId,{?MENU_ID_HIDE_TEST, {ide,toggle_pane,[test]}, {update_label,Frame,2}}),
    ets:insert(TabId,{?MENU_ID_HIDE_UTIL, {ide,toggle_pane,[util]}, {update_label,Frame,2}}),
    ets:insert(TabId,{?MENU_ID_MAX_EDITOR, {ide,toggle_pane,[editor]}}),
    ets:insert(TabId,{?MENU_ID_MAX_UTIL, {ide,toggle_pane,[maxutil]}}),
      
    ets:insert(TabId,{?MENU_ID_AUTO_INDENT, {}}),
    ets:insert(TabId,{?MENU_ID_INDENT_SELECTION, {}}),
    ets:insert(TabId,{?MENU_ID_COMMENT_SELECTION, {}}),
    ets:insert(TabId,{?MENU_ID_FOLD_ALL, {}}),
    ets:insert(TabId,{?MENU_ID_UNFOLD_ALL, {}}),
    ets:insert(TabId,{?MENU_ID_WRANGLER, {}}),
    ets:insert(TabId,{?MENU_ID_COMPILE, {ide, open_dialog, [Frame]}}),
    ets:insert(TabId,{?MENU_ID_RUN, {}}),
    ets:insert(TabId,{?MENU_ID_DIALYZER, {}}),
    ets:insert(TabId,{?MENU_ID_TESTS, {}}),
    ets:insert(TabId,{?MENU_ID_DEBUGGER, {}}),
    ets:insert(TabId,{?wxID_HELP, {}}),
    ets:insert(TabId,{?MENU_ID_SHORTCUTS, {}}),
    ets:insert(TabId,{?MENU_ID_SEARCH_DOC, {}}),
    ets:insert(TabId,{?MENU_ID_MANUAL, {}}),
    ets:insert(TabId,{?wxID_ABOUT, {about, new, [{parent, Frame}]}}),
		
	  wxFrame:connect(Frame, menu_highlight,  
			[{userData, {ets_table,TabId}}, {id,?wxID_LOWEST}, {lastId, ?MENU_ID_HIGHEST}]),
	  wxFrame:connect(Frame, menu_close,  [{id,?wxID_LOWEST}, {lastId, ?MENU_ID_HIGHEST}]),
	  wxFrame:connect(Frame, command_menu_selected, 
			[{userData,{ets_table,TabId}}, {id,?wxID_LOWEST}, {lastId, ?MENU_ID_HIGHEST}]),
		%% Submenus
	  wxFrame:connect(Frame, command_menu_selected,  
			[{userData, {theme_menu,Theme}}, {id,?MENU_ID_THEME_LOWEST}, {lastId, ?MENU_ID_THEME_HIGHEST}]),
	  % wxFrame:connect(Frame, command_menu_selected,  
	  % 			[{userData, {use_tabs,Theme}}, {id,?MENU_ID_THEME_LOWEST}, {lastId, ?MENU_ID_THEME_HIGHEST}]),		
	  wxFrame:connect(Frame, command_menu_selected,  
			[{userData, IndentWidth}, {id,?MENU_ID_INDENT_WIDTH_LOWEST}, {lastId, ?MENU_ID_INDENT_WIDTH_HIGHEST}]),        
  
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
