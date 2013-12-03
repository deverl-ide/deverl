%% =====================================================================
%% @author
%% @copyright
%% @title
%% @version
%% @doc
%% @end
%% =====================================================================

-module(ide_menu).

%% Client API
-export([
	create/1, 
	update_label/3,
	toggle_item/2,
	get_checked_menu_item/1]). 
         
-include_lib("wx/include/wx.hrl").
-include("../include/ide.hrl").


%% =====================================================================
%% Client API
%% =====================================================================

%% =====================================================================
%% @doc Start/add a new menubar/toolbar to the frame.

create(Config) ->
  init(Config).


%% =====================================================================
%% @doc Update the label of a menu item.

update_label(Menubar, ItemId, Label) ->
	wxMenuBar:setLabel(Menubar, ItemId, Label).


%% =====================================================================
%% @doc Toggle the enabled status of a menu item.
	
toggle_item(Menubar, ItemId) ->
	wxMenuBar:enable(Menubar, ItemId, not wxMenuBar:isEnabled(Menubar, ItemId)).


%% =====================================================================
%% @doc Get the checked item in a radio menu

-spec get_checked_menu_item(MenuItems) -> Result when
	MenuItems :: [wxMenuItem:wxMenuItem()],
	Result :: {'ok', wxMenuItem:wxMenuItem()}.

get_checked_menu_item([]) ->
  {error, nomatch};
get_checked_menu_item([H|T]) ->
  case wxMenuItem:isChecked(H) of
    true ->
      {ok, H};
    _ ->
      get_checked_menu_item(T)
  end.
    
    
%% =====================================================================
%% Internal functions
%% =====================================================================

%% =====================================================================
%% @doc Initialise the menubar/toolbar.

init(Config) ->
  Frame = proplists:get_value(parent, Config),
    
	%% =====================================================================
	%% Menubar
	%% 
	%% =====================================================================

  MenuBar     = wxMenuBar:new(),
      
  File        = wxMenu:new([]),
  wxMenu:append(File, ?wxID_NEW, "New File\tCtrl+N"),
  wxMenu:append(File, ?MENU_ID_NEW_PROJECT, "New Project\tCtrl+Alt+N"),
  wxMenu:append(File, ?wxID_SEPARATOR, []),
  wxMenu:append(File, ?wxID_OPEN, "Open File\tCtrl+O"),
  wxMenu:append(File, ?MENU_ID_OPEN_PROJECT, "Open Project\tCtrl+Alt+O"),
  wxMenu:append(File, ?wxID_SEPARATOR, []),
  wxMenu:append(File, ?wxID_SAVE, "Save\tCtrl+S"),
  wxMenu:append(File, ?wxID_SAVEAS, "Save As\tCtrl+Shift+S"),
  wxMenu:append(File, ?MENU_ID_SAVE_PROJECT, "Save Project\tCtrl+Alt+S"),
  wxMenu:append(File, ?MENU_ID_SAVE_ALL, "Save All\tCtrl+Alt+Shift+S"),
  wxMenu:append(File, ?wxID_SEPARATOR, []),
  wxMenu:append(File, ?wxID_CLOSE, "Close File"),
  wxMenu:append(File, ?wxID_CLOSE_ALL, "Close All"),
  wxMenu:append(File, ?MENU_ID_CLOSE_PROJECT, "Close Project"),
  wxMenu:append(File, ?wxID_SEPARATOR, []),
  
  Import = wxMenu:new([]),
  wxMenu:append(Import, ?MENU_ID_IMPORT_FILE, "Import File"),
  wxMenu:append(Import, ?MENU_ID_IMPORT_PROJECT, "Import Erlang Project"),
  wxMenu:append(File, ?wxID_ANY, "Import", Import),
  
  Export = wxMenu:new([]),
  wxMenu:append(Export, ?MENU_ID_EXPORT_EDOC, "Export eDoc"),
  wxMenu:append(File, ?wxID_ANY, "Export", Export),
  
  wxMenu:append(File, ?wxID_SEPARATOR, []),
  wxMenu:append(File, ?MENU_ID_PROJECT_CONFIG, "Project Configuration\tCtrl+Alt+Shift+P"),
  wxMenu:append(File, ?wxID_SEPARATOR, []),
  wxMenu:append(File, ?wxID_PRINT, "Print\tCtrl+P"),
  case os:type() of
    {_, darwin} ->
      ok;
    _ ->
      wxMenu:append(File, ?wxID_SEPARATOR, [])
  end,
  wxMenu:append(File, ?wxID_EXIT, "Exit\tCtrl+Q"),
  wxMenu:append(File, ?wxID_PREFERENCES, "Preferences"),

  
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
  wxMenu:append(View, ?MENU_ID_LINE_WRAP, "Line Wrap\tCtrl+W", [{kind, ?wxITEM_CHECK}]),
  Pref = 
    case sys_pref_manager:get_preference(line_wrap) of
      0 -> false;
			_ -> true
		end,
  wxMenu:check(View, ?MENU_ID_LINE_WRAP, Pref),
  wxMenu:append(View, ?wxID_SEPARATOR, []),
  wxMenu:append(View, ?MENU_ID_LN_TOGGLE, "Toggle Line Numbers\tCtrl+Alt+L", [{kind, ?wxITEM_CHECK}]),
  wxMenu:check(View, ?MENU_ID_LN_TOGGLE, sys_pref_manager:get_preference(show_line_no)),
  wxMenu:append(View, ?wxID_SEPARATOR, []),
  TabPref = 
    case sys_pref_manager:get_preference(use_tabs) of
			true -> "Tabs";
			_ -> "Spaces"
		end,
  {IndentType, _} = generate_radio_submenu(wxMenu:new([]), ["Tabs", "Spaces"],
  TabPref, ?MENU_ID_INDENT_TABS),
		
  wxMenu:append(View, ?MENU_ID_INDENT_TYPE, "Indent Type", IndentType),
		
  {IndentWidth, IndentMax} = generate_radio_submenu(wxMenu:new([]),
  [integer_to_list(Width) || Width <- lists:seq(2, 8)], 
  sys_pref_manager:get_preference(tab_width), ?MENU_ID_TAB_WIDTH_LOWEST),
			
  wxMenu:append(View, ?MENU_ID_TAB_WIDTH, "Tab Width", IndentWidth),
		
  {Theme, ThemeMax} = generate_radio_submenu(wxMenu:new([]),
  editor_theme:get_theme_names(), sys_pref_manager:get_preference(theme), ?MENU_ID_THEME_LOWEST),
		
  wxMenu:append(View, ?MENU_ID_INDENT_GUIDES, "Indent Guides\tCtrl+Alt+G", [{kind, ?wxITEM_CHECK}]),
  wxMenu:check(View, ?MENU_ID_INDENT_GUIDES, sys_pref_manager:get_preference(indent_guides)),
  wxMenu:append(View, ?wxID_SEPARATOR, []),
  wxMenu:append(View, ?MENU_ID_THEME_SELECT, "Theme", Theme),
  wxMenu:append(View, ?wxID_SEPARATOR, []),
  wxMenu:append(View, ?MENU_ID_FULLSCREEN, "Enter Fullscreen\tCtrl+Alt+F", []),
  wxMenu:append(View, ?wxID_SEPARATOR, []),
  wxMenu:append(View, ?MENU_ID_HIDE_TEST, "Hide Test Pane\tShift+Alt+T", []),
  wxMenu:append(View, ?MENU_ID_HIDE_UTIL, "Hide Utilities Pane\tShift+Alt+U", []),
  wxMenu:append(View, ?MENU_ID_MAX_EDITOR, "Maximise Editor\tAlt+E", []),
  wxMenu:append(View, ?MENU_ID_MAX_UTIL, "Maximise Utilities\tAlt+U", []),
  
  Document    = wxMenu:new([]),	
  wxMenu:append(Document, ?MENU_ID_AUTO_INDENT, "Auto-Indent\tCtrl+Alt+I", [{kind, ?wxITEM_CHECK}]),
  wxMenu:check(Document, ?MENU_ID_AUTO_INDENT, sys_pref_manager:get_preference(auto_indent)),
  wxMenu:append(Document, ?wxID_SEPARATOR, []),
  wxMenu:append(Document, ?MENU_ID_INDENT_RIGHT, "Indent Right\tCtrl+]"),
  wxMenu:append(Document, ?MENU_ID_INDENT_LEFT, "Indent Left\tCtrl+["),
  wxMenu:append(Document, ?wxID_SEPARATOR, []),
  wxMenu:append(Document, ?MENU_ID_TOGGLE_COMMENT, "Comment\tCtrl+/"),
  wxMenu:append(Document, ?wxID_SEPARATOR, []),
  wxMenu:append(Document, ?MENU_ID_UC_SEL, "Uppercase Selection\tCtrl+U"),
  wxMenu:append(Document, ?MENU_ID_LC_SEL, "Lowercase Selection\tCtrl+Shift+U"),
  wxMenu:append(Document, ?wxID_SEPARATOR, []),
  wxMenu:append(Document, ?MENU_ID_FOLD_ALL, "Fold All"),
  wxMenu:append(Document, ?MENU_ID_UNFOLD_ALL, "Unfold All"),
  wxMenu:append(Document, ?wxID_SEPARATOR, []),	
  wxMenu:append(Document, ?MENU_ID_GOTO_LINE, "Go to Line..\tCtrl+L"),
  
  Wrangler    = wxMenu:new([]),
  wxMenu:append(Wrangler, ?MENU_ID_WRANGLER, "WRANGLER"),

  ToolMenu    = wxMenu:new([]),
  wxMenu:append(ToolMenu, ?MENU_ID_COMPILE_FILE, "Compile File\tF1"),
  wxMenu:append(ToolMenu, ?MENU_ID_MAKE_PROJECT, "Make Project"),
  wxMenu:append(ToolMenu, ?MENU_ID_RUN, "Run Project"),
  wxMenu:append(ToolMenu, ?wxID_SEPARATOR, []),
  wxMenu:append(ToolMenu, ?MENU_ID_DIALYZER, "Run Dialyzer\tF3"),
  wxMenu:append(ToolMenu, ?MENU_ID_TESTS, "Run Tests\tF4"),
  wxMenu:append(ToolMenu, ?MENU_ID_DEBUGGER, "Run Debugger\tF5"),
  
	Window      = wxMenu:new([]),
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
    
	%% ===================================================================
	%% Toolbar
	%% ===================================================================

	ToolBar = wxFrame:createToolBar(Frame, []),
	% wxToolBar:setMargins(ToolBar, 10, 10),
	wxToolBar:setToolBitmapSize(ToolBar, {24,24}),
	%% Id, StatusBar help, filename/art id, args, add seperator
	Tools = [
		{?wxID_NEW, "ToolTip", {custom, "../icons/document_08.png"},    
			[{shortHelp, "Create a new document"}], false},
		{?wxID_OPEN, "ToolTip", {custom, "../icons/document_06.png"},   
			[{shortHelp, "Open an existing document"}], false},
		{?wxID_SAVE, "ToolTip", {custom, "../icons/document_12.png"},   
			[{shortHelp, "Save the current document"}], false}, 
		{?wxID_CLOSE, "ToolTip", {custom, "../icons/document_03.png"},  
			[{shortHelp, "Close the current document"}], false},
		{?MENU_ID_NEW_PROJECT, "ToolTip", {custom, "../icons/folders_08.png"},    
			[{shortHelp, "Create a new project"}], false},
		{?MENU_ID_OPEN_PROJECT, "ToolTip", {custom, "../icons/folders_12.png"},   
			[{shortHelp, "Open an existing project"}], false},
		{?MENU_ID_SAVE_PROJECT, "ToolTip", {custom, "../icons/folders_06.png"},   
			[{shortHelp, "Save the current project"}], false},			
		{?MENU_ID_CLOSE_PROJECT, "ToolTip", {custom, "../icons/folders_03.png"},  
			[{shortHelp, "Close the current project"}], true},
		{?MENU_ID_COMPILE_FILE, "ToolTip", {custom, "../icons/document_10.png"},  
			[{shortHelp, "Compile the current file"}],        true},
    {?MENU_ID_MAKE_PROJECT, "ToolTip", {custom, "../icons/folders_14.png"},  
			[{shortHelp, "Make Project"}],        false},
		{?MENU_ID_RUN, "ToolTip", {custom, "../icons/folders_10.png"},      
			[{shortHelp, "Run Project"}],            true},
		{?MENU_ID_HIDE_TEST, "ToolTip", {custom, "../icons/application_08.png"},       
			[{shortHelp, "Toggle left pane visibility"}],              false},
		{?MENU_ID_HIDE_UTIL, "ToolTip", {custom, "../icons/application_10.png"},       
			[{shortHelp, "Toggle utility pane visibility"}],         false},
		{?MENU_ID_MAX_EDITOR, "ToolTip", {custom, "../icons/application_03.png"}, 
			[{shortHelp, "Maximise/restore the text editor"}], false},
		{?MENU_ID_MAX_UTIL,   "ToolTip", {custom, "../icons/application_06.png"},   
			[{shortHelp, "Maximise/restore the utilities"}],   false}],

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
		
  %% ===================================================================
  %% ETS menu events table.
  %% Record format: {Id, {Module, Function, [Args]}, [Options]}
  %% When Options can be 0 or more of:
  %% 		
  %%		{send_event, true}	|	
  %%		{help_string, HelpString}	| {group, Groups}	
  %%
  %%		HelpString :: string(), % help string for status bar
  %%		Groups :: integer(), % any menu groups to which the menu item belongs (combine by adding)
  %%		Use send_event to forward the event record to Function
  %% ===================================================================  
  
  TabId = ets:new(myTable, []),
  ets:insert(TabId, [
		{?wxID_NEW, {doc_manager, new_document, [Frame]}},
		{?MENU_ID_NEW_PROJECT, {project_manager, new_project, [Frame]}},
    {?wxID_OPEN, {doc_manager, open_document_dialog,[Frame]}},
		{?MENU_ID_OPEN_PROJECT,{project_manager, open_project_dialog, [Frame]}},
    {?wxID_SAVE, {doc_manager, save_active_document,[]},
      [{group, ?MENU_GROUP_NOTEBOOK_EMPTY}]},
    {?wxID_SAVEAS, {doc_manager, save_as,[]},
      [{group, ?MENU_GROUP_NOTEBOOK_EMPTY}]},
    {?MENU_ID_SAVE_ALL, {},
      [{group, ?MENU_GROUP_NOTEBOOK_EMPTY}]},
    {?MENU_ID_SAVE_PROJECT, {doc_manager, save_active_project, []},
      [{group, ?MENU_GROUP_NOTEBOOK_EMPTY bor ?MENU_GROUP_PROJECTS_EMPTY}]},
    {?wxID_PRINT, {},
      [{group, ?MENU_GROUP_NOTEBOOK_EMPTY}]},
    {?wxID_CLOSE, {doc_manager, close_active_document, []},
      [{group, ?MENU_GROUP_NOTEBOOK_EMPTY}]},
    {?wxID_CLOSE_ALL, {doc_manager, close_all, []},
      [{group, ?MENU_GROUP_NOTEBOOK_EMPTY}]},
		{?MENU_ID_CLOSE_PROJECT, {project_manager, close_active_project, []},
      [{group, ?MENU_GROUP_PROJECTS_EMPTY}]},
    {?MENU_ID_IMPORT_FILE, {},
      [{group, ?MENU_GROUP_PROJECTS_EMPTY}]},
    {?MENU_ID_IMPORT_PROJECT, {}},
    {?MENU_ID_PROJECT_CONFIG, {project_manager, set_project_configuration, [Frame]},
      [{group, ?MENU_GROUP_PROJECTS_EMPTY}]},
    {?MENU_ID_IMPORT_PROJECT, {project_manager, import, [Frame]}},
    {?wxID_EXIT, {}},
    {?wxID_PREFERENCES, {ide_prefs,start, [[{parent,Frame}]]}},
    
    % {?wxID_UNDO, {}},
    % {?wxID_REDO, {}},
    % {?wxID_CUT, {}},
    % {?wxID_COPY, {}},
    % {?wxID_PASTE, {}},
    % {?wxID_DELETE, {}},
    {?wxID_FIND, {editor_ops,find_replace,[Frame]},
      [{group, ?MENU_GROUP_NOTEBOOK_EMPTY}]},
    
    {?MENU_ID_FONT,          {editor_ops,update_styles,[Frame]}},
    {?MENU_ID_FONT_BIGGER,   {editor_ops,zoom_in,[]},
      [{group, ?MENU_GROUP_NOTEBOOK_EMPTY}]},
    {?MENU_ID_FONT_SMALLER,  {editor_ops,zoom_out,[]},
      [{group, ?MENU_GROUP_NOTEBOOK_EMPTY}]},
    {?MENU_ID_LINE_WRAP,     {editor_ops,set_line_wrap,[View]}},
    {?MENU_ID_LN_TOGGLE,     {editor_ops,set_line_margin_visible,[View]}},
    {?MENU_ID_INDENT_TABS,   {editor_ops,set_indent_tabs,[]}, [{send_event, true}]},
    {?MENU_ID_INDENT_SPACES, {editor_ops,set_indent_tabs,[]}, [{send_event, true}]},
    {?MENU_ID_INDENT_GUIDES, {editor_ops,set_indent_guides,[View]}},		
      
		{?MENU_ID_INDENT_RIGHT, {editor_ops, indent_right,[]},
      [{group, ?MENU_GROUP_NOTEBOOK_EMPTY}]},
		{?MENU_ID_INDENT_LEFT, {editor_ops, indent_left,[]},
      [{group, ?MENU_GROUP_NOTEBOOK_EMPTY}]},
		{?MENU_ID_TOGGLE_COMMENT, {editor_ops, comment,[]},
      [{group, ?MENU_GROUP_NOTEBOOK_EMPTY}]},
		{?MENU_ID_GOTO_LINE, {editor_ops,go_to_line,[Frame]},
      [{group, ?MENU_GROUP_NOTEBOOK_EMPTY}]},
		{?MENU_ID_UC_SEL, {editor_ops,transform_selection,[]}, 
      [{send_event, true},
       {group, ?MENU_GROUP_NOTEBOOK_EMPTY}]},
		{?MENU_ID_LC_SEL, {editor_ops,transform_selection,[]}, 
      [{send_event, true},
       {group, ?MENU_GROUP_NOTEBOOK_EMPTY}]},			
		{?MENU_ID_FOLD_ALL, {},
      [{group, ?MENU_GROUP_NOTEBOOK_EMPTY}]},
		{?MENU_ID_UNFOLD_ALL, {},
      [{group, ?MENU_GROUP_NOTEBOOK_EMPTY}]},
		
    {?MENU_ID_WRANGLER, {},
      [{group, ?MENU_GROUP_NOTEBOOK_EMPTY}]},
		
    {?MENU_ID_COMPILE_FILE, {ide_build, compile_file,[]},
      [{group, ?MENU_GROUP_NOTEBOOK_EMPTY}]},
    {?MENU_ID_MAKE_PROJECT, {ide_build, make_project, []},
      [{group, ?MENU_GROUP_NOTEBOOK_EMPTY bor ?MENU_GROUP_PROJECTS_EMPTY}]},
    {?MENU_ID_RUN, {ide_build, run_project, [Frame]},
      [{group, ?MENU_GROUP_NOTEBOOK_EMPTY bor ?MENU_GROUP_PROJECTS_EMPTY}]},
    {?MENU_ID_DIALYZER, {}, []},
    {?MENU_ID_TESTS, {}, []},
    {?MENU_ID_DEBUGGER, {}, []},
    
  	{?MENU_ID_PROJECTS_WINDOW, {}, 
      []},
  	{?MENU_ID_TESTS_WINDOW, {}, 
      []},
  	{?MENU_ID_CONSOLE_WINDOW, {}, 
      []},
  	{?MENU_ID_OBSERVER_WINDOW, {}, 
      []},
  	{?MENU_ID_DIALYSER_WINDOW, {doc_manager, new_document, [Frame]}, 
      []},
  	{?MENU_ID_DEBUGGER_WINDOW, {}, 
      []},
  	{?MENU_ID_NEXT_TAB, {doc_manager, new_document, [Frame]}, 
      [{group, ?MENU_GROUP_NOTEBOOK_EMPTY}]},
  	{?MENU_ID_PREV_TAB, {}, 
      [{group, ?MENU_GROUP_NOTEBOOK_EMPTY}]},
  
    {?wxID_HELP, {}},
    {?MENU_ID_HOTKEYS, {}},
    {?MENU_ID_SEARCH_DOC, {}},
    {?MENU_ID_MANUAL, {}},
    {?wxID_ABOUT, {about, new, [{parent, Frame}]}}
		]),
				
	%% Connect event handlers
	wxFrame:connect(Frame, menu_highlight,  
		[{userData, {ets_table,TabId}}, {id,?wxID_LOWEST}, {lastId, ?MENU_ID_HIGHEST}]),
	wxFrame:connect(Frame, command_menu_selected, 
		[{userData,{ets_table,TabId}}, {id,?MENU_ID_LOWEST}, {lastId, ?MENU_ID_HIGHEST}]),
		
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
	generate_radio_submenu(Menu, T, Label, StartId + 1);
	
generate_radio_submenu(Menu, [Label|T], ToCheck, StartId) ->
	wxMenu:appendRadioItem(Menu, StartId, Label),
	generate_radio_submenu(Menu, T, ToCheck, StartId + 1).