%% =====================================================================
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%% 
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%% 
%% You should have received a copy of the GNU General Public License
%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%
%% @author Tom Richmond <tr201@kent.ac.uk>
%% @author Mike Quested <mdq3@kent.ac.uk>
%% @copyright Tom Richmond, Mike Quested 2014
%%
%% @doc Builds the frames menu and toolbar.
%% @end
%% =====================================================================


-module(deverl_menu).

%% Client API
-export([
	create/1,
	update_label/3,
	toggle_item/2,
	get_checked_menu_item/1
  ]).

-include_lib("wx/include/wx.hrl").
-include("deverl.hrl").

-define(ICONS, "icons_orig"). %% Specify the subdirectory containing the icon set


%% =====================================================================
%% Client API
%% =====================================================================

%% =====================================================================
%% @doc Start/add a new menubar/toolbar to the frame.

-spec create(list()) -> ets:tid().

create(Config) ->
  init(Config).


%% =====================================================================
%% @doc Update the label of a menu item.

-spec update_label(wxMenuBar:wxMenuBar(), integer(), string()) -> ok.

update_label(Menubar, ItemId, Label) ->
	wxMenuBar:setLabel(Menubar, ItemId, Label).


%% =====================================================================
%% @doc Toggle the enabled status of a menu item.

-spec toggle_item(wxMenuBar:wxMenuBar(), integer()) -> ok.

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

-spec init(list()) -> ets:tid().

init(Config) ->
  Frame = proplists:get_value(parent, Config),
  Mb = build_menu(Frame),
  wxFrame:setMenuBar(Frame, Mb),
  Tb = build_toolbar(Frame),
  % disable_not_implemented(Mb, Tb), %% during dev.
  menu_groups(). %% Returned to deverl


%% =====================================================================
%% @doc Construct the menubar.
 
build_menu(Frame) ->
  MenuBar = wxMenuBar:new(),  
  File = wxMenu:new([]),
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
  wxMenu:append(Edit, ?wxID_UNDO, "Undo\tCtrl+Z"),
  wxMenu:append(Edit, ?wxID_REDO, "Redo\tCtrl+Shift+Z"),
  wxMenu:append(Edit, ?wxID_SEPARATOR, []),
  wxMenu:append(Edit, ?wxID_CUT, "Cut\tCtrl+X"),
  wxMenu:append(Edit, ?wxID_COPY, "Copy\tCtrl+C"),
  wxMenu:append(Edit, ?wxID_PASTE, "Paste\tCtrl+V"),
  wxMenu:append(Edit, ?wxID_DELETE, "Delete"),
  wxMenu:append(Edit, ?wxID_SEPARATOR, []),
  wxMenu:append(Edit, ?wxID_SELECTALL, "Select All\tCtrl+A"),
  wxMenu:appendSeparator(Edit),
  wxMenu:append(Edit, ?MENU_ID_QUICK_FIND, "Quick Find\tCtrl+F"),
  wxMenu:append(Edit, ?wxID_FIND, "Find\tCtrl+Shift+F"),
  
  Font = wxMenu:new([]), %% Sub-menu
  wxMenu:append(Font, ?MENU_ID_FONT, "Choose Font"),
  wxMenu:appendSeparator(Font),
  wxMenu:append(Font, ?MENU_ID_FONT_BIGGER, "Zoom In\tCtrl++"),
  wxMenu:append(Font, ?MENU_ID_FONT_SMALLER, "Zoom Out\tCtrl+-"),

  View        = wxMenu:new([]),
  wxMenu:append(View, ?wxID_ANY, "Font", Font),
  wxMenu:append(View, ?wxID_SEPARATOR, []),
  wxMenu:append(View, ?MENU_ID_LINE_WRAP, "Line Wrap\tCtrl+W", [{kind, ?wxITEM_CHECK}]),
  Pref =
    case deverl_sys_pref_gen:get_preference(line_wrap) of
      0 -> false;
			_ -> true
		end,
  wxMenu:check(View, ?MENU_ID_LINE_WRAP, Pref),
  wxMenu:append(View, ?wxID_SEPARATOR, []),
  wxMenu:append(View, ?MENU_ID_LN_TOGGLE, "Toggle Line Numbers\tCtrl+Alt+L", [{kind, ?wxITEM_CHECK}]),
  wxMenu:check(View, ?MENU_ID_LN_TOGGLE, deverl_sys_pref_gen:get_preference(show_line_no)),
  wxMenu:append(View, ?wxID_SEPARATOR, []),
  TabPref =
    case deverl_sys_pref_gen:get_preference(use_tabs) of
			true -> "Tabs";
			_ -> "Spaces"
		end,
  {IndentType, _MaxId0} = generate_radio_submenu(wxMenu:new([]), ["Tabs", "Spaces"],
  TabPref, ?MENU_ID_INDENT_TABS),

  wxMenu:append(View, ?MENU_ID_INDENT_TYPE, "Indent Type", IndentType),

  {IndentWidth, _MaxId1} = generate_radio_submenu(wxMenu:new([]),
  [integer_to_list(Width) || Width <- lists:seq(2, 8)],
  deverl_sys_pref_gen:get_preference(tab_width), ?MENU_ID_TAB_WIDTH_LOWEST),

  wxMenu:append(View, ?MENU_ID_TAB_WIDTH, "Tab Width", IndentWidth),

  {Theme, _MaxId2} = generate_radio_submenu(wxMenu:new([]),
      deverl_theme:get_theme_names(), deverl_sys_pref_gen:get_preference(theme), ?MENU_ID_THEME_LOWEST),
  wxMenu:appendSeparator(Theme),
  wxMenu:append(Theme, ?MENU_ID_IMPORT_THEME, "Import"),

  {Lang, _MaxId3} = generate_radio_submenu(wxMenu:new([]),
      deverl_editor_wx:get_supported_langs(), "Erlang", ?MENU_ID_LANG_LOWEST),

  wxMenu:append(View, ?MENU_ID_INDENT_GUIDES, "Indent Guides\tCtrl+Alt+G", [{kind, ?wxITEM_CHECK}]),
  wxMenu:check(View, ?MENU_ID_INDENT_GUIDES, deverl_sys_pref_gen:get_preference(indent_guides)),
  wxMenu:append(View, ?wxID_SEPARATOR, []),
  wxMenu:append(View, ?MENU_ID_THEME_SELECT, "Theme", Theme),
  wxMenu:append(View, ?wxID_SEPARATOR, []),
  wxMenu:append(View, ?MENU_ID_LANG_SELECT, "Language", Lang),
  wxMenu:append(View, ?wxID_SEPARATOR, []),
  wxMenu:append(View, ?MENU_ID_FULLSCREEN, "Enter Fullscreen\tCtrl+Alt+F", []),
  wxMenu:append(View, ?wxID_SEPARATOR, []),
  wxMenu:append(View, ?MENU_ID_HIDE_TEST, "Toggle Left Pane\tShift+Alt+T", []),
  wxMenu:append(View, ?MENU_ID_HIDE_UTIL, "Toggle Utilities Pane\tShift+Alt+U", []),
  wxMenu:append(View, ?MENU_ID_HIDE_OUTPUT, "Toggle Output Pane\tShift+Alt+O", []),
  wxMenu:append(View, ?MENU_ID_MAX_EDITOR, "Maximise/Minimise Editor\tAlt+E", []),
  wxMenu:append(View, ?MENU_ID_MAX_UTIL, "Maximise/Minimise Utilities\tAlt+U", []),

  Document    = wxMenu:new([]),
  wxMenu:append(Document, ?MENU_ID_AUTO_INDENT, "Auto-Indent\tCtrl+Alt+I", [{kind, ?wxITEM_CHECK}]),
  wxMenu:check(Document, ?MENU_ID_AUTO_INDENT, deverl_sys_pref_gen:get_preference(auto_indent)),
  wxMenu:append(Document, ?wxID_SEPARATOR, []),
  wxMenu:append(Document, ?MENU_ID_INDENT_RIGHT, "Indent Right\tCtrl+]"),
  wxMenu:append(Document, ?MENU_ID_INDENT_LEFT, "Indent Left\tCtrl+["),
  wxMenu:append(Document, ?wxID_SEPARATOR, []),
  wxMenu:append(Document, ?MENU_ID_TOGGLE_COMMENT, "Comment\tCtrl+/"),
  wxMenu:append(Document, ?wxID_SEPARATOR, []),
  wxMenu:append(Document, ?MENU_ID_UC_SEL, "Uppercase Selection\tCtrl+U"),
  wxMenu:append(Document, ?MENU_ID_LC_SEL, "Lowercase Selection\tCtrl+Shift+U"),
  wxMenu:append(Document, ?wxID_SEPARATOR, []),
  wxMenu:append(Document, ?MENU_ID_STRIP_SPACES, "Strip Trailing Whitespace"),
  wxMenu:append(Document, ?wxID_SEPARATOR, []),
  wxMenu:append(Document, ?MENU_ID_FOLD_ALL, "Fold All"),
  wxMenu:append(Document, ?MENU_ID_UNFOLD_ALL, "Unfold All"),
  wxMenu:append(Document, ?wxID_SEPARATOR, []),
  wxMenu:append(Document, ?MENU_ID_GOTO_LINE, "Go to Line..\tCtrl+L"),

  RunMenu = wxMenu:new([]),
  wxMenu:append(RunMenu, ?MENU_ID_COMPILE_FILE, "Compile File\tF1"),
  wxMenu:append(RunMenu, ?MENU_ID_MAKE_PROJECT, "Make Project"),
  wxMenu:append(RunMenu, ?MENU_ID_RUN, "Run Project"),
  
  Dialyser = wxMenu:new([]),
  wxMenu:append(Dialyser, ?MENU_ID_DIALYZER, "Run Dialyzer\tF3"),
  wxMenu:append(Dialyser, ?MENU_ID_ADD_TO_PLT, "Add to PLT"),
  wxMenu:append(Dialyser, ?MENU_ID_PLT_INFO, "PLT Info"),
  wxMenu:append(Dialyser, ?MENU_ID_DIAL_WARN, "Warning Options"),
  
  Wrangler    = wxMenu:new([]),
  wxMenu:append(Wrangler, ?MENU_ID_WRANGLER, "WRANGLER"),

  ToolMenu    = wxMenu:new([]),
  wxMenu:append(ToolMenu, ?wxID_ANY, "Dialyzer", Dialyser),
  wxMenu:append(ToolMenu, ?wxID_ANY, "Wrangler", Wrangler),
  wxMenu:append(ToolMenu, ?wxID_SEPARATOR, []),
  wxMenu:append(ToolMenu, ?MENU_ID_RUN_TESTS, "Run Tests\tF4"),
  wxMenu:append(ToolMenu, ?MENU_ID_RUN_OBSERVER, "Run Observer\tF5"),
  wxMenu:append(ToolMenu, ?MENU_ID_RUN_DEBUGGER, "Run Debugger\tF6"),

	Window      = wxMenu:new([]),
	wxMenu:append(Window, ?MENU_ID_PROJECTS_WINDOW, "Browser\tCtrl+1"),
	wxMenu:append(Window, ?MENU_ID_TESTS_WINDOW, "Tests\tCtrl+2"),
	wxMenu:append(Window, ?MENU_ID_FUNC_WINDOW, "Functions\tCtrl+3"),
	wxMenu:append(Window, ?wxID_SEPARATOR, []),
	wxMenu:append(Window, ?MENU_ID_LOG_WINDOW, "Log\tCtrl+4"),
	wxMenu:append(Window, ?MENU_ID_OUTPUT_WINDOW, "Output\tCtrl+5"),
	wxMenu:append(Window, ?wxID_SEPARATOR, []),
	wxMenu:append(Window, ?MENU_ID_NEXT_TAB, "Next Tab\tCtrl+}"),
	wxMenu:append(Window, ?MENU_ID_PREV_TAB, "Previous Tab\tCtrl+{"),

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
  wxMenuBar:append(MenuBar, RunMenu, "Run"),
  wxMenuBar:append(MenuBar, ToolMenu, "Tools"),
  wxMenuBar:append(MenuBar, Window, "Display"),
  wxMenuBar:append(MenuBar, Help, "Help"),
  
  wxFrame:connect(Frame, command_menu_selected), 
  MenuBar.


%% =====================================================================
%% @doc Construct the toolbar.

build_toolbar(Frame) ->
	ToolBar = wxFrame:createToolBar(Frame, []),
  wxToolBar:setToolBitmapSize(ToolBar, {24,24}),
	%% Id, StatusBar help, filename/art id, args, add seperator
	Tools = [  
      {?wxID_NEW, "ToolTip", {custom, "document_new.png"},    
  			[{shortHelp, "Create a new document"}], false},
      {?wxID_OPEN, "ToolTip", {custom, "document_open.png"},   
  			[{shortHelp, "Open an existing document"}], false},
      {?wxID_SAVE, "ToolTip", {custom, "document_save.png"},   
  			[{shortHelp, "Save the current document"}], false}, 
      {?wxID_CLOSE, "ToolTip", {custom, "document_close.png"},  
  			[{shortHelp, "Close the current document"}], false},
      {?MENU_ID_NEW_PROJECT, "ToolTip", {custom, "project_new.png"},    
  			[{shortHelp, "Create a new project"}], false},
      {?MENU_ID_OPEN_PROJECT, "ToolTip", {custom, "project_open.png"},   
  			[{shortHelp, "Open an existing project"}], false},
      {?MENU_ID_SAVE_PROJECT, "ToolTip", {custom, "project_save.png"},   
  			[{shortHelp, "Save the current project"}], false},			
      {?MENU_ID_CLOSE_PROJECT, "ToolTip", {custom, "project_close.png"},  
  			[{shortHelp, "Close the current project"}], true},
  		{?MENU_ID_COMPILE_FILE, "ToolTip", {custom, "compile.png"},  
  			[{shortHelp, "Compile the current file"}], false},
      {?MENU_ID_MAKE_PROJECT, "ToolTip", {custom, "project_build.png"},  
  			[{shortHelp, "Make Project"}], false},
  		{?MENU_ID_RUN, "ToolTip", {custom, "project_run.png"},      
  			[{shortHelp, "Run Project"}], true},
  		{?MENU_ID_DIALYZER, "ToolTip", {custom, "dialyzer.png"},      
  			[{shortHelp, "Run Dialyzer"}], false},
  		{?MENU_ID_RUN_TESTS, "ToolTip", {custom, "tests.png"},      
  			[{shortHelp, "Run Tests"}], false},
  		{?MENU_ID_RUN_OBSERVER, "ToolTip", {custom, "observer.png"},      
  			[{shortHelp, "Run Observer"}], true},
      {?MENU_ID_HIDE_TEST, "ToolTip", {custom, "application_08.png"},       
  			[{shortHelp, "Toggle left pane visibility"}], false},
      {?MENU_ID_HIDE_UTIL, "ToolTip", {custom, "application_10.png"},       
  			[{shortHelp, "Toggle utility pane visibility"}], false},
  		{?MENU_ID_MAX_EDITOR, "ToolTip", {custom, "application_03.png"}, 
  			[{shortHelp, "Maximise/restore the text editor"}], false},
  		{?MENU_ID_MAX_UTIL,   "ToolTip", {custom, "application_06.png"},   
  			[{shortHelp, "Maximise/restore the utilities"}], false}],

	AddTool = fun({Id, Tooltip, {custom, File1}, Args, true}) ->
                  wxToolBar:addTool(ToolBar, Id, Tooltip, wxBitmap:new(wxImage:new(deverl_lib_widgets:rc_dir(filename:join(?ICONS, File1)))), Args),
                  wxToolBar:addSeparator(ToolBar);
               ({Id, Tooltip, {custom, File1}, Args, false}) ->
                  wxToolBar:addTool(ToolBar, Id, Tooltip, wxBitmap:new(wxImage:new(deverl_lib_widgets:rc_dir(filename:join(?ICONS, File1)))), Args);
               ({Id, Tooltip, {default, Art}, Args, true}) ->
                  wxToolBar:addTool(ToolBar, Id, Tooltip, wxArtProvider:getBitmap(Art), Args),
                  wxToolBar:addSeparator(ToolBar);
               ({Id, Tooltip, {default, Art}, Args, false}) ->
                  wxToolBar:addTool(ToolBar, Id, Tooltip, wxArtProvider:getBitmap(Art), Args)
            end,  

	[AddTool(Tool) || Tool <- Tools],
    
	wxToolBar:realize(ToolBar).


%% =====================================================================
%% @doc Used for enabling/disabling menu items with a single group id.
  
menu_groups() ->
  Groups = [
    {?MENU_GROUP_NOTEBOOK_EMPTY, [?wxID_SAVE, ?wxID_SAVEAS, ?MENU_ID_SAVE_ALL, 
                                  ?MENU_ID_SAVE_PROJECT, ?wxID_CLOSE, 
                                  ?wxID_CLOSE_ALL, ?MENU_ID_QUICK_FIND,
                                  ?MENU_ID_FONT_BIGGER, ?MENU_ID_FONT_SMALLER, ?MENU_ID_INDENT_RIGHT,
                                  ?MENU_ID_INDENT_LEFT, ?MENU_ID_TOGGLE_COMMENT, ?MENU_ID_GOTO_LINE,
                                  ?MENU_ID_UC_SEL, ?MENU_ID_LC_SEL, 
                                  ?MENU_ID_STRIP_SPACES, ?MENU_ID_COMPILE_FILE, 
                                  ?MENU_ID_MAKE_PROJECT, ?MENU_ID_RUN, ?MENU_ID_NEXT_TAB, 
                                  ?MENU_ID_PREV_TAB, ?MENU_ID_RUN_TESTS, ?MENU_ID_DIALYZER]},
    {?MENU_GROUP_PROJECTS_EMPTY, [?MENU_ID_CLOSE_PROJECT, ?MENU_ID_PROJECT_CONFIG,
                                  ?MENU_ID_MAKE_PROJECT, ?MENU_ID_RUN, ?MENU_ID_SAVE_PROJECT]},
    {?MENU_GROUP_NOTEBOOK_KILL_FOCUS, [?wxID_UNDO, ?wxID_REDO, ?wxID_SAVE, ?wxID_SAVEAS, ?wxID_CLOSE, ?MENU_ID_QUICK_FIND]},
    {?MENU_GROUP_NOTEBOOK_SET_FOCUS, [?wxID_SELECTALL, ?wxID_SAVEAS, ?wxID_CLOSE, ?MENU_ID_QUICK_FIND]},
    {?MENU_GROUP_TEXT, [?wxID_CUT, ?wxID_COPY, ?wxID_PASTE, ?wxID_SELECTALL, ?wxID_DELETE]},
    {?MENU_GROUP_ERL, [?MENU_ID_RUN_OBSERVER, ?MENU_ID_DIALYZER, ?MENU_ID_COMPILE_FILE, 
                       ?MENU_ID_MAKE_PROJECT, ?MENU_ID_RUN, ?MENU_ID_RUN_DEBUGGER, ?MENU_ID_RUN_TESTS]}
  ].
  
  
%% =====================================================================
%% @doc Generate a radio menu given a list of labels.
%% The menu item ids are automatically generated (within their reserved
%% range @see deverl.hrl), starting from StartId.
%% The menu item whose label == ToCheck will be checked on.
%% @private

-spec generate_radio_submenu(Menu, Items, ToCheck, StartId) -> Result when
	Menu :: wxMenu:wxMenu(),
	Items :: [string()],
	ToCheck :: string(),
	StartId :: integer(),
	Result :: {wxMenu:wxMenu(), integer()}. %% The complete menu and id of the last item added

generate_radio_submenu(Menu, [], _, Id) ->
	{Menu, Id - 1};

generate_radio_submenu(Menu, [Label|T], Label, StartId) ->
	wxMenu:appendRadioItem(Menu, StartId, Label),
	wxMenu:check(Menu, StartId, true),
	generate_radio_submenu(Menu, T, Label, StartId + 1);

generate_radio_submenu(Menu, [Label|T], ToCheck, StartId) ->
	wxMenu:appendRadioItem(Menu, StartId, Label),
	generate_radio_submenu(Menu, T, ToCheck, StartId + 1).


%% =====================================================================
%% @doc 

disable_not_implemented(MenuBar, ToolBar) ->
  ToDisable = [?MENU_ID_FOLD_ALL,
               ?MENU_ID_UNFOLD_ALL ,
               ?MENU_ID_ADD_TO_PLT,
               ?MENU_ID_PLT_INFO,
               ?MENU_ID_DIAL_WARN,
               ?MENU_ID_WRANGLER,
               ?MENU_ID_MANUAL,
               ?MENU_ID_HOTKEYS,
               ?wxID_FIND,
               ?wxID_PRINT,
               ?MENU_ID_EXPORT_EDOC,
               ?MENU_ID_IMPORT_FILE
               ],
  Disable = fun(Item) ->
    wxMenuItem:enable(wxMenuBar:findItem(MenuBar, Item), [{enable, false}])
    % wxToolBar:enableTool(ToolBar, Item, false)
  end,
  [Disable(Id) || Id <- ToDisable],
  ok.
