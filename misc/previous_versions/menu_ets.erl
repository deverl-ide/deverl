%% Replaced on 14/01/14

%% =====================================================================
%% @author Tom Richmond
%% @copyright
%% @title
%% @version < 1
%% @doc This was the original menu. An ETS table was used to store each
%% item and the function it should call. This meant we had an extra module
%% ide_editor_ops to get data prior to calling the intended function.

%% The following event handler in ide.erl extracted the data from the
%% ets table before calling the function:
%% The menu items from the ETS table
% handle_event(E=#wx{id=Id, userData={ets_table, TabId}, event=#wxCommand{type=command_menu_selected}},
%              State) ->
%   Result = case ets:lookup(TabId, Id) of
%     [{_MenuItemID, {Mod, Func, Args}, Options}] ->
%       case proplists:get_value(send_event, Options) of
%         undefined -> {ok, {Mod,Func,Args}};
%         true -> {ok, {Mod,Func,[E]}}
%       end;
%     [{_,{Mod,Func,Args}}] ->
%       {ok, {Mod,Func,Args}};
%     _ -> nomatch
%   end,
%   case Result of
%     {ok,{M,F,A}} ->
%       erlang:apply(M,F,A);
%     nomatch ->
%       io:format("Not yet implemented~n")
%   end,
%   {noreply, State};
%% @end

%% The following functions were responsible for enabling/disabling menu groups:
%% =====================================================================
%% @doc Enable/disable a menu group.
%% Menu items can be associated to one or more menu groups (bit
%% flags, defined in ide.hrl). The Mask is compared using bitwise AND to
%% every menu item. If the menu item has the group bit set then it will
%% be enabled when Toggle is true and disabled when Toggle is false.
% 
% -spec toggle_menu_group(integer(), boolean()) -> ok.
% 
% toggle_menu_group(Mask, Toggle) ->
%   wx_object:cast(?MODULE, {toggle_menu_group, Mask, Toggle}).

% Iterate over the ets table, calling toggle_menu_item/6 on each
% handle_cast({toggle_menu_group, Mask, Toggle}, State=#state{frame=Frame, menu_ets=MenuEts}) ->
%   MenuBar = wxFrame:getMenuBar(Frame),
%   ets:foldl(
%   fun({_Id,_Options}, DontCare) ->
%     DontCare;
%   ({Id,_,Options}, DontCare) ->
%     case proplists:get_value(group, Options) of
%       undefined -> ok;
%       Groups ->
%         toggle_menu_item(MenuBar, wxFrame:getToolBar(Frame), Mask, Id, Groups, Toggle)
%     end,
%     DontCare
%   end, notused, MenuEts),
%   {noreply, State};

%% =====================================================================
%% @doc Enable/disable a menu item.
%% For the menu item identified by Id, Groups is compared to Mask using bitwise AND.
%% If the item has the group bit set then it will be enabled when
%% Enabled is set to true, and disabled when false.
%% @see toggle_menu_group
%% @private

% -spec toggle_menu_item(MenuBar, ToolBar, Mask, Id, Groups, Enable) -> ok when
%   MenuBar :: wxMenuBar:wxMenuBar(),
%   ToolBar :: wxToolBar:wxToolBar(),
%   Mask :: integer(), % Defines which groups to affect
%   Id :: integer(),  % The menu item id
%   Groups :: integer(), % The groups associated to the menu item with Id
%   Enable :: boolean().
% 
% toggle_menu_item(MenuBar, ToolBar, Mask, Id, Groups, Enable) ->
%   case (Mask band Groups) of
%     0 -> ok;
%     _ ->
%       wxMenuItem:enable(wxMenuBar:findItem(MenuBar, Id), [{enable, Enable}]),
%       wxToolBar:enableTool(ToolBar, Id, Enable)
%   end.

%% =====================================================================

-module(ide_menu).

%% Client API
-export([
	create/1,
	update_label/3,
	toggle_item/2,
	get_checked_menu_item/1]).

-include_lib("wx/include/wx.hrl").
-include("ide.hrl").


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
  wxMenu:appendSeparator(Font),

  View        = wxMenu:new([]),
  wxMenu:append(View, ?wxID_ANY, "Font", Font),
  wxMenu:append(View, ?wxID_SEPARATOR, []),
  wxMenu:append(View, ?MENU_ID_LINE_WRAP, "Line Wrap\tCtrl+W", [{kind, ?wxITEM_CHECK}]),
  Pref =
    case ide_sys_pref_gen:get_preference(line_wrap) of
      0 -> false;
			_ -> true
		end,
  wxMenu:check(View, ?MENU_ID_LINE_WRAP, Pref),
  wxMenu:append(View, ?wxID_SEPARATOR, []),
  wxMenu:append(View, ?MENU_ID_LN_TOGGLE, "Toggle Line Numbers\tCtrl+Alt+L", [{kind, ?wxITEM_CHECK}]),
  wxMenu:check(View, ?MENU_ID_LN_TOGGLE, ide_sys_pref_gen:get_preference(show_line_no)),
  wxMenu:append(View, ?wxID_SEPARATOR, []),
  TabPref =
    case ide_sys_pref_gen:get_preference(use_tabs) of
			true -> "Tabs";
			_ -> "Spaces"
		end,
  {IndentType, _MaxId0} = generate_radio_submenu(wxMenu:new([]), ["Tabs", "Spaces"],
  TabPref, ?MENU_ID_INDENT_TABS),

  wxMenu:append(View, ?MENU_ID_INDENT_TYPE, "Indent Type", IndentType),

  {IndentWidth, _MaxId1} = generate_radio_submenu(wxMenu:new([]),
  [integer_to_list(Width) || Width <- lists:seq(2, 8)],
  ide_sys_pref_gen:get_preference(tab_width), ?MENU_ID_TAB_WIDTH_LOWEST),

  wxMenu:append(View, ?MENU_ID_TAB_WIDTH, "Tab Width", IndentWidth),

  {Theme, _MaxId2} = generate_radio_submenu(wxMenu:new([]),
  ide_editor_theme:get_theme_names(), ide_sys_pref_gen:get_preference(theme), ?MENU_ID_THEME_LOWEST),

  wxMenu:append(View, ?MENU_ID_INDENT_GUIDES, "Indent Guides\tCtrl+Alt+G", [{kind, ?wxITEM_CHECK}]),
  wxMenu:check(View, ?MENU_ID_INDENT_GUIDES, ide_sys_pref_gen:get_preference(indent_guides)),
  wxMenu:append(View, ?wxID_SEPARATOR, []),
  wxMenu:append(View, ?MENU_ID_THEME_SELECT, "Theme", Theme),
  wxMenu:append(View, ?wxID_SEPARATOR, []),
  wxMenu:append(View, ?MENU_ID_FULLSCREEN, "Enter Fullscreen\tCtrl+Alt+F", []),
  wxMenu:append(View, ?wxID_SEPARATOR, []),
  wxMenu:append(View, ?MENU_ID_HIDE_TEST, "Toggle Left Pane\tShift+Alt+T", []),
  wxMenu:append(View, ?MENU_ID_HIDE_UTIL, "Toggle Utilities Pane\tShift+Alt+U", []),
  wxMenu:append(View, ?MENU_ID_MAX_EDITOR, "Maximise/Minimise Editor\tAlt+E", []),
  wxMenu:append(View, ?MENU_ID_MAX_UTIL, "Maximise/Minimise Utilities\tAlt+U", []),

  Document    = wxMenu:new([]),
  wxMenu:append(Document, ?MENU_ID_AUTO_INDENT, "Auto-Indent\tCtrl+Alt+I", [{kind, ?wxITEM_CHECK}]),
  wxMenu:check(Document, ?MENU_ID_AUTO_INDENT, ide_sys_pref_gen:get_preference(auto_indent)),
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
	wxMenu:append(Window, ?MENU_ID_PROJECTS_WINDOW, "Browser\tCtrl+1"),
	wxMenu:append(Window, ?MENU_ID_TESTS_WINDOW, "Tests\tCtrl+2"),
	wxMenu:append(Window, ?MENU_ID_FUNC_WINDOW, "Functions\tCtrl+3"),
	wxMenu:append(Window, ?wxID_SEPARATOR, []),
	wxMenu:append(Window, ?MENU_ID_CONSOLE_WINDOW, "Console\tCtrl+4"),
  % wxMenu:append(Window, ?MENU_ID_OBSERVER_WINDOW, "Observer \tCtrl+4"),
	wxMenu:append(Window, ?MENU_ID_DIALYSER_WINDOW, "Dialyser\tCtrl+5"),
	wxMenu:append(Window, ?MENU_ID_DEBUGGER_WINDOW, "Debugger\tCtrl+6"),
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
  wxMenuBar:append(MenuBar, Wrangler, "Wrangler"),
  wxMenuBar:append(MenuBar, ToolMenu, "Tools"),
  wxMenuBar:append(MenuBar, Window, "Display"),
  wxMenuBar:append(MenuBar, Help, "Help"),

	wxFrame:setMenuBar(Frame, MenuBar),

	%% ===================================================================
	%% Toolbar
	%% ===================================================================

	ToolBar = wxFrame:createToolBar(Frame, []),
	% wxToolBar:setMargins(ToolBar, 10, 10),
  % wxToolBar:setToolBitmapSize(ToolBar, {24,24}),
	%% Id, StatusBar help, filename/art id, args, add seperator
	Tools = [  
  		{?wxID_NEW, "ToolTip", {custom, "document_08.png"},    
  			[{shortHelp, "Create a new document"}], false},
  		{?wxID_OPEN, "ToolTip", {custom, "document_06.png"},   
  			[{shortHelp, "Open an existing document"}], false},
  		{?wxID_SAVE, "ToolTip", {custom, "document_12.png"},   
  			[{shortHelp, "Save the current document"}], false}, 
  		{?wxID_CLOSE, "ToolTip", {custom, "document_03.png"},  
  			[{shortHelp, "Close the current document"}], false},
  		{?MENU_ID_NEW_PROJECT, "ToolTip", {custom, "folders_08.png"},    
  			[{shortHelp, "Create a new project"}], false},
  		{?MENU_ID_OPEN_PROJECT, "ToolTip", {custom, "folders_12.png"},   
  			[{shortHelp, "Open an existing project"}], false},
  		{?MENU_ID_SAVE_PROJECT, "ToolTip", {custom, "folders_06.png"},   
  			[{shortHelp, "Save the current project"}], false},			
  		{?MENU_ID_CLOSE_PROJECT, "ToolTip", {custom, "folders_03.png"},  
  			[{shortHelp, "Close the current project"}], true},
  		{?MENU_ID_COMPILE_FILE, "ToolTip", {custom, "document_10.png"},  
  			[{shortHelp, "Compile the current file"}], true},
      {?MENU_ID_MAKE_PROJECT, "ToolTip", {custom, "folders_14.png"},  
  			[{shortHelp, "Make Project"}], false},
  		{?MENU_ID_RUN, "ToolTip", {custom, "folders_10.png"},      
  			[{shortHelp, "Run Project"}], true},
  		{?MENU_ID_HIDE_TEST, "ToolTip", {custom, "application_08.png"},       
  			[{shortHelp, "Toggle left pane visibility"}], false},
  		{?MENU_ID_HIDE_UTIL, "ToolTip", {custom, "application_10.png"},       
  			[{shortHelp, "Toggle utility pane visibility"}], false},
  		{?MENU_ID_MAX_EDITOR, "ToolTip", {custom, "application_03.png"}, 
  			[{shortHelp, "Maximise/restore the text editor"}], false},
  		{?MENU_ID_MAX_UTIL,   "ToolTip", {custom, "application_06.png"},   
  			[{shortHelp, "Maximise/restore the utilities"}], false}],

	AddTool = fun({Id, Tooltip, {custom, File1}, Args, true}) ->
                  wxToolBar:addTool(ToolBar, Id, Tooltip, wxBitmap:new(wxImage:new(ide_lib_widgets:rc_dir(File1))), Args),
                  wxToolBar:addSeparator(ToolBar);
               ({Id, Tooltip, {custom, File1}, Args, false}) ->
                  wxToolBar:addTool(ToolBar, Id, Tooltip, wxBitmap:new(wxImage:new(ide_lib_widgets:rc_dir(File1))), Args);
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
		{?wxID_NEW, {ide_doc_man_wx, new_document, [Frame]}},
		{?MENU_ID_NEW_PROJECT, {ide_proj_man, new_project, [Frame]}},
    {?wxID_OPEN, {ide_doc_man_wx, open_document_dialog,[Frame]}},
		{?MENU_ID_OPEN_PROJECT,{ide_proj_man, open_project_dialog, [Frame]}},
    {?wxID_SAVE, {ide_doc_man_wx, save_active_document,[]},
      [{group, ?MENU_GROUP_NOTEBOOK_EMPTY}]},
    {?wxID_SAVEAS, {ide_doc_man_wx, save_as,[]},
      [{group, ?MENU_GROUP_NOTEBOOK_EMPTY}]},
    {?MENU_ID_SAVE_ALL, {},
      [{group, ?MENU_GROUP_NOTEBOOK_EMPTY}]},
    {?MENU_ID_SAVE_PROJECT, {ide_doc_man_wx, save_active_project, []},
      [{group, ?MENU_GROUP_NOTEBOOK_EMPTY bor ?MENU_GROUP_PROJECTS_EMPTY}]},
    {?wxID_PRINT, {},
      [{group, ?MENU_GROUP_NOTEBOOK_EMPTY}]},
    {?wxID_CLOSE, {ide_doc_man_wx, close_active_document, []},
      [{group, ?MENU_GROUP_NOTEBOOK_EMPTY}]},
    {?wxID_CLOSE_ALL, {ide_doc_man_wx, close_all, []},
      [{group, ?MENU_GROUP_NOTEBOOK_EMPTY}]},
		{?MENU_ID_CLOSE_PROJECT, {ide_proj_man, close_active_project, []},
      [{group, ?MENU_GROUP_PROJECTS_EMPTY}]},
    {?MENU_ID_IMPORT_FILE, {},
      [{group, ?MENU_GROUP_PROJECTS_EMPTY}]},
    {?MENU_ID_IMPORT_PROJECT, {}},
    {?MENU_ID_PROJECT_CONFIG, {ide_proj_man, set_project_configuration, [Frame]},
      [{group, ?MENU_GROUP_PROJECTS_EMPTY}]},
    {?MENU_ID_IMPORT_PROJECT, {ide_proj_man, import, [Frame]}},
    {?wxID_EXIT, {}},
    {?wxID_PREFERENCES, {ide_dlg_prefs_wx, start, [[{parent,Frame}]]}},
    
     %{?wxID_UNDO, {}},
     %{?wxID_REDO, {}},
     %{?wxID_CUT, {}},
     %{?wxID_COPY, {}},
     %{?wxID_PASTE, {}},
     %{?wxID_DELETE, {}},
    
    {?MENU_ID_QUICK_FIND, {ide_editor_ops,quick_find,[]},
      [{group, ?MENU_GROUP_NOTEBOOK_EMPTY}]},
    {?wxID_FIND, {ide_editor_ops,find_replace,[Frame]},
      [{group, ?MENU_GROUP_NOTEBOOK_EMPTY}]},

    {?MENU_ID_FONT,          {ide_editor_ops,update_styles,[Frame]}},
    {?MENU_ID_FONT_BIGGER,   {ide_editor_ops,zoom_in,[]},
      [{group, ?MENU_GROUP_NOTEBOOK_EMPTY}]},
    {?MENU_ID_FONT_SMALLER,  {ide_editor_ops,zoom_out,[]},
      [{group, ?MENU_GROUP_NOTEBOOK_EMPTY}]},
    {?MENU_ID_LINE_WRAP,     {ide_editor_ops,set_line_wrap,[View]}},
    {?MENU_ID_LN_TOGGLE,     {ide_editor_ops,set_line_margin_visible,[View]}},
    {?MENU_ID_INDENT_TABS,   {ide_editor_ops,set_indent_tabs,[]}, [{send_event, true}]},
    {?MENU_ID_INDENT_SPACES, {ide_editor_ops,set_indent_tabs,[]}, [{send_event, true}]},
    {?MENU_ID_INDENT_GUIDES, {ide_editor_ops,set_indent_guides,[View]}},

		{?MENU_ID_INDENT_RIGHT, {ide_editor_ops, indent_right,[]},
      [{group, ?MENU_GROUP_NOTEBOOK_EMPTY}]},
		{?MENU_ID_INDENT_LEFT, {ide_editor_ops, indent_left,[]},
      [{group, ?MENU_GROUP_NOTEBOOK_EMPTY}]},
		{?MENU_ID_TOGGLE_COMMENT, {ide_editor_ops, comment,[]},
      [{group, ?MENU_GROUP_NOTEBOOK_EMPTY}]},
		{?MENU_ID_GOTO_LINE, {ide_editor_ops,go_to_line,[Frame]},
      [{group, ?MENU_GROUP_NOTEBOOK_EMPTY}]},
		{?MENU_ID_UC_SEL, {ide_editor_ops,transform_selection,[]},
      [{send_event, true},
       {group, ?MENU_GROUP_NOTEBOOK_EMPTY}]},
		{?MENU_ID_LC_SEL, {ide_editor_ops,transform_selection,[]},
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
    {?MENU_ID_DIALYZER, {ide, load_xrc, [Frame]}, []},
    {?MENU_ID_TESTS, {dlg_ld, show_dlg, [Frame, new_proj]}, []},
    {?MENU_ID_DEBUGGER, {}, []},

    % {?MENU_ID_PROJECTS_WINDOW, {},
    %       []},
    % {?MENU_ID_TESTS_WINDOW, {},
    %       []},
    % {?MENU_ID_CONSOLE_WINDOW, {},
    %       []},
    %     % {?MENU_ID_OBSERVER_WINDOW, {},
    %     %       []},
    % {?MENU_ID_DIALYSER_WINDOW, {},
    %       []},
    % {?MENU_ID_DEBUGGER_WINDOW, {},
    %       []},

  	{?MENU_ID_NEXT_TAB, {ide_doc_man_wx, set_selection, [right]},
      [{group, ?MENU_GROUP_NOTEBOOK_EMPTY}]},
  	{?MENU_ID_PREV_TAB, {ide_doc_man_wx, set_selection, [left]},
      [{group, ?MENU_GROUP_NOTEBOOK_EMPTY}]},

    {?wxID_HELP, {}},
    {?MENU_ID_HOTKEYS, {}},
    {?MENU_ID_SEARCH_DOC, {}},
    {?MENU_ID_MANUAL, {}},
    {?wxID_ABOUT, {ide_dlg_about_wx, start, [{parent, [Frame]}]}}
		]),

	%% Connect event handlers
	%wxFrame:connect(Frame, menu_highlight,
		%[{userData, {ets_table,TabId}}, {id, ?wxID_LOWEST}, {lastId, ?MENU_ID_HIGHEST}]),
	wxFrame:connect(Frame, command_menu_selected,
		[{userData,{ets_table,TabId}}, {id,?wxID_LOWEST}, {lastId, ?MENU_ID_HIGHEST}]),

	%% Submenus
	wxFrame:connect(Frame, command_menu_selected,
		[{userData, {theme_menu,Theme}}, {id,?MENU_ID_THEME_LOWEST}, {lastId, ?MENU_ID_THEME_HIGHEST}]),
	wxFrame:connect(Frame, command_menu_selected,
		[{userData, IndentWidth}, {id,?MENU_ID_TAB_WIDTH_LOWEST}, {lastId, ?MENU_ID_TAB_WIDTH_HIGHEST}]),

	TabId.

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
	{Menu, Id - 1};

generate_radio_submenu(Menu, [Label|T], Label, StartId) ->
	wxMenu:appendRadioItem(Menu, StartId, Label),
	wxMenu:check(Menu, StartId, true),
	generate_radio_submenu(Menu, T, Label, StartId + 1);

generate_radio_submenu(Menu, [Label|T], ToCheck, StartId) ->
	wxMenu:appendRadioItem(Menu, StartId, Label),
	generate_radio_submenu(Menu, T, ToCheck, StartId + 1).