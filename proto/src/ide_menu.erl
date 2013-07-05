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
    
    File        = wxMenu:new([]),
    Edit        = wxMenu:new([]),
    View        = wxMenu:new([]),
    Document    = wxMenu:new([]),
    Wrangler    = wxMenu:new([]),
    Tools       = wxMenu:new([]),
    Help        = wxMenu:new([]),
    
    Font        = wxMenu:new([]),
	IndentType  = wxMenu:new([]),
    IndentWidth = wxMenu:new([]),
    
    %% Format of table entry:
    %% (TabId, {ItemId, {Label, AltLabel}, HelpString, Separator, ParentMenu, {Type, Checked}, [{Module, Function, [Args]}]})
   
    %%%%%%%%%%%%%%%%%%%%%
    %%%%% ETS Table %%%%%
    TabId = ets:new(myTable, []),
  % File Menu
    ets:insert(TabId,{?wxID_NEW,         {"New",       null}, "Create a new file.",             false, File, {}, [{ide,add_editor,[]}]}),
    ets:insert(TabId,{?wxID_OPEN,        {"Open",      null}, "Open an existing file.",         true,  File, {}, [{ide,open_file,[Frame]}]}),
    ets:insert(TabId,{?wxID_SAVE,        {"Save",      null}, "Save the current file.",         false, File, {}, [{ide,save_current_file,[]}]}),
    ets:insert(TabId,{?wxID_SAVEAS,      {"Save As",   null}, "Save the file with a new name.", false, File, {}, [{ide,apply_to_all_editors,[]}]}),
    ets:insert(TabId,{?MENU_ID_SAVE_ALL, {"Save All",  null}, "Save all open files.",           true,  File, {}, []}),
    ets:insert(TabId,{?wxID_PRINT,       {"Print",     null}, "Print the current file.",        true,  File, {}, []}),
    ets:insert(TabId,{?wxID_CLOSE,       {"Close",     null}, "Close the current file.",        false, File, {}, []}),
    ets:insert(TabId,{?wxID_CLOSE_ALL,   {"Close All", null}, "Close all open files.",          true,  File, {}, []}),
    ets:insert(TabId,{?wxID_EXIT,        {"Exit",      null}, "Quit the application.",          false, File, {}, []}),
    
  % Edit Menu
    ets:insert(TabId,{?wxSTC_CMD_UNDO,  {"Undo",   null}, "Undo the last change.",                    false, Edit, {}, []}),
    ets:insert(TabId,{?wxSTC_CMD_REDO,  {"Redo",   null}, "Redo the last change.",                    true,  Edit, {}, []}),
    ets:insert(TabId,{?wxSTC_CMD_CUT,   {"Cut",    null}, "Cut the selected text.",                   false, Edit, {}, []}),
    ets:insert(TabId,{?wxSTC_CMD_COPY,  {"Copy",   null}, "Copy the selected text to the clipboard.", false, Edit, {}, []}),
    ets:insert(TabId,{?wxSTC_CMD_PASTE, {"Paste",  null}, "Paste from clipboard.",                    false, Edit, {}, []}),
    ets:insert(TabId,{?wxID_DELETE,     {"Delete", null}, "Delete the current selection.",            false, Edit, {}, []}),
    
  % View Menu
    ets:insert(TabId,{?MENU_ID_FONT,          {"Font",                null},                  "Select font.",                  true,  View,       {},             [{ide,update_styles,[Frame]}]}),
    ets:insert(TabId,{?MENU_FONT_BIGGER,      {"Bigger",              null},                  "Increase the font size.",       false, Font,       {},             []}),
    ets:insert(TabId,{?MENU_FONT_SMALLER,     {"Smaller",             null},                  "Decrease the font size.",       false, Font,       {},             []}),
    ets:insert(TabId,{?MENU_ID_LN_TOGGLE,     {"Toggle line numbers", null},                  "Toggle line numbers on/off.",   true,  View,       {check, true},  []}),
    ets:insert(TabId,{?MENU_ID_INDENT_TYPE,   {"Indent Type",         null},                  "Indent type: tabs/spaces.",     false, View,       {},             []}),
    ets:insert(TabId,{?MENU_ID_INDENT_TABS,   {"Tabs",                null},                  "Indent using tabs.",            false, IndentType, {radio, true},  []}),
    ets:insert(TabId,{?MENU_ID_INDENT_SPACES, {"Spaces",              null},                  "Indent using spaces.",          false, IndentType, {radio, false}, []}),
    ets:insert(TabId,{?MENU_ID_INDENT_WIDTH,  {"Indent width",        null},                  "Width of indent in spaces.",    true,  View,       {},             []}),
    ets:insert(TabId,{?MENU_ID_FULLSCREEN,    {"Fullscreen",          null},                  "Toggle fullscreen.",            true,  View,       {check, false}, []}),
    ets:insert(TabId,{?MENU_ID_HIDE_TEST,     {"Hide Test Pane",      "Show Test Pane"},      "Hide/Show the test pane.",      false, View,       {},             [{ide,toggle_pane,[test]}, {update_label,Frame,2}]}),
    ets:insert(TabId,{?MENU_ID_HIDE_UTIL,     {"Hide Utilities Pane", "Show Utilities Pane"}, "Hide/Show the utilities pane.", false, View,       {},             [{ide,toggle_pane,[util]}, {update_label,Frame,2}]}),
    ets:insert(TabId,{?MENU_ID_MAX_EDITOR,    {"Maximise editor",     null},                  "Maximise the editor pane.",     false, View,       {},             [{ide,toggle_pane,[editor]}]}),
    ets:insert(TabId,{?MENU_ID_MAX_UTIL,      {"Maximise utilities",  null},                  "Maximise the utilities pane.",  false, View,       {},             [{ide,toggle_pane,[maxutil]}]}),
    
  % Document Menu
    ets:insert(TabId,{?MENU_ID_LINE_WRAP,         {"Line Wrap",         null}, "Line wrap.",                 false, Document, {check, false}, []}),
    ets:insert(TabId,{?MENU_ID_AUTO_INDENT,       {"Auto indent",       null}, "Auto indent.",               true,  Document, {check, true},  []}),
    ets:insert(TabId,{?MENU_ID_INDENT_SELECTION,  {"Indent selection",  null}, "Indent selected text.",      false, Document, {},             []}),
    ets:insert(TabId,{?MENU_ID_COMMENT_SELECTION, {"Comment selection", null}, "Comment the selected text.", true,  Document, {},             []}),
    ets:insert(TabId,{?MENU_ID_FOLD_ALL,          {"Fold all",          null}, "Fold all code.",             false, Document, {},             []}),
    ets:insert(TabId,{?MENU_ID_UNFOLD_ALL,        {"Unfold all",        null}, "Unfold all code.",           false, Document, {},             []}),
    
  % Wrangler Menu    
    ets:insert(TabId,{?MENU_ID_WRANGLER, {"Wrangler", null}, "Wrangler.", false, Wrangler, {}, []}),
    
  % Tools Menu
    ets:insert(TabId,{?MENU_ID_COMPILE,  {"Compile",      null}, "Compile the current file.",   true,  Tools, {}, []}),
    ets:insert(TabId,{?MENU_ID_RUN,      {"Run",          null}, "Run the current file.",       false, Tools, {}, []}),
    ets:insert(TabId,{?MENU_ID_DIALYZER, {"Dialyzer",     null}, "Run Dialyzer.",               false, Tools, {}, []}),
    ets:insert(TabId,{?MENU_ID_TESTS,    {"Run tests",    null}, "Run tests for current file.", false, Tools, {}, []}),
    ets:insert(TabId,{?MENU_ID_DEBUGGER, {"Run debugger", null}, "Run debugger.",               false, Tools, {}, []}),
    
  % Help Menu
    ets:insert(TabId,{?wxID_HELP,          {"Help",               null}, "View Help.",                       false, Help, {}, []}),
    ets:insert(TabId,{?MENU_ID_SHORTCUTS,  {"Keyboard shortcuts", null}, "View keyboard shortcuts.",         true,  Help, {}, []}),
    ets:insert(TabId,{?MENU_ID_SEARCH_DOC, {"Search doc",         null}, "Search the Erlang documentation.", false, Help, {}, []}),
    ets:insert(TabId,{?MENU_ID_MANUAL,     {"Manual",             null}, "View the IDE manual.",             true,  Help, {}, []}),
    ets:insert(TabId,{?wxID_ABOUT,         {"About",              null}, "About.",                           false, Help, {}, [{about, new, [{parent, Frame}]}]}),
    
    %%%%%%%%%%%%%%%%%%%
    %%%%% Menubar %%%%%
    MenuBar = wxMenuBar:new(),
    buildMenuBar(MenuBar, TabId, ets:first(TabId)),
  
    wxMenuBar:append(MenuBar, File, "File"),
    wxMenuBar:append(MenuBar, Edit, "Edit"),
    wxMenuBar:append(MenuBar, View, "View"),
    wxMenuBar:append(MenuBar, Document, "Document"),
    wxMenuBar:append(MenuBar, Wrangler, "Wrangler"),
    wxMenuBar:append(MenuBar, Tools, "Tools"),
    wxMenuBar:append(MenuBar, Help, "Help"),
    wxMenu:append(View, ?MENU_ID_INDENT_TYPE, "Indent Type", IndentType),
    add_tab_width_menu(IndentWidth),
    wxMenu:append(View, ?MENU_ID_INDENT_WIDTH, "Indent Width", IndentWidth),
    
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
    
buildMenuBar(MenuBar, Table, Key) ->
	case Key of
		'$end_of_table' ->
			ok;
		_ ->
			{Item, {Label, AltLabel}, _, Separator, ParentMenu, {Type, Checked}, _} = ets:lookup(Table, Key),
			addItem({Item, Label, Separator, ParentMenu, {Type, Checked}}),
			buildMenuBar(MenuBar, Table, ets:next(Table, Key))
	end.

addItem(Item) ->
	case Item of
		{Item, Label, true, ParentMenu, {check, true}} ->
			wxMenu:append(ParentMenu, Item, Label, [{kind, ?wxITEM_CHECK}]),
			wxMenu:check(ParentMenu, Item, true),
			wxMenu:appendSeparator(ParentMenu);

		{Item, Label, true, ParentMenu, {check, false}} ->
			wxMenu:append(ParentMenu, Item, Label, [{kind, ?wxITEM_CHECK}]),
			wxMenu:appendSeparator(ParentMenu);

		{Item, Label, true, ParentMenu, {radio, true}} ->
			wxMenu:appendRadioItem(ParentMenu, Item, Label),
			wxMenu:check(ParentMenu, Item, true),
			wxMenu:appendSeparator(ParentMenu);

		{Item, Label, true, ParentMenu, {radio, false}} ->
			wxMenu:appendRadioItem(ParentMenu, Item, Label),
			wxMenu:appendSeparator(ParentMenu);

		{Item, Label, false, ParentMenu, {check, true}} ->
			wxMenu:append(ParentMenu, Item, Label, [{kind, ?wxITEM_CHECK}]),
			wxMenu:check(ParentMenu, Item, true);

		{Item, Label, false, ParentMenu, {check, false}} ->
			wxMenu:append(ParentMenu, Item, Label, [{kind, ?wxITEM_CHECK}]);
			
		{Item, Label, false, ParentMenu, {radio, true}} ->
			wxMenu:appendRadioItem(ParentMenu, Item, Label),
			wxMenu:check(ParentMenu, Item, true);
			
		{Item, Label, false, ParentMenu, {radio, false}} ->
			wxMenu:appendRadioItem(ParentMenu, Item, Label);
			
		{Item, Label, false, ParentMenu, _} ->
			wxMenu:append(ParentMenu, Item, Label)
	end.   
 

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
               customStatusBar:set_text_timeout(Sb, {field, help}, "Not yet implemented.")
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

add_tab_width_menu(TabMenu) ->
	add_tab_width(TabMenu, 1).
add_tab_width(TabMenu, 8) ->
    wxMenu:appendRadioItem(TabMenu, 7008, integer_to_list(8));
add_tab_width(TabMenu, Width) ->
    wxMenu:appendRadioItem(TabMenu, 7000 + Width, integer_to_list(Width)),
    add_tab_width(TabMenu, Width + 1).

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
