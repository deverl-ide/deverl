%% Types
-type path() :: string().
-type project_id() :: {integer(), integer(), integer()}.

%% Records
-record(font, {size,
               facename,
               family,
               style,
               weight
               }).
               
-record(general_prefs, {path_to_erl,
                        path_to_erlc,
                        path_to_dialyzer,
                        home_env_var
                        }).

-record(compiler_options, {show_warnings,
                           include_dirs,
                           verbose_out,
                           warn_to_err,
                           debug_info
                           }).

-record(dialyzer_options, {plt,
                           include_dirs,
                           verbose_out,
                           stats_out,
                           quiet_out
                           }).
%% Global constants
-define(FIND_ALL, 3).
-define(REPLACE_ALL, 5).
-define(REPLACE_FIND, 6).
-define(FIND_NEXT, 7).
-define(FIND_PREV, 9).
-define(FIND_INPUT, 10).
-define(REPLACE_INPUT, 11).
-define(IGNORE_CASE, 1).
-define(WHOLE_WORD, 2).
-define(START_WORD, 4).
-define(REGEX, 8).
-define(FIND_LOC, 12).
-define(FIND_LOC_DOC, 0).
-define(FIND_LOC_PROJ, 1).
-define(FIND_LOC_OPEN, 2).

-define(DEFAULT_FONT_SIZE,    12).

-define(STATUS_BAR_HELP_DEFAULT, "").
-define(SB_ID_LINE, 1).
-define(SB_ID_SELECTION, 2).
-define(SB_ID_FUNCTION, 3).
-define(SB_ID_HELP, 4).

-define(THEME_DIRECTORY, "../priv/themes").

%% New menu items sholud be defined within the reserved range:
%% MENU_ID_LOWEST < NEW_ITEM < MENU_ID_HIGHEST
-define(MENU_ID_LOWEST,						 6000).
-define(MENU_ID_NEW_PROJECT,			 6001).
-define(MENU_ID_OPEN_PROJECT,			 6002).
-define(MENU_ID_SAVE_ALL,          6003).
-define(MENU_ID_SAVE_PROJECT,      6048).
-define(MENU_ID_CLOSE_PROJECT,     6049).
-define(MENU_ID_FONT,              6004).
-define(MENU_ID_FONT_BIGGER,       6005).
-define(MENU_ID_FONT_SMALLER,      6006).
-define(MENU_ID_LN_TOGGLE,         6007).
-define(MENU_ID_INDENT_TYPE,       6008).
-define(MENU_ID_TAB_WIDTH,      	 6009).
-define(MENU_ID_INDENT_TABS,       6010).
-define(MENU_ID_INDENT_SPACES,     6011).
-define(MENU_ID_INDENT_GUIDES,		 6012).
-define(MENU_ID_THEME_SELECT, 		 6013).
-define(MENU_ID_FULLSCREEN,        6015).
-define(MENU_ID_HIDE_TEST,         6016).
-define(MENU_ID_HIDE_UTIL,         6017).
-define(MENU_ID_MAX_EDITOR,        6018).
-define(MENU_ID_MAX_UTIL,          6019).
-define(MENU_ID_LINE_WRAP,         6020).
-define(MENU_ID_AUTO_INDENT,       6021).
-define(MENU_ID_INDENT_RIGHT,  		 6022).
-define(MENU_ID_INDENT_LEFT,  	 	 6023).
-define(MENU_ID_TOGGLE_COMMENT, 	 6024).
-define(MENU_ID_UC_SEL, 				   6025).
-define(MENU_ID_LC_SEL, 				 	 6026).
-define(MENU_ID_FOLD_ALL,          6027).
-define(MENU_ID_UNFOLD_ALL,        6028).
-define(MENU_ID_GOTO_LINE, 		     6029).
-define(MENU_ID_WRANGLER,          6030).
-define(MENU_ID_COMPILE_FILE,      6031).
-define(MENU_ID_MAKE_PROJECT,      6047).
-define(MENU_ID_RUN,               6032).
-define(MENU_ID_DIALYZER,          6033).
-define(MENU_ID_RUN_TESTS,         6034).
-define(MENU_ID_RUN_OBSERVER,      6041).
-define(MENU_ID_RUN_DEBUGGER,      6035).
-define(MENU_ID_PROJECTS_WINDOW,   6036).
-define(MENU_ID_TESTS_WINDOW,      6037).
-define(MENU_ID_FUNC_WINDOW,       6038).
-define(MENU_ID_OUTPUT_WINDOW,     6039).
-define(MENU_ID_LOG_WINDOW,        6040).
-define(MENU_ID_NEXT_TAB,          6042).
-define(MENU_ID_PREV_TAB,          6043).
-define(MENU_ID_HOTKEYS,           6044).
-define(MENU_ID_SEARCH_DOC,        6045).
-define(MENU_ID_MANUAL,            6046).
-define(MENU_ID_PROJECT_CONFIG,    6050).
-define(MENU_ID_IMPORT_FILE,       6051).
-define(MENU_ID_IMPORT_PROJECT,    6052).
-define(MENU_ID_EXPORT_EDOC,       6053).
% -define(MENU_ID_RUN_OBSERVER,      6055).
-define(MENU_ID_QUICK_FIND,        6056).
-define(MENU_ID_ADD_TO_PLT,        6057).
-define(MENU_ID_PLT_INFO,          6058).
-define(MENU_ID_DIAL_WARN,         6059).
% -define(MENU_ID_ADD_TO_PLT,        6057).
% -define(MENU_ID_ADD_TO_PLT,        6057).
% -define(MENU_ID_ADD_TO_PLT,        6057).
% -define(MENU_ID_ADD_TO_PLT,        6057).
-define(MENU_ID_HIGHEST,					 6999).

%% Sub-menus
%% Reserved range for theme sub-menu
%% MENU_ID_THEME_LOWEST -> MENU_ID_THEME_HIGHEST
-define(MENU_ID_THEME_LOWEST,			 7000).
-define(MENU_ID_THEME_HIGHEST,		 7200).     

-define(MENU_ID_TAB_WIDTH_LOWEST,	 7201).
-define(MENU_ID_TAB_WIDTH_HIGHEST, 7210).

%% Menu groups (Bit flags/fields)
-define(MENU_GROUP_NOTEBOOK_EMPTY, 	1).
-define(MENU_GROUP_PROJECTS_EMPTY, 	(?MENU_GROUP_NOTEBOOK_EMPTY bsl 1)).

%% Documents
-define(DEFAULT_TAB_LABEL, "untitled").
-define(ID_WORKSPACE, 3211).

%% Windows
-define(WINDOW_EDITOR, 500).
-define(WINDOW_CONSOLE, 501).
-define(WINDOW_LOG, 502).
-define(WINDOW_OUTPUT, 503).
-define(WINDOW_FUNCTION_SEARCH, 504).

%% Global colours
-define(ROW_BG_EVEN, {250,250,250,255}).
-define(ROW_BG_ODD, {237,243,254,255}).
