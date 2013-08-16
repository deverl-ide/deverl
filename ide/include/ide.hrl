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

%% Reserved range for menu items starts 
%% MENU_ID_LOWEST -> MENU_ID_HIGHEST
-define(MENU_ID_LOWEST,						 6000).
-define(MENU_ID_FONT,              6040).
-define(MENU_ID_FONT_BIGGER,       6042).
-define(MENU_ID_FONT_SMALLER,      6043).
-define(MENU_ID_LN_TOGGLE,         6001).
-define(MENU_ID_INDENT_TYPE,       6002).
-define(MENU_ID_TAB_WIDTH,      6003).
-define(MENU_ID_FULLSCREEN,        6004).
-define(MENU_ID_HIDE_TEST,         6005).
-define(MENU_ID_HIDE_UTIL,         6006).
-define(MENU_ID_LINE_WRAP,         6007).
-define(MENU_ID_AUTO_INDENT,       6008).
-define(MENU_ID_INDENT_RIGHT,  		 6009).
-define(MENU_ID_INDENT_LEFT,  	 	 6009).
-define(MENU_ID_COMMENT, 					 6010).
-define(MENU_ID_UNCOMMENT, 				 6010).
-define(MENU_ID_GOTO_LINE, 				 6010).
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
-define(MENU_ID_THEME_SELECT, 		 6030).
-define(MENU_ID_THEME,						 6031).
-define(MENU_ID_SAVE_ALL,          6041).
-define(MENU_ID_INDENT_GUIDES,		 6044).
-define(MENU_ID_HIGHEST,					 6999).

%% Reserved range for theme sub-menu
%% MENU_ID_THEME_LOWEST -> MENU_ID_THEME_HIGHEST
-define(MENU_ID_THEME_LOWEST,			 7000).
-define(MENU_ID_THEME_HIGHEST,		 7200).     

-define(MENU_ID_TAB_WIDTH_LOWEST,	7201).
-define(MENU_ID_TAB_WIDTH_HIGHEST,	7210).
