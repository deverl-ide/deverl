-module(ide_menubar).
-export([new/1]).
-include_lib("wx/include/wx.hrl").

-define(wxID_FONT, 6000).
-define(wxID_LN_TOGGLE, 6001).
-define(wxID_INDENT_TYPE, 6002).
-define(wxID_INDENT_WIDTH, 6003).
-define(wxID_FULLSCREEN, 6004).
-define(wxID_SHOW_HIDE_TEST, 6005).
-define(wxID_SHOW_HIDE_UTIL, 6006).
-define(wxID_LINE_WRAP, 6007).
-define(wxID_AUTO_INDENT, 6008).
-define(wxID_INDENT_SELECTION, 6009).
-define(wxID_COMMENT_SELECTION, 6010).
-define(wxID_FOLD_ALL, 6011).
-define(wxID_UNFOLD_ALL, 6012).
-define(wxID_COMPILE, 6013).
-define(wxID_RUN, 6014).
-define(wxID_DIALYZER, 6015).
-define(wxID_TESTS, 6016).
-define(wxID_DEBUGGER, 6017).
-define(wxID_SHORTCUTS, 6018).
-define(wxID_SEARCH_DOC, 6019).
-define(wxID_MANUAL, 6020).

new(Frame) ->
	wxFrame:setMenuBar(Frame, make_menubar()).

make_menubar() ->
	MenuBar = wxMenuBar:new(),
	
	File     = wxMenu:new([]),
	wxMenu:append(File, ?wxID_NEW, "New"),
	wxMenu:append(File, ?wxID_OPEN, "Open"),
	wxMenu:append(File, ?wxID_SEPARATOR, []),
	wxMenu:append(File, ?wxID_SAVE, "Save"),
	wxMenu:append(File, ?wxID_SAVEAS, "Save As"),
	wxMenu:append(File, ?wxID_SEPARATOR, []),
	wxMenu:append(File, ?wxID_PRINT, "Print"),
	wxMenu:append(File, ?wxID_SEPARATOR, []),
	wxMenu:append(File, ?wxID_CLOSE, "Close"),
	wxMenu:append(File, ?wxID_CLOSE_ALL, "Close All"),
	wxMenu:append(File, ?wxID_SEPARATOR, []),
	wxMenu:append(File, ?wxID_EXIT, "Exit"),
	
	Edit     = wxMenu:new([]),
	wxMenu:append(Edit, ?wxID_UNDO, "Undo"),
	wxMenu:append(Edit, ?wxID_REDO, "Redo"),
	wxMenu:append(Edit, ?wxID_SEPARATOR, []),
	wxMenu:append(Edit, ?wxID_CUT, "Cut"),
	wxMenu:append(Edit, ?wxID_COPY, "Copy"),
	wxMenu:append(Edit, ?wxID_PASTE, "Paste"),
	wxMenu:append(Edit, ?wxID_DELETE, "Delete"),
	
	View     = wxMenu:new([]),
	wxMenu:append(View, ?wxID_FONT, "Font"),
	wxMenu:append(View, ?wxID_SEPARATOR, []),
	wxMenu:append(View, ?wxID_LN_TOGGLE, "Toggle Line Numbers", [{kind, ?wxITEM_CHECK}]),
	wxMenu:append(View, ?wxID_SEPARATOR, []),
	wxMenu:append(View, ?wxID_INDENT_TYPE, "Indent Type"),
	wxMenu:append(View, ?wxID_INDENT_WIDTH, "Indent Width"),
	wxMenu:append(View, ?wxID_SEPARATOR, []),
	wxMenu:append(View, ?wxID_FULLSCREEN, "Fullscreen", [{kind, ?wxITEM_CHECK}]),
	wxMenu:append(View, ?wxID_SEPARATOR, []),
	wxMenu:append(View, ?wxID_SHOW_HIDE_TEST, "Show/Hide Test Pane", [{kind, ?wxITEM_CHECK}]),
	wxMenu:append(View, ?wxID_SHOW_HIDE_UTIL, "Show/Hide Utilities Pane", [{kind, ?wxITEM_CHECK}]),
	
	Document = wxMenu:new([]),
	wxMenu:append(Document, ?wxID_LINE_WRAP, "Line Wrap", [{kind, ?wxITEM_CHECK}]),
	wxMenu:append(Document, ?wxID_AUTO_INDENT, "Auto-Indent", [{kind, ?wxITEM_CHECK}]),
	wxMenu:append(Document, ?wxID_SEPARATOR, []),
	wxMenu:append(Document, ?wxID_INDENT_SELECTION, "Indent Selection"),
	wxMenu:append(Document, ?wxID_COMMENT_SELECTION, "Comment Selection"),
	wxMenu:append(Document, ?wxID_SEPARATOR, []),
	wxMenu:append(Document, ?wxID_FOLD_ALL, "Fold All"),
	wxMenu:append(Document, ?wxID_UNFOLD_ALL, "Unfold All"),
	
	Wrangler = wxMenu:new([]),
	wxMenu:append(Wrangler, 0000, "WRANGLER"),
	
	Tools    = wxMenu:new([]),
	wxMenu:append(Tools, ?wxID_COMPILE, "Compile"),
	wxMenu:append(Tools, ?wxID_SEPARATOR, []),
	wxMenu:append(Tools, ?wxID_RUN, "Run Module"),
	wxMenu:append(Tools, ?wxID_DIALYZER, "Run Dialyzer"),
	wxMenu:append(Tools, ?wxID_TESTS, "Run Tests"),
	wxMenu:append(Tools, ?wxID_DEBUGGER, "Run Debugger"),
	
	Help     = wxMenu:new([]),
	wxMenu:append(Help, ?wxID_HELP, "Help"), 
	wxMenu:append(Help, ?wxID_SHORTCUTS, "Keyboard Shortcuts"),
	wxMenu:append(Help, ?wxID_SEPARATOR, []),
	wxMenu:append(Help, ?wxID_SEARCH_DOC, "Search Erlang API"),
	wxMenu:append(Help, ?wxID_MANUAL, "IDE Manual"),
	wxMenu:append(Help, ?wxID_SEPARATOR, []),
    wxMenu:append(Help, ?wxID_ABOUT, "About"), 
	
	wxMenuBar:append(MenuBar, File, "File"),
	wxMenuBar:append(MenuBar, Edit, "Edit"),
	wxMenuBar:append(MenuBar, View, "View"),
	wxMenuBar:append(MenuBar, Document, "Document"),
	wxMenuBar:append(MenuBar, Wrangler, "Wrangler"),
	wxMenuBar:append(MenuBar, Tools, "Tools"),
	wxMenuBar:append(MenuBar, Help, "Help"),
	
	MenuBar.
	
