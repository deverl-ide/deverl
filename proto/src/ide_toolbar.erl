-module(ide_toolbar).
-include_lib("wx/include/wx.hrl").
-compile(export_all).

-define(wxID_POO, 6000).

new(Frame) ->
	ToolBar = wxFrame:createToolBar(Frame, []),
	
	wxToolBar:addTool(ToolBar, ?wxID_NEW, "New", wxArtProvider:getBitmap("wxART_NEW"),
		      [{shortHelp, "New"}]),

    wxToolBar:addTool(ToolBar, ?wxID_OPEN, "Open", wxArtProvider:getBitmap("wxART_FILE_OPEN"),
		      [{shortHelp, "Open"}]),
		      
	wxToolBar:addTool(ToolBar, ?wxID_CLOSE, "Close", wxArtProvider:getBitmap("wxART_CROSS_MARK"),
		      [{shortHelp, "Close"}]),

    wxToolBar:addSeparator(ToolBar),
    
	wxToolBar:realize(ToolBar),
    wxFrame:setToolBar(Frame,ToolBar).
    ok.
