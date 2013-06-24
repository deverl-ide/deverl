-module(test_frame).
-compile(export_all).

-include_lib("wx/include/wx.hrl").


start() ->
	Server = wx:new(),
	Frame = wxFrame:new(Server, ?wxID_ANY, "Test", [{size, {800, 300}}]),
	%wxFrame:setMenuBar(Frame, ide_menubar:new()),
	ide_menubar:new(Frame),
	
			
	
	wxFrame:show(Frame).
