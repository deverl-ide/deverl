-module(ide_io).
-compile(export_all).
-include_lib("wx/include/wx.hrl").


open_file(_Parent) ->
	OpenDialog = wxFileDialog:new(wx:null(), [{style, wxFD_OPEN}]),
	
	
	wxFileDialog:showModal(OpenDialog).
	
