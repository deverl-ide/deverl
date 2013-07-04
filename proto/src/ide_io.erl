-module(ide_io).
-compile(export_all).
-include_lib("wx/include/wx.hrl").


open_file(_Frame) ->
	OpenDialog = wxFileDialog:new(wx:null(), [{style, wxFD_OPEN}]),
	
	
	wxFileDialog:showModal(OpenDialog).
	
  
save(Parent) ->
  Dialog = wxFileDialog:new(Parent, []),
  wxFileDialog:showModal(Dialog)