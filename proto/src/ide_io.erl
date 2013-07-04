-module(ide_io).
-compile(export_all).
-include_lib("wx/include/wx.hrl").


open_file(_Frame) ->
	OpenDialog = wxFileDialog:new(wx:null(), [{style, wxFD_OPEN}]),
	
	
	wxFileDialog:showModal(OpenDialog).
	
  
save(Parent, Contents) ->
  Dialog = wxFileDialog:new(Parent, [{style, ?wxFD_SAVE}]),
  wxFileDialog:showModal(Dialog),
  Path = wxFileDialog:getPath(Dialog),
  
  Handle = file:open(Path, [write]),
  file:write(Handle,Contents).