-module(ide_io).
-compile(export_all).
-include_lib("wx/include/wx.hrl").


open_file(Parent) ->
	Dialog = wxFileDialog:new(Parent, [{style, ?wxFD_OPEN}]),
	wxFileDialog:showModal(Dialog),

	Path = wxFileDialog:getPath(Dialog),
	Filename = wxFileDialog:getFilename(Dialog),
	File = file:open(Path, [read, write]),
	Text = file:read_file(File),
	
	{Filename, Text}.
	%io:format(Path ++ "~n"),
	%io:format(Filename ++ "~n").
	  
save(Parent, Contents) ->
  Dialog = wxFileDialog:new(Parent, [{style, ?wxFD_SAVE}]),
  wxFileDialog:showModal(Dialog),
  Path = wxFileDialog:getPath(Dialog),
  
  Handle = file:open(Path, [write]),
  file:write(Handle,Contents).