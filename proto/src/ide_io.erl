-module(ide_io).
-compile(export_all).
-include_lib("wx/include/wx.hrl").


open_file(Parent) ->
	Dialog = wxFileDialog:new(Parent, [{style, ?wxFD_OPEN}]),
	wxFileDialog:showModal(Dialog),

	Path = wxFileDialog:getPath(Dialog),
	Filename = wxFileDialog:getFilename(Dialog),
	
	{ok, Contents} = file:read_file(Path),
	{Filename, binary_to_list(Contents)}.
	  
save(Parent, Contents) ->
  Dialog = wxFileDialog:new(Parent, [{style, ?wxFD_SAVE}]),
  wxFileDialog:showModal(Dialog),
  Path = wxFileDialog:getPath(Dialog),
  {ok, Fd} = file:open(Path, [read, write, raw]),
  file:write(Fd, Contents),
  file:close(Fd).

