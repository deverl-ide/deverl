-module(ide_io).
-compile(export_all).
-include_lib("wx/include/wx.hrl").


open_file(Parent) ->
	Dialog = wxFileDialog:new(Parent, [{style, ?wxFD_OPEN}]),
	wxFileDialog:showModal(Dialog),

	Path = wxFileDialog:getPath(Dialog),
	
	Filename = wxFileDialog:getFilename(Dialog),
	
	File = file:open(Path, [read, write]),
	
	{ok, Contents} = file:read_file(Path),
	
	io:format(Path ++ "~n"),
	io:format(Filename ++ "~n"),
	io:format("~p~n",[binary_to_list(Contents)]),
	{Filename, binary_to_list(Contents)}.
	  
save(Parent, Contents) ->
  Dialog = wxFileDialog:new(Parent, [{style, ?wxFD_SAVE}]),
  wxFileDialog:showModal(Dialog),
  Path = wxFileDialog:getPath(Dialog),
  
<<<<<<< HEAD
  Handle = file:open(Path, [write]),
  file:write(Handle,Contents).
=======
  {ok, Fd} = file:open(Path, [read, write, raw]),
  file:write(Fd, Contents),
  file:close(Fd).
>>>>>>> 221d6bac7ef238d961f2d10e909d674637de2bc0
