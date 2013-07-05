-module(ide_io).
-export([open/1, save_as/2, save/2]).
-include_lib("wx/include/wx.hrl").


open(Parent) ->
	Dialog = wxFileDialog:new(Parent, [{style, ?wxFD_OPEN}]),
	wxFileDialog:showModal(Dialog),
	Path = wxFileDialog:getPath(Dialog),
	Filename = wxFileDialog:getFilename(Dialog),
	{ok, Contents} = file:read_file(Path),
	{Filename, binary_to_list(Contents)}.
	  
save_as(Parent, Contents) ->
	Dialog = wxFileDialog:new(Parent, [{style, ?wxFD_SAVE}]),
	case wxFileDialog:showModal(Dialog) of
		?wxID_OK ->
			Path = wxFileDialog:getPath(Dialog),
			{ok, Fd} = file:open(Path, [read, write, raw]),
			file:write(Fd, Contents),
			file:close(Fd),
			{ok, {Path, wxFileDialog:getFilename(Dialog)}};
		?wxID_CANCEL -> 
			{cancel}
	end.

save(Path, Contents) ->
	{ok, Fd} = file:open(Path, [read, write, raw]),
	file:write(Fd, Contents),
	file:close(Fd),
	ok.
