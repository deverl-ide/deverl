%% ide_io.erl
%% Module deals with reading and writing to files.

-module(ide_io).

-export([open_new/1, read_file/1, save_as/2, save/2]).

-include_lib("wx/include/wx.hrl").


%% =====================================================================
%% @doc Read a file from the path specified by the user.

-spec open_new(Parent) -> Result when
  Parent :: wxWindow:wxWindow(),
  Result :: {string(), string(), string()}
          | {'cancel'}.
  
open_new(Parent) ->
	Dialog = wxFileDialog:new(Parent, [{style, ?wxFD_OPEN}]),
	case wxFileDialog:showModal(Dialog) of
		?wxID_OK ->
			Path = wxFileDialog:getPath(Dialog),
			Filename = wxFileDialog:getFilename(Dialog),
			% {ok, Contents} = file:read_file(Path),
			% {Path, Filename, binary_to_list(Contents)};
			{Path, Filename, read_file(Path)};
		?wxID_CANCEL ->
			{cancel}
	end.
	

%% =====================================================================
%% @doc Read the file at Path.

read_file(Path) ->
	{ok, Contents} = file:read_file(Path),
	binary_to_list(Contents).

%% =====================================================================
%% @doc Write the data to the path specified by the user through a 
%% dialog.
%% A new file will be created if the specified path doesn't exist, and
%% an existing file will be overwritten.
	
-spec save_as(Parent, Contents) -> Result when
    Parent :: wxWindow:wxWindow(),
    Contents :: string(),
    Result :: {'ok', {string(), string()}}
            | {'cancel'}.
    
save_as(Parent, Contents) ->
	Dialog = wxFileDialog:new(Parent, [{style, ?wxFD_SAVE bor 
											   ?wxFD_OVERWRITE_PROMPT bor 
											   ?wxFD_CHANGE_DIR}]),
	case wxFileDialog:showModal(Dialog) of
		?wxID_OK ->
			Path = wxFileDialog:getPath(Dialog),
			save(Path, Contents),
			{ok, {Path, wxFileDialog:getFilename(Dialog)}};
		?wxID_CANCEL -> 
			{cancel}
	end.


%% =====================================================================
%% @doc Write the data to disk.

-spec save(Path, Contents) -> 'ok' when
	Path :: string(),
	Contents :: string().

save(Path, Contents) ->
	{ok, Fd} = file:open(Path, [read, write, raw]),
	file:write(Fd, Contents),
	file:close(Fd),
	ok.
