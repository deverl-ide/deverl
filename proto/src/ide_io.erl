%% ide_io.erl
%% Module deals with reading and writing to files.

-module(ide_io).

-export([open/1, save_as/2, save/2]).

-include_lib("wx/include/wx.hrl").


%% =====================================================================
%% @doc Read a file from the path specified by the user.

-spec open(Parent) -> Result when
  Parent :: wxWindow:wxWindow(),
  Result :: {string(), string(), string()}
          | {'cancel'}.
  
open(Parent) ->
	Dialog = wxFileDialog:new(Parent, [{style, ?wxFD_OPEN}]),
	case wxFileDialog:showModal(Dialog) of
		?wxID_OK ->
			Path = wxFileDialog:getPath(Dialog),
			Filename = wxFileDialog:getFilename(Dialog),
			{ok, Contents} = file:read_file(Path),
			{Path, Filename, binary_to_list(Contents)};
		?wxID_CANCEL ->
			{cancel}
	end.


%% =====================================================================
%% @doc Write the data to the path specified by the user through a 
%% dialog.
%% A new file will be created if the specified path doesn't exist, and
%% an existing file will be overwritten.
	
-spec save_as(Parent, Contents) -> Result when
    Parent :: wxWindow:wxWindow(),
    Result :: {'ok', {string(), string()}}
            | {'cancel'}.
    
save_as(Parent, Contents) ->
	Dialog = wxFileDialog:new(Parent, [{style, ?wxFD_SAVE}]),
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
