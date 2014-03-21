%% =====================================================================
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%% 
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%% 
%% You should have received a copy of the GNU General Public License
%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%
%% @author Tom Richmond <tr201@kent.ac.uk>
%% @author Mike Quested <mdq3@kent.ac.uk>
%% @copyright Tom Richmond, Mike Quested 2014
%%
%% @doc Manages file i/o.
%% @end
%% =====================================================================


-module(deverl_io).

-include_lib("wx/include/wx.hrl").

%% Client API
-export([
        create_directory_structure/1,
        create_new_file/1,
        create_new_file/2,
        open_new/1,
        read_file/1,
        save_as/2,
        save/2,
        copy_to_poject_dir/2,
        delete/1
        ]).


%% =====================================================================
%% Client API
%% =====================================================================

%% =====================================================================
%% @doc Create the directory structure for a new project.

-spec create_directory_structure(string()) -> string() | no_return().

create_directory_structure(Path) ->
  try
    create_dir(Path),
    create_dir(Path, "ebin"),
    create_dir(Path, "priv"),
    create_dir(Path, "include"),
    create_dir(Path, "src"),
		Path
  catch
    throw:E -> throw(E)
  end.


%% =====================================================================
%% @doc Create a new file on disc.

-spec create_new_file(string()) -> ok | error.

create_new_file(Path) ->
  create_new_file(Path, []).
  
create_new_file(Path, Options) ->
  case file:open(Path, [write, read]) of
    {error, _Reason} ->
      error;
    {ok, IoDevice} ->
      copy_from_template(Path, proplists:get_value(template, Options)),
      file:close(IoDevice),  
      ok
  end.


%% =====================================================================
%% @doc Read a file from the path specified by the user.

-spec open_new({'wx_ref',integer(),_,_}) -> 'cancel' | wxFileDialog:charlist().

open_new(Parent) ->
	Dialog = wxFileDialog:new(Parent, [{style, ?wxFD_OPEN}]),
	case wxFileDialog:showModal(Dialog) of
		?wxID_OK ->
			Path = wxFileDialog:getPath(Dialog),
      wxFileDialog:destroy(Dialog),
      Path;
		?wxID_CANCEL ->
      wxFileDialog:destroy(Dialog),
			cancel
	end.


%% =====================================================================
%% @doc Read the file at Path.

-spec read_file(string()) -> list() | no_return().

read_file(Path) ->
	try
		{ok, Contents} = file:read_file(Path),
		binary_to_list(Contents)
	catch
		error:_E ->
			throw("Could not read file.")
	end.


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
	try
    %Result = file:open(Path, [write]),
		{ok, Fd} = file:open(Path, [write]),
		ok = file:write(Fd, Contents),
		ok = file:close(Fd)
	catch
		error:{badmatch,{error,Error}} ->
      get_error_message(Error, Path)
	end.
  
 
%% =====================================================================
%% @doc Copy the directory at Source to Dest, providing they aren't in
%% the same dir to begin with. The user will be notified if an error
%% occurs.

-spec copy_to_poject_dir(file:filename(), file:filename()) ->
  ok | {error, term()}.

copy_to_poject_dir(Source, Dest) ->
  SubPath = string:sub_string(Source, 1, length(Dest)),
  case string:equal(SubPath, Dest) of %% check we're not copying from/to same dir
    true -> ok; %% same dir
    false ->
      case copy_dir_recursive(Source, Dest) of
        ok -> ok;
        {error, Error} ->
          Dlg = deverl_lib_dlg_wx:message_quick(wx:null(), "Oops", Error),
          {error, Error}
      end
  end.


%% =====================================================================
%% @doc

delete(Path) ->
  case file:delete(Path) of
    ok -> 
      ok;
    {error, Error} ->
      Dlg = deverl_lib_dlg_wx:message(wx:null(), 
        [{caption, "Oops, the file could't be deleted."},
         {text1, format_error_msg(Error, Path)},
         {buttons, [?wxID_OK]}]),
      {error, Error}
  end.


%% =====================================================================
%% Internal functions
%% =====================================================================

%% =====================================================================
%% @doc Recursively copy directory from Source to Dest.
%% If an error occurs mid-operaiton the user will be notified, and given
%% the choice to cancel or skip/continue. If they cancel any files
%% copied up to that point will be deleted.

-spec copy_dir_recursive(file:filename(), file:filename()) -> ok | {error, string()}.

copy_dir_recursive(Source, Dest) ->
  case file:list_dir(Source) of
    {ok, Files} ->
      Root = filename:join(Dest, filename:basename(Source)),
      try
        ok = file:make_dir(Root),
        copy_dir_recursive(Files, Source, Root)
      catch
        throw:cancelled -> %% User cancelled the op, cleanup
          del_dir(filename:join(Dest, filename:basename(Source))); 
          
        _:{badmatch, {error, E}} -> 
          {error, format_error_msg(E, Root)}
      end;
    {error, Error} ->
      {error, format_error_msg(Error, Source)}
  end.
  
copy_dir_recursive([], Source, Dest) -> ok;
copy_dir_recursive([File | T], Source, Dest) ->
  Cd = filename:join(Source, File),
  case filelib:is_file(Cd) of
    true ->
      case filelib:is_dir(Cd) of
        true -> %% Copy the directory recursively
          Dir = filename:join(Dest, File),
          ok = file:make_dir(Dir),
          Files = list_dirs(Cd),
          Files1 = lists:map(fun(L) -> filename:join(File, L) end, Files),
          copy_dir_recursive(Files1, Source, Dest);
        false -> %% Copy file
          do_copy(Cd, filename:join(Dest, File))
      end,
      copy_dir_recursive(T, Source, Dest), %% Process the rest of the list
      ok;
    false -> ok
  end.
  
do_copy(Source, Dest) ->
  case file:copy(Source, Dest) of
    {ok, _Copied} -> ok;
    {error, Error} -> 
      notify_error(Error, Source)
  end.
  
list_dirs(Name) ->
  case file:list_dir(Name) of
    {ok, Filenames} ->
      Filenames;
    {error, Error} ->
      notify_error(Error, Name),
      []
  end.
  
%% Notify the user of a problem mid-operation. i.e. Trying to copy a file
%% without permission.  
notify_error(Error, Path) ->
  Dlg = deverl_lib_dlg_wx:message(wx:null(), 
    [{caption, "Oops"},
     {text1, format_error_msg(Error, Path)},
     {text2, "Click Ok to continue, or Cancel to stop the operation."},
     {buttons, [?wxID_CANCEL, ?wxID_OK]}]),
  case wxDialog:showModal(Dlg) of
    ?wxID_CANCEL -> %% Stop the operation
      wxDialog:destroy(Dlg),
      throw(cancelled);
    ?wxID_OK -> wxDialog:destroy(Dlg)
  end.
 

%% =====================================================================
%% @doc Delete the directory Dir. This will recursively delete all
%% contents of the directory.

del_dir(Dir) ->
  lists:foreach(fun(D) ->
    ok = file:del_dir(D)
  end, del_all_files([Dir], [])).
 
del_all_files([], EmptyDirs) ->
  EmptyDirs;
del_all_files([Dir | T], EmptyDirs) ->
  {ok, FilesInDir} = file:list_dir(Dir),
  {Files, Dirs} = lists:foldl(fun(F, {Fs, Ds}) ->
    Path = Dir ++ "/" ++ F,
    case filelib:is_dir(Path) of
      true ->
        {Fs, [Path | Ds]};
      false ->
        {[Path | Fs], Ds}
    end
  end, {[],[]}, FilesInDir),
  lists:foreach(fun(F) ->
    ok = file:delete(F)
  end, Files),
  del_all_files(T ++ Dirs, [Dir | EmptyDirs]). 
  
%% =====================================================================
%% @doc Create directory Dir.

create_dir(Dir) ->
	case file:make_dir(Dir) of
    {error, Error} ->
      get_error_message(Error, Dir);
    ok ->
      ok
	end.
  
create_dir(Dest, Name) ->
  create_dir(Dest, Name, []).

create_dir(Dest, Name, Options) ->
  Path = filename:join(Dest, Name),
  Msg = proplists:get_value(use_in_error, Options, Path),
	case file:make_dir(Path) of
    {error, Error} ->
      get_error_message(Error, Msg);
    ok ->
      ok
	end.

%% =====================================================================
%% @doc Get a more comprehensive error message.

-spec get_error_message(string(), string()) -> no_return().

get_error_message(Error, Filename) ->
  % Filename = filename:basename(Path),
  case Error of
    eacces ->
			throw("Permission denied.\n\nCannot access " ++ Filename ++ ", please check your permissions.");
		eexist ->
			throw(Filename ++ " already exists.");
		enoent ->
			throw("No such file or directory: " ++ Filename);
		enospc ->
			throw("There is a no space left on the device.");
		_ ->
  		throw("An error occurred.")
  end.
  
format_error_msg(eacces) -> "Permission denied. Please check your permissions.".
format_error_msg(eacces, Path) -> "Permission denied.\n\nCannot access " ++ Path ++ ", please check your permissions.";
format_error_msg(eexist, Path) -> "The file already exists.";
format_error_msg(enoent, Path) -> "No such file or directory.";
format_error_msg(enospc, Path) -> "There is no space left on the device. The disk is full.";
format_error_msg(enotdir, Path) -> "Not a directory.";
format_error_msg(_Posix, Path) -> "An error occured.".


%% =====================================================================
%% @doc

copy_from_template(_File, undefined) -> ok;
copy_from_template(_File, plain_text) -> ok;
copy_from_template(File, Type) -> 
  Dir = deverl_lib_widgets:rc_dir(filename:join("templates", atom_to_list(Type) ++ ".txt")),
  try
    copy_file(Dir, File)
  catch
    _Throw -> ok %% Don't bother with template
  end,
  ok.

copy_file(Source, Dest) ->
  case file:copy(Source, Dest) of
    {ok, _BytesCopied} -> 
      ok;
    {error, Reason} -> 
      throw("Copy failed: " ++ atom_to_list(Reason))
  end.