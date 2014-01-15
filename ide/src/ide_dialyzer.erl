%% =====================================================================
%% @author Tom Richmond <tr201@kent.ac.uk>
%% @author Mike Quested <mdq3@kent.ac.uk>
%% @copyright 2014 Tom Richmond, Mike Quested
%% @version 1
%% @doc Starts an instance of the IDE. Builds all wx components.
%% @end
%% =====================================================================

-module(ide_dialyzer).

-export([
  check_plt/0
	]).
	
-record(file_plt, {version,
                   file_md5_list,
                   info, contracts,
                   callbacks,
                   types,
                   exported_types,
                   mod_deps,
                   implementation_md5}).


check_plt() ->
  FileName = "/Users/tommo/.dialyzer_plt",
  R = case file:read_file(FileName) of
    {ok, Bin} ->
      try binary_to_term(Bin) of
    	  #file_plt{}=FilePLT -> {ok, FilePLT};
    	  _ -> {error, not_valid}
      catch
      	_:_ -> {error, not_valid}
      end;
    {error, enoent} ->
      {error, no_such_file};
    {error, _} ->
      {error, read_error}
  end.