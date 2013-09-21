%% This module contains library functions to display standard wx dialogs
-module(lib_dialog_wx).
-include_lib("wx/include/wx.hrl").

-define(ERROR_CAPTION, "The operation could not continue..").

-export([
	get_dir/1,
	get_existing_dir/1,
	error_msg/2]).

get_dir(Parent) ->
	dir_dialog(Parent, []).
	
get_existing_dir(Parent) ->
	dir_dialog(Parent, [{style, ?wxDD_DIR_MUST_EXIST}]).
	
dir_dialog(Parent, Args) ->
	Dialog = wxDirDialog:new(Parent, Args),
	case wxDirDialog:showModal(Dialog) of
		?wxID_OK -> 
			wxDirDialog:getPath(Dialog);
		?wxID_CANCEL -> cancelled
	end.
	
error_msg(Parent, Msg) ->
	Dialog = wxMessageDialog:new(Parent, Msg, [{style, ?wxICON_ERROR bor ?wxOK}, {caption, ?ERROR_CAPTION}]),
	wxMessageDialog:showModal(Dialog).