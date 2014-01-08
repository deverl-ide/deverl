%% =====================================================================
%% @author
%% @copyright
%% @title
%% @version
%% @doc
%% A module of functions that provide text editor operations.
%% @end
%% =====================================================================

-module(ide_editor_ops).

-include_lib("wx/include/wx.hrl").
-include("ide.hrl").

%% API
-export([
	update_styles/1,
	find_replace/1,
	set_theme/1,
	set_line_wrap/1,
	set_line_margin_visible/1,
	set_indent_tabs/1,
	set_indent_guides/1,
  transform_selection/1,
	indent_right/0,
	indent_left/0,
	comment/0,
	zoom_in/0,
	zoom_out/0,
  go_to_line/1
	]).


%% =====================================================================
%% @doc

update_styles(Frame) ->
  %% Display the system font picker
  FD = wxFontData:new(),
  % Font = wxFont:new(ide_sys_pref_gen:get_preference(editor_font_size),
  %                   ide_sys_pref_gen:get_preference(editor_font_family),
  %                   ide_sys_pref_gen:get_preference(editor_font_style),
  %                   ide_sys_pref_gen:get_preference(editor_font_weight), []),
  % wxFontData:setInitialFont(FD, Font),
  Dialog = wxFontDialog:new(Frame, FD),
  case wxDialog:showModal(Dialog) of
    ?wxID_OK ->
      %% Get the user selected font, and update the editors
      Font = wxFontData:getChosenFont(wxFontDialog:getFontData(Dialog)),
      ide_sys_pref_gen:set_preference(editor_font_size, wxFont:getPointSize(Font)),
      ide_sys_pref_gen:set_preference(editor_font_family, wxFont:getFamily(Font)),
      ide_sys_pref_gen:set_preference(editor_font_style, wxFont:getStyle(Font)),
      ide_sys_pref_gen:set_preference(editor_font_weight, wxFont:getWeight(Font)),
      ide_sys_pref_gen:set_preference(editor_font_facenmae, wxFont:getFaceName(Font)),
      ide_doc_man_wx:apply_to_all_documents(fun ide_editor_wx:set_font/2, [Font]),
      ok;
    ?wxID_CANCEL ->
				ok
	end.


%% =====================================================================
%% @doc Show the find/replace dialog.

find_replace(Parent) ->
  FindData = ide_find_dlg_data_wx:new(),
  ide_find_dlg_data_wx:set_options(FindData, ?IGNORE_CASE bor ?WHOLE_WORD bor ?START_WORD),
  ide_find_dlg_data_wx:set_search_location(FindData, ?FIND_LOC_DOC),
  case erlang:whereis(ide_find_dlg_wx) of
    undefined ->
      ide_find_dlg_wx:show(ide_find_dlg_wx:new(Parent, FindData));
    Pid ->
      wxDialog:raise(ide_find_dlg_wx:get_ref(Pid))
  end.


%% =====================================================================
%% Functions for all open documents
%% =====================================================================

set_theme(ThemeMenu) ->
  {ok, Ckd} = ide_menu:get_checked_menu_item(wxMenu:getMenuItems(ThemeMenu)),
	Font = wxFont:new(ide_sys_pref_gen:get_preference(editor_font_size),
										ide_sys_pref_gen:get_preference(editor_font_family),
										ide_sys_pref_gen:get_preference(editor_font_style),
										ide_sys_pref_gen:get_preference(editor_font_weight), []),
	ide_doc_man_wx:apply_to_all_documents(fun ide_editor_wx:set_theme/3, [wxMenuItem:getLabel(Ckd),
		Font]),
  ide_sys_pref_gen:set_preference(theme, wxMenuItem:getLabel(Ckd)).

set_line_wrap(Menu) ->
  Bool = wxMenuItem:isChecked(wxMenu:findItem(Menu, ?MENU_ID_LINE_WRAP)),
	ide_doc_man_wx:apply_to_all_documents(fun ide_editor_wx:set_line_wrap/2, [Bool]),
  ide_sys_pref_gen:set_preference(line_wrap, Bool).

set_line_margin_visible(Menu) ->
  Bool = wxMenuItem:isChecked(wxMenu:findItem(Menu, ?MENU_ID_LN_TOGGLE)),
	ide_doc_man_wx:apply_to_all_documents(fun ide_editor_wx:set_line_margin_visible/2, [Bool]),
  ide_sys_pref_gen:set_preference(show_line_no, Bool).

set_indent_tabs(#wx{id=Id, event=#wxCommand{type=command_menu_selected}}) ->
  Cmd = case Id of
    ?MENU_ID_INDENT_SPACES -> false;
    ?MENU_ID_INDENT_TABS -> true
  end,
	ide_doc_man_wx:apply_to_all_documents(fun ide_editor_wx:set_use_tabs/2, [Cmd]),
  ide_sys_pref_gen:set_preference(use_tabs, Cmd).

set_indent_guides(Menu) ->
  Bool = wxMenuItem:isChecked(wxMenu:findItem(Menu, ?MENU_ID_INDENT_GUIDES)),
	ide_doc_man_wx:apply_to_all_documents(fun ide_editor_wx:set_indent_guides/2, [Bool]),
  ide_sys_pref_gen:set_preference(indent_guides, Bool).


%% =====================================================================
%% Functions for single documents
%% =====================================================================

transform_selection(#wx{id=Id, event=#wxCommand{type=command_menu_selected}}) ->
	Cmd = case Id of
		?MENU_ID_UC_SEL -> uppercase;
		?MENU_ID_LC_SEL -> lowercase
	end,
	ide_doc_man_wx:apply_to_active_document(fun ide_editor_wx:transform_selection/2, [{transform, Cmd}]).

indent_right() -> 
  ide_doc_man_wx:apply_to_active_document(fun ide_editor_wx:indent_right/1, []).
indent_left() -> 
  ide_doc_man_wx:apply_to_active_document(fun ide_editor_wx:indent_left/1, []).
comment() -> 
  ide_doc_man_wx:apply_to_active_document(fun ide_editor_wx:comment/1, []).
zoom_in() -> 
  ide_doc_man_wx:apply_to_active_document(fun ide_editor_wx:zoom_in/1, []).
zoom_out() -> 
  ide_doc_man_wx:apply_to_active_document(fun ide_editor_wx:zoom_out/1, []).


%% =====================================================================
%% @doc Display the goto line dialog.

go_to_line(Parent) ->
	Callback =
	fun(#wx{obj=Dialog, id=?wxID_OK, userData=Input},O) -> %% OK clicked
		wxEvent:skip(O),
	  {Line, Column} = case string:tokens(wxTextCtrl:getValue(Input), ":") of
			[] ->  {0, 0};
	    [Ln | []] -> {Ln, 0};
	    [Ln, Col | _ ] -> {Ln, Col}
	  end,
	  L = try
	    list_to_integer(Line)
	  catch _:_ -> 0
	  end,
	  C = try
	    list_to_integer(Column)
	  catch _:_ -> 0
	  end,
		ide_editor_wx:go_to_position(ide_doc_man_wx:get_active_document_ref(), {L,C});
	(_,O) -> wxEvent:skip(O) %% Cancel/Close
	end,
	{Ln, Col} = ide_editor_wx:get_current_pos(ide_doc_man_wx:get_active_document_ref()),
	ide_lib_dlg_wx:text_input_dialog(Parent, "Go to Line", "Enter line:", "Go",
		[{callback, Callback}, {init_text, integer_to_list(Ln)++":"++integer_to_list(Col)}]).
  
