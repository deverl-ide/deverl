%% =====================================================================
%% @author
%% @copyright
%% @title
%% @version
%% @doc
%% A module of functions that provide text editor operations.
%% @end
%% =====================================================================

-module(editor_ops).

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
	Font = wxFont:new(sys_pref_manager:get_preference(editor_font_size),
										sys_pref_manager:get_preference(editor_font_family),
										sys_pref_manager:get_preference(editor_font_style),
										sys_pref_manager:get_preference(editor_font_weight), []),
  wxFontData:setInitialFont(FD, Font),
  Dialog = wxFontDialog:new(Frame, FD),
  case wxDialog:showModal(Dialog) of
    ?wxID_OK ->
      %% Get the user selected font, and update the editors
      Font = wxFontData:getChosenFont(wxFontDialog:getFontData(Dialog)),
      sys_pref_manager:set_preference(editor_font_size, wxFont:getPointSize(Font)),
      sys_pref_manager:set_preference(editor_font_family, wxFont:getFamily(Font)),
      sys_pref_manager:set_preference(editor_font_style, wxFont:getStyle(Font)),
      sys_pref_manager:set_preference(editor_font_weight, wxFont:getWeight(Font)),
			doc_manager:apply_to_all_documents(fun editor:set_font_style/2, [Font]),
      ok;
    ?wxID_CANCEL ->
				ok
	end.


%% =====================================================================
%% @doc Show the find/replace dialog.

find_replace(Parent) ->
  FindData = find_replace_data:new(),
  find_replace_data:set_options(FindData, ?IGNORE_CASE bor ?WHOLE_WORD bor ?START_WORD),
  find_replace_data:set_search_location(FindData, ?FIND_LOC_DOC),
  case erlang:whereis(find_replace_dialog) of
    undefined ->
      find_replace_dialog:show(find_replace_dialog:new(Parent, FindData));
    Pid ->
      wxDialog:raise(find_replace_dialog:get_ref(Pid))
  end.


%% =====================================================================
%% Functions for all open documents
%% =====================================================================

set_theme(ThemeMenu) ->
  {ok, Ckd} = ide_menu:get_checked_menu_item(wxMenu:getMenuItems(ThemeMenu)),
	Font = wxFont:new(sys_pref_manager:get_preference(editor_font_size),
										sys_pref_manager:get_preference(editor_font_family),
										sys_pref_manager:get_preference(editor_font_style),
										sys_pref_manager:get_preference(editor_font_weight), []),
	doc_manager:apply_to_all_documents(fun editor:set_theme/3, [wxMenuItem:getLabel(Ckd),
		Font]),
  sys_pref_manager:set_preference(theme, wxMenuItem:getLabel(Ckd)).

set_line_wrap(Menu) ->
  Bool = wxMenuItem:isChecked(wxMenu:findItem(Menu, ?MENU_ID_LINE_WRAP)),
	doc_manager:apply_to_all_documents(fun editor:set_line_wrap/2, [Bool]),
  sys_pref_manager:set_preference(line_wrap, Bool).

set_line_margin_visible(Menu) ->
  Bool = wxMenuItem:isChecked(wxMenu:findItem(Menu, ?MENU_ID_LN_TOGGLE)),
	doc_manager:apply_to_all_documents(fun editor:set_line_margin_visible/2, [Bool]),
  sys_pref_manager:set_preference(show_line_no, Bool).

set_indent_tabs(#wx{id=Id, event=#wxCommand{type=command_menu_selected}}) ->
  Cmd = case Id of
    ?MENU_ID_INDENT_SPACES -> false;
    ?MENU_ID_INDENT_TABS -> true
  end,
	doc_manager:apply_to_all_documents(fun editor:set_use_tabs/2, [Cmd]),
  sys_pref_manager:set_preference(use_tabs, Cmd).

set_indent_guides(Menu) ->
  Bool = wxMenuItem:isChecked(wxMenu:findItem(Menu, ?MENU_ID_INDENT_GUIDES)),
	doc_manager:apply_to_all_documents(fun editor:set_indent_guides/2, [Bool]),
  sys_pref_manager:set_preference(indent_guides, Bool).


%% =====================================================================
%% Functions for single documents
%% =====================================================================

transform_selection(#wx{id=Id, event=#wxCommand{type=command_menu_selected}}) ->
	Cmd = case Id of
		?MENU_ID_UC_SEL -> uppercase;
		?MENU_ID_LC_SEL -> lowercase
	end,
	doc_manager:apply_to_active_document(fun editor:transform_selection/2, [{transform, Cmd}]).

indent_right() -> 
  doc_manager:apply_to_active_document(fun editor:indent_right/1).
indent_left() -> 
  doc_manager:apply_to_active_document(fun editor:indent_left/1).
comment() -> 
  doc_manager:apply_to_active_document(fun editor:comment/1).
zoom_in() -> 
  doc_manager:apply_to_active_document(fun editor:zoom_in/1).
zoom_out() -> 
  doc_manager:apply_to_active_document(fun editor:zoom_out/1).


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
		editor:go_to_position(doc_manager:get_active_document_ref(), {L,C});
	(_,O) -> wxEvent:skip(O) %% Cancel/Close
	end,
	{Ln, Col} = editor:get_current_pos(doc_manager:get_active_document_ref()),
	lib_dialog_wx:text_input_dialog(Parent, "Go to Line", "Enter line:", "Go",
		[{callback, Callback}, {init_text, integer_to_list(Ln)++":"++integer_to_list(Col)}]).
  
