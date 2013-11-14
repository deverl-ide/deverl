%% =====================================================================
%% @author
%% @copyright
%% @title
%% @version
%% @doc This module holds the default preferences that the program uses
%%      when no user-specified preferences have been defined.
%% @end
%% =====================================================================

-module(sys_pref_defaults).

-include_lib("wx/include/wx.hrl").
-include("../../include/ide.hrl").

-export([get_defaults/0]).


get_defaults() ->
  [
  %% Project/Directory prefs
  {project_directory, undefined},
  {projects, []},

  %% Editor prefs
  {theme, "Putty"},
  {editor_font_size, ?DEFAULT_FONT_SIZE},
  {editor_font_facename, undefined},
  {editor_font_family, ?wxFONTFAMILY_TELETYPE},
  {editor_font_style, ?wxNORMAL},
  {editor_font_weight, ?wxNORMAL},
  {show_line_no, true},
  {line_wrap, 1},
  {auto_indent, false},
  {use_tabs, true},
  {tab_width, "2"},
  {indent_guides, false},

  %% Console prefs
  {console_theme, {"Light", ?wxBLACK, ?wxWHITE}},
  {console_font_size, ?DEFAULT_FONT_SIZE},
  {console_font_facename, undefined},
  {console_font_family, ?wxFONTFAMILY_TELETYPE},
  {console_font_style, ?wxNORMAL},
  {console_font_weight, ?wxNORMAL}
  ].
