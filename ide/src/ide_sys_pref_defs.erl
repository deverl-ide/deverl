%% =====================================================================
%% @author
%% @copyright
%% @title
%% @version
%% @doc This module holds the default preferences that the program uses
%%      when no user-specified preferences have been defined.
%% @end
%% =====================================================================

-module(ide_sys_pref_defs).

-include_lib("wx/include/wx.hrl").
-include("ide.hrl").

-export([get_defaults/0]).


%% =====================================================================
%% Client API
%% =====================================================================

%% =====================================================================
%% @doc

-spec get_defaults() -> DefaultPrefs when
  DefaultPrefs :: list().

get_defaults() ->
  
  DefaultFont = #font{size = ?DEFAULT_FONT_SIZE,
                      facename = "",
                      family = ?wxFONTFAMILY_TELETYPE,
                      style = ?wxNORMAL,
                      weight = ?wxNORMAL
                      },
  
  [
    %% Project/Directory prefs
    {project_directory, undefined},
    {projects, []},
  
    %% Editor prefs
    {theme, "Putty"},
    {editor_font, DefaultFont},
    {show_line_no, true},
    {line_wrap, 1},
    {auto_indent, false},
    {use_tabs, true},
    {tab_width, "2"},
    {indent_guides, false},
  
    %% Console prefs
    {console_theme, {"Light", ?wxBLACK, ?wxWHITE, {230,230,230}, ?wxRED}},  
    {console_font, DefaultFont},
  
    %% Log prefs
    {log_font, DefaultFont},
  
    %% Compiler
    {dialyzer_options, #dialyzer_options{plt = wx_misc:getHomeDir(),
                                         include_dirs = [],
                                         verbose_out = true,
                                         stats_out = false,
                                         quiet_out = false
                                         }}
  ].
