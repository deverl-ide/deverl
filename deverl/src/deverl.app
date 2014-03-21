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
%% =====================================================================

{application, deverl,
	[{description, "A simple IDE for learning erlang"},
	 {vsn, "0.1.0"},
	 {modules, [deverl,
              deverl_app,
	 						deverl_build,
							deverl_compiler_port,
							deverl_console_parser,
							deverl_console_port_gen,
							deverl_console_sup,
							deverl_console_wx,
              deverl_dialyzer_port,
              deverl_dialyzer,
							deverl_dlg_about_wx,
							deverl_dlg_data_find_wx,
              deverl_dlg_dialyzer_wx,
							deverl_dlg_find_wx,
							deverl_dlg_import_proj_wx,
							deverl_dlg_new_file_wx,
							deverl_dlg_new_proj_wx,
							deverl_dlg_open_proj_wx,
							deverl_dlg_prefs_wx,
							deverl_dlg_proj_conf_wx,
							deverl_doc_man_wx,
              deverl_editor_theme,
              deverl_editor_wx,
              deverl_eunit_listener,
							deverl_file_poll_gen,
							deverl_file_poll_sup,
							deverl_io,
							deverl_lib_dlg_wx,
							deverl_lib_widgets,
							deverl_log_out_wx,
							deverl_menu,
							deverl_proj_man,
							deverl_proj_tree_wx,
							deverl_sb_wx,
							deverl_sl_wx,
	 						deverl_stdout_wx,
							deverl_sys_pref_defs,
							deverl_sys_pref_gen,
							deverl_tabbed_win_img_wx,
              deverl_testpane
              ]},
	{registered, [deverl,
								deverl_stdout_wx,
								deverl_doc_man_wx,
								deverl_log_out_wx,
                deverl_proj_man,
                deverl_proj_tree_wx,
                deverl_sb_wx,
                deverl_sl_wx,
                deverl_console_parser,
                deverl_console_port_gen,
                deverl_console_wx,
                deverl_dlg_find_wx,
                deverl_dlg_import_proj_wx,
                deverl_dlg_new_file_wx,
                deverl_dlg_new_proj_wx,
                deverl_dlg_open_proj_wx,
                deverl_sys_pref_gen]},
  {applications, [kernel, stdlib]},
  {mod, {deverl_app, []}}
]}.
