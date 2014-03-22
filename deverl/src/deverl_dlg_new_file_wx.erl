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
%% @doc Displays the XRC based <em>New File</em> dialog.
%% @end
%% =====================================================================

-module(deverl_dlg_new_file_wx).

-include_lib("wx/include/wx.hrl").
-include("deverl.hrl").

%% wx_object
-behaviour(wx_object).
-export([init/1, terminate/2, code_change/3, handle_event/2,
         handle_call/3, handle_cast/2, handle_info/2]).

%% API
-export([start/1,
         get_path/1,
         get_project_id/1,
         get_type/1,
         destroy/1]).

%% Macros
-define(FILE_TYPE_ERLANG,   0).
-define(FILE_TYPE_TEXT,     1).
-define(FILE_TYPES,   ["Erlang",
                       "Plain Text"]).
-define(MODULE_TYPES, [{erlang_basic, "Erlang Module",
                                      ".erl",
                                      "A standard Erlang module."},
                       {header, "Erlang Header File",
                                ".hrl",
                                "An Erlang header file. Used for defining global macros, types and records."},
                       {application, "OTP Application",
                                     ".app",
                                     "An application specification. Applications group together common modules, allowing them "
                                      "to be started and stopped as a single unit. Erlang applications may also be used easily by "
                                      "other systems."},
                       {gen_server, "OTP Gen Server",
                                    ".erl",
                                    "A behaviour module for implementing a client-server relationship."},
                       {supervisor, "OTP Supervisor",
                                    ".erl",
                                    "OTP supervisor behaviour. A supervisor is responsible for monitoring child "
                                    "processes. This includes starting, stopping, and restarting processes "
                                    "where necessary."},
                       {event_server, "OTP Gen FSM",
                                      ".erl",
                                      "A behaviour module for implementing a generic finite state machine."},
                       {wx_object, "Wx Object",
                                   ".erl",
                                   "A behaviour module for the wxWidgets binding. It works like a regular gen_server module."}
                       ]).

%% Server state
-record(state, {dlg,
                path        :: string(),
                project_id  :: project_id(),
                type
               }).


%% =====================================================================
%% Client API
%% =====================================================================

%% =====================================================================
%% @doc

-spec start(Config) -> wxWindow:wxWindow() when
  Config :: list().

start(Config) ->
  wx_object:start({local, ?MODULE}, ?MODULE, Config, []).


%% =====================================================================
%% @doc

-spec get_path(wxDialog:wxDialog()) -> path().

get_path(This) ->
  wx_object:call(This, get_path).


%% =====================================================================
%% @doc

-spec get_project_id(wxDialog:wxDialog()) -> project_id() | undefined.

get_project_id(This) ->
  case wx_object:call(This, get_project_id) of
    "No Project" ->
      undefined;
    Id ->
      Id
  end.


%% =====================================================================
%% @doc Return the type of the module (for loading correct skeleton).

-spec get_type(wxDialog:wxDialog()) -> plain_text
                                     | erlang_basic
                                     | header
                                     | supervisor
                                     | application
                                     | gen_server
                                     | event_server
                                     | wx_object.

get_type(This) ->
  wx_object:call(This, get_type).


%% =====================================================================
%% @doc

-spec destroy(wxDialog:wxDialog()) -> ok.

destroy(This) ->
  wx_object:call(This, shutdown).


%% =====================================================================
%% Callback functions
%% =====================================================================
%% @hidden
init({Parent, Projects, ActiveProject}) ->
  Xrc = wxXmlResource:get(),
  Dlg = wxDialog:new(),
  deverl_lib_dlg_wx:win_variant(Dlg),

  %% Load XRC (Assumes all XRC handlers init previously)
  wxXmlResource:loadDialog(Xrc, Dlg, wx:null(), "new_file"),

  %% Add open projects
  Project = case ActiveProject of
    undefined -> "No Project";
    _ -> deverl_proj_man:get_name(ActiveProject)
  end,
  ProjectChoice = wxXmlResource:xrcctrl(Dlg, "proj_choice", wxChoice),
  wxChoice:append(ProjectChoice, "No Project", undefined),
  add_project_data(ProjectChoice, Projects),
  wxChoice:setSelection(ProjectChoice, wxChoice:findString(ProjectChoice, Project)),

  %% Set project name (panel 2)
  ProjNameSt = wxXmlResource:xrcctrl(Dlg, "proj_name_st", wxStaticText),
  wxStaticText:setLabel(ProjNameSt, wxChoice:getString(ProjectChoice, wxChoice:getSelection(ProjectChoice))),

  %% Add file types
  FileTypeList = wxXmlResource:xrcctrl(Dlg, "file_type_lb", wxListBox),
  wxListBox:insertItems(FileTypeList, ?FILE_TYPES, 0),
  wxListBox:setSelection(FileTypeList, 0),
  wxListBox:setFirstItem(FileTypeList, 0),

  %% Add module types
  ModuleTypeList = wxXmlResource:xrcctrl(Dlg, "mod_type_lb", wxListBox),
  InsertMod = fun({Type, Name, Ext, Desc}) ->
    Str = io_lib:format("~-30s[~s]", [Name, Ext]),
    wxListBox:append(ModuleTypeList, Str, {Type, Ext, Desc})
  end,
  lists:foreach(InsertMod, ?MODULE_TYPES),
  wxListBox:setSelection(ModuleTypeList, 0),
  wxListBox:setFirstItem(ModuleTypeList, 0),
  

  UpdatePath = fun(Filename) ->
    ProjPath = case wxChoice:getString(ProjectChoice, wxChoice:getSelection(ProjectChoice)) of
      "No Project" -> wx_misc:getHomeDir();
      _ ->
        filename:join(deverl_proj_man:get_root(wxChoice:getClientData(ProjectChoice, wxChoice:getSelection(ProjectChoice))), get_default_folder_text(Dlg))
    end,
    ProjPathTc = wxXmlResource:xrcctrl(Dlg, "path_tc", wxTextCtrl),
    set_path_text(Dlg, ProjPathTc, ProjPath, Filename)
  end,
  UpdatePath(""),

  FileNameTc = wxXmlResource:xrcctrl(Dlg, "filename_tc", wxTextCtrl),
  CB = fun(_E,_O) ->
    Filename = wxTextCtrl:getValue(FileNameTc),
    check_if_finished(Dlg, Filename, wxXmlResource:xrcctrl(Dlg, "desc_string", wxStaticText)),
    UpdatePath(wxTextCtrl:getValue(FileNameTc))
  end,
  wxTextCtrl:connect(FileNameTc, command_text_updated, [{callback, CB}]),

  ChoiceFun = fun(_EvtRec, EvtObj) ->
    ProjNameSt = wxXmlResource:xrcctrl(Dlg, "proj_name_st", wxStaticText),
    wxStaticText:setLabel(ProjNameSt, wxCommandEvent:getString(EvtObj)),
    UpdatePath(wxTextCtrl:getValue(FileNameTc))
  end,
  wxChoice:connect(ProjectChoice, command_choice_selected, [{callback, ChoiceFun}]),

  %% -------------------------------------------------------------------
  %% The following handlers crash the ERTS when destroying the dialog in Linux.
  %% We suspect a bug in the erlang release we are currently using (R16B01).
  %% We have used a work around for this below (using userData to match on the event).
  %% In future versions of erlang the following may work...

  %% Listbox 1 handler
  %CB1 = fun(E, O) ->
  %    wxListBox:enable(ModuleTypeList, [{enable, not wxListBox:isEnabled(ModuleTypeList)}]),
  %    UpdatePath(wxTextCtrl:getValue(FileNameTc)) %% change for update_path/2
  %end,
  %wxDialog:connect(FileTypeList, command_listbox_selected, [{callback, CB1}]),

  %% Listbox 2 handler
  %CB2 = fun(_E,_O) ->
  %  UpdatePath(wxTextCtrl:getValue(FileNameTc)) %% change for update_path/2
  %end,
  %wxDialog:connect(ModuleTypeList, command_listbox_selected, [{callback, CB2}]),

  %% -------------------------------------------------------------------

  wxDialog:connect(FileTypeList, command_listbox_selected, [{userData, 0}]),
  wxDialog:connect(ModuleTypeList, command_listbox_selected, [{userData, 1}]),

  %% Browse button
  BrowseBtn = wxXmlResource:xrcctrl(Dlg, "browse_btn", wxButton),
  Browse = fun(_E,_O) ->
    case wxChoice:getString(ProjectChoice, wxChoice:getSelection(ProjectChoice)) of
      "No Project" -> %% Standalone file can save anywhere
        DirectoryChoice = deverl_lib_dlg_wx:get_existing_dir(Parent),
        case DirectoryChoice of
          cancelled -> ok;
          _ ->
            FileNameTc = wxXmlResource:xrcctrl(Dlg, "filename_tc", wxTextCtrl),
            ProjPathTc = wxXmlResource:xrcctrl(Dlg, "path_tc", wxTextCtrl),
            set_path_text(Dlg, ProjPathTc, DirectoryChoice, wxTextCtrl:getValue(FileNameTc))
        end;
      _ ->
        ProjectId = wxChoice:getClientData(ProjectChoice, wxChoice:getSelection(ProjectChoice)),
        Path = case ProjectId of
          undefined ->
            wx_misc:getHomeDir();
          _ ->
            deverl_proj_man:get_root(ProjectId)
        end,
        custom_browse_dialog(Dlg, filename:join(Path, get_default_folder_text(Dlg)), ProjectId)
    end
  end,
  wxButton:connect(BrowseBtn, command_button_clicked, [{callback, Browse}]),

  %% Back/next buttons
  NextBtn = wxXmlResource:xrcctrl(Dlg, "next_btn", wxButton),
  BackBtn = wxXmlResource:xrcctrl(Dlg, "back_btn", wxButton),

  %% Get swapped panels
  Panel1 = wxXmlResource:xrcctrl(Dlg, "swap_1", wxPanel),
  Panel2 = wxXmlResource:xrcctrl(Dlg, "swap_2", wxPanel),
  SwapCtnr = wxXmlResource:xrcctrl(Dlg, "swap_cont", wxPanel),
  SwapSz = wxWindow:getSizer(SwapCtnr),

  Swap = 
  fun(#wx{userData=show_panel_1}, _O) ->
    %% Swap ctrls
    Bool1 = wxSizer:hide(SwapSz, Panel2),
    Bool2 = wxSizer:show(SwapSz, Panel1),
    wxWindow:enable(NextBtn),
    wxWindow:disable(BackBtn);
  (#wx{userData=show_panel_2}, _O) ->
    %% Swap ctrls
    Bool1 = wxSizer:hide(SwapSz, Panel1),
    Bool2 = wxSizer:show(SwapSz, Panel2),
    wxWindow:enable(BackBtn),
    wxWindow:disable(NextBtn),
    wxSizer:layout(SwapSz)
  end,
  
  wxButton:connect(BackBtn, command_button_clicked, [{callback, Swap}, {userData, show_panel_1}]),
  wxButton:connect(NextBtn, command_button_clicked, [{callback, Swap}, {userData, show_panel_2}]),
  
  %% Set the description to the selected module
  set_module_description(Dlg),

  %% Overide OK handler, normal event
  wxButton:connect(Dlg, command_button_clicked, [{id, ?wxID_OK}]),

  State=#state{
    dlg=Dlg
  },
  {Dlg, State}.
  
%% @hidden
handle_cast(_Msg, State) ->
  {noreply, State}.
%% @hidden
handle_info(_Info, State) ->
  {noreply, State}.
%% @hidden
handle_call(shutdown, _From, State) ->
  {stop, normal, ok, State};
handle_call(get_project_id, _From, State) ->
  {reply, State#state.project_id, State};
handle_call(get_path, _From, State) ->
  {reply, State#state.path, State};
handle_call(get_type, _From, State) ->
  {reply, State#state.type, State}.
%% @hidden
code_change(_, _, State) ->
  {ok, State}.
%% @hidden
terminate(_Reason, #state{dlg=Dialog}) ->
  wxDialog:destroy(Dialog),
  ok.


%% =====================================================================
%% Event handlers
%% =====================================================================

handle_event(#wx{id=?wxID_OK=Id, event=#wxCommand{type=command_button_clicked}}, State=#state{dlg=Dlg}) ->
  ProjPathTc = wxXmlResource:xrcctrl(Dlg, "path_tc", wxTextCtrl),
  ProjectChoice = wxXmlResource:xrcctrl(Dlg, "proj_choice", wxChoice),
  FileTypeList = wxXmlResource:xrcctrl(Dlg, "file_type_lb", wxListBox),
  ModuleTypeList = wxXmlResource:xrcctrl(Dlg, "mod_type_lb", wxListBox),
  Type1 = case wxListBox:getSelection(FileTypeList) of
    ?FILE_TYPE_ERLANG ->
      {Type, _Ext, _Desc} = wxListBox:getClientData(ModuleTypeList, wxListBox:getSelection(ModuleTypeList)),
      Type;
    ?FILE_TYPE_TEXT ->
      plain_text
  end,
  State1=State#state{path=wxTextCtrl:getValue(ProjPathTc),
                project_id=wxChoice:getClientData(ProjectChoice, wxChoice:getSelection(ProjectChoice)),
                type=Type1
                },
  wxDialog:endModal(Dlg, Id),
  {noreply, State1};

handle_event(#wx{obj=FileTypeList, userData=0, event=#wxCommand{type=command_listbox_selected}}, State=#state{dlg=Dlg}) ->
  ModuleTypeList = wxXmlResource:xrcctrl(Dlg, "mod_type_lb", wxListBox),
  case wxListBox:getStringSelection(FileTypeList) of
    "Erlang" ->
      set_module_description(Dlg);
    "Plain Text" ->
      set_description(Dlg, "A plain text file. Can be used to make notes and readme files.")
  end,
  FileNameTc = wxXmlResource:xrcctrl(Dlg, "filename_tc", wxTextCtrl),
  wxListBox:enable(ModuleTypeList, [{enable, not wxListBox:isEnabled(ModuleTypeList)}]),
  update_path(Dlg, wxTextCtrl:getValue(FileNameTc)),
  {noreply, State};
handle_event(#wx{obj=ListBox, userData=1, event=#wxCommand{type=command_listbox_selected, commandInt=Item}}, State=#state{dlg=Dlg}) ->
  {_Type, _Ext, Desc} = wxListBox:getClientData(ListBox, Item),
  set_description(Dlg, Desc),
  FileNameTc = wxXmlResource:xrcctrl(Dlg, "filename_tc", wxTextCtrl),
  update_path(Dlg, wxTextCtrl:getValue(FileNameTc)),
  {noreply, State}.


%% =====================================================================
%% Internal functions
%% =====================================================================

%% =====================================================================
%% @doc Used to dynamically update the path of the file to be created.

update_path(Dlg, Filename) ->
  ProjectChoice = wxXmlResource:xrcctrl(Dlg, "proj_choice", wxChoice),
  ProjPath = case wxChoice:getString(ProjectChoice, wxChoice:getSelection(ProjectChoice)) of
    "No Project" ->
      wx_misc:getHomeDir();
    _ ->
      filename:join(deverl_proj_man:get_root(wxChoice:getClientData(ProjectChoice, wxChoice:getSelection(ProjectChoice))), get_default_folder_text(Dlg))
  end,
  ProjPathTc = wxXmlResource:xrcctrl(Dlg, "path_tc", wxTextCtrl),
  set_path_text(Dlg, ProjPathTc, ProjPath, Filename).


%% =====================================================================
%% @doc Set the description from the module list as the description.

set_module_description(Dlg) ->
  ListBox = wxXmlResource:xrcctrl(Dlg, "mod_type_lb", wxListBox),
  {_Type, _Ext, Desc} = wxListBox:getClientData(ListBox, wxListBox:getSelection(ListBox)),
  set_description(Dlg, Desc),
  ok.
  
  
%% =====================================================================
%% @doc Set the description box based on the list item selected.

set_description(Dlg, Desc) ->
  Ctrl = wxXmlResource:xrcctrl(Dlg, "desc_string", wxStaticText),
  wxStaticText:setLabel(Ctrl, Desc),
  {W, _H} = wxWindow:getSize(wxWindow:getParent(Ctrl)),
  wxStaticText:wrap(Ctrl, W).


%% =====================================================================
%% @doc Set the path text to a given path.

-spec set_path_text(wxDialog:wxDialog(), wxTextCtrl:wxTextCtrl(), path(), string()) -> ok.

set_path_text(Dlg, PathTextBox, Path, Filename) ->
  wxTextCtrl:clear(PathTextBox),
  case string:len(Filename) of
    0 ->
      wxTextCtrl:appendText(PathTextBox, Path);
    _ ->
      wxTextCtrl:appendText(PathTextBox, filename:join(Path,Filename ++ get_file_extension(Dlg)))
  end.


%% =====================================================================
%% @doc Get the default folder depending on what file type is selected.

-spec get_default_folder_text(wxDialog:wxDialog()) -> string().

get_default_folder_text(Dlg) ->
  ProjectChoice = wxXmlResource:xrcctrl(Dlg, "proj_choice", wxChoice),
  FileTypeList = wxXmlResource:xrcctrl(Dlg, "file_type_lb", wxListBox),
  ModuleTypeList = wxXmlResource:xrcctrl(Dlg, "mod_type_lb", wxListBox),
  case wxChoice:getSelection(ProjectChoice) of
    0 -> %% No Project
      "";
    _ ->
      case wxListBox:getSelection(FileTypeList) of
        ?FILE_TYPE_ERLANG ->
          case wxListBox:getClientData(ModuleTypeList, wxListBox:getSelection(ModuleTypeList)) of
            {header, _Ext, _Desc} ->  "include";
            {_Type, _Ext, _Desc} -> "src"
          end;
        ?FILE_TYPE_TEXT ->
          ""
      end
  end.


%% =====================================================================
%% @doc Get the file extension that should be used when creating new file.

-spec get_file_extension(wxWindow:wxWindow()) -> string().

get_file_extension(Dlg) ->
  FileTypeList = wxXmlResource:xrcctrl(Dlg, "file_type_lb", wxListBox),
  ModuleTypeList = wxXmlResource:xrcctrl(Dlg, "mod_type_lb", wxListBox),
  case wxListBox:getSelection(FileTypeList) of
    ?FILE_TYPE_ERLANG ->
      {_Type, Ext, _Desc} = wxListBox:getClientData(ModuleTypeList, wxListBox:getSelection(ModuleTypeList)),
      Ext;
    ?FILE_TYPE_TEXT ->
      ".txt"
  end.


%% =====================================================================
%% @doc Check if the dialog is in a finished state.

-spec check_if_finished(wxWindow:wxWindow(), string(), wxPanel:wxPanel()) -> boolean().

check_if_finished(Dlg, Filename, Desc) ->
  Finish = wxXmlResource:xrcctrl(Dlg, "wxID_OK", wxButton),
  Back = wxXmlResource:xrcctrl(Dlg, "back_btn", wxButton),
  case length(Filename) of
    0 ->
      set_module_description(Dlg),
      swap_desc_icons(Dlg, show_info_icn),
      wxWindow:enable(Back),
      wxWindow:disable(Finish);
    _ ->    
      case validate_name(Dlg, Filename, Desc) of
        true ->
          wxWindow:enable(Back),
          wxWindow:enable(Finish);
        false ->
          wxWindow:disable(Back),
          wxWindow:disable(Finish)
      end
  end.


%% =====================================================================
%% @doc Validate the input Name.

validate_name(Dlg, Str, Desc) ->
	case validate_name(Str) of
		nomatch ->
      ErrBmp = wxXmlResource:xrcctrl(Dlg, "img_error", wxStaticBitmap),
      case wxWindow:isShown(ErrBmp) of
        false -> ok;
        _ ->
          set_module_description(Dlg)
      end,
      swap_desc_icons(Dlg, show_info_icn),
			true;
		{match, [{Pos,_}]} ->
      swap_desc_icons(Dlg, show_error_icn),
      set_description(Dlg, "Illegal character \"" ++ [lists:nth(Pos + 1, Str)] ++ "\" in filename."),
			false
	end.
  
  
%% =====================================================================
%% @doc
  
swap_desc_icons(Dlg, ToShow) ->
  InfoBmp = wxXmlResource:xrcctrl(Dlg, "img_info", wxStaticBitmap),
  ErrBmp = wxXmlResource:xrcctrl(Dlg, "img_error", wxStaticBitmap),
  case ToShow of
    show_info_icn ->
      wxWindow:hide(ErrBmp),
      wxWindow:show(InfoBmp);
    show_error_icn ->
      wxWindow:hide(InfoBmp),
      wxWindow:show(ErrBmp)
  end,
  IcnSz = wxWindow:getSizer(wxWindow:getParent(InfoBmp)),
  wxSizer:layout(IcnSz),
  ok.


%% =====================================================================
%% @doc Validate the input string.

-spec validate_name(string()) -> {match, {integer, integer}} | nomatch.

validate_name(Str) ->
	re:run(Str, "[/\]").


%% =====================================================================
%% @doc Add project labels and data to wxChoice.

-spec add_project_data(wxChoice:wxChoice(), [project_id()]) -> ok.

add_project_data(_, []) ->
  ok;
add_project_data(ProjectChoice, [ProjectId|Projects]) ->
  wxChoice:append(ProjectChoice, deverl_proj_man:get_name(ProjectId), ProjectId),
  add_project_data(ProjectChoice, Projects).


%% =====================================================================
%% Custom tree dialog functions
%% =====================================================================

%% =====================================================================
%% @doc Create the custom tree dialog

custom_browse_dialog(Parent, Path, ProjectId) ->
  Xrc = wxXmlResource:get(),
  Dlg = wxDialog:new(),
  deverl_lib_dlg_wx:win_variant(Dlg),
  wxXmlResource:loadDialog(Xrc, Dlg, Parent, "custom_tree_dlg"),

  Tree = wxXmlResource:xrcctrl(Dlg, "browse_tree", wxTreeCtrl),
  RootItem = wxTreeCtrl:addRoot(Tree, filename:basename(Path), [{data, Path}]),
  build_tree(Tree, RootItem, ProjectId),
  wxTreeCtrl:expand(Tree, RootItem),

  OkButton = wxXmlResource:xrcctrl(Dlg, "wxID_OK", wxButton),
  Ok = fun(_E,_O) ->
    Selection = wxTreeCtrl:getSelection(Tree),
    ProjPathTc = wxXmlResource:xrcctrl(Parent, "path_tc", wxTextCtrl),
    set_path_text(Parent, ProjPathTc, wxTreeCtrl:getItemData(Tree, Selection) ++ "/",
        wxTextCtrl:getValue(ProjPathTc)),
    wxDialog:endModal(Dlg, ?wxID_OK)
  end,
  wxButton:connect(OkButton, command_button_clicked, [{callback, Ok}]),

  wxDialog:centre(Dlg),
  wxDialog:showModal(Dlg),
  wxDialog:destroy(Dlg).


%% =====================================================================
%% @doc Build the tree structure for a given tree item.

-spec build_tree(wxTreeCtrl:wxTreeCtrl(), integer(), project_id()) -> ok.

build_tree(Tree, Item, ProjectId) ->
  Root = wxTreeCtrl:getItemData(Tree, Item),
  Files = filelib:wildcard(Root ++ "/*"),
  add_files(Tree, Item, Files, ProjectId).


%% =====================================================================
%% @doc Add the files to the given tree item.

-spec add_files(wxTreeCtrl:wxTreeCtrl(), integer(), [path()], project_id()) -> ok.

add_files(_, _, [], _) ->
	ok;
add_files(Tree, Root, [File|Files], ProjectId) ->
	FileName = filename:basename(File),
	IsDir = filelib:is_dir(File),
	case IsDir of
		true ->
			Child = wxTreeCtrl:appendItem(Tree, Root, FileName, [{data, File}]),
			wxTreeCtrl:setItemImage(Tree, Child, 0),
			build_tree(Tree, Child, ProjectId);
		_ ->
			ok
	end,
	add_files(Tree, Root, Files, ProjectId).
