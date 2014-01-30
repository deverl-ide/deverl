%% =====================================================================
%% @author
%% @copyright
%% @title
%% @version
%% @doc
%% A module that constructs and displays a modal dialog box for creating
%% a new file. Handlers and internal functions deal with user input and
%% functionality.
%% @end
%% =====================================================================

-module(ide_dlg_new_file_wx).

-include_lib("wx/include/wx.hrl").
-include("ide.hrl").

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
                                    "A behaviour module for implementing the server of a client-server relationship."},
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

init({Parent, Projects, ActiveProject}) ->
  Xrc = wxXmlResource:get(),
  Dlg = wxDialog:new(),
  ide_lib_dlg_wx:win_var(Dlg),

  %% Load XRC (Assumes all XRC handlers init previously)
  wxXmlResource:loadDialog(Xrc, Dlg, Parent, "new_file"),

  %% Add open projects
  Project = case ActiveProject of
    undefined -> "No Project";
    _ -> ide_proj_man:get_name(ActiveProject)
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

  %% Add module types
  ModuleTypeList = wxXmlResource:xrcctrl(Dlg, "mod_type_lb", wxListBox),
  InsertMod = fun({Type, Name, Ext, Desc}) ->
    Str = io_lib:format("~-30s[~s]", [Name, Ext]),
    wxListBox:append(ModuleTypeList, Str, {Type, Ext, Desc})
  end,
  lists:foreach(InsertMod, ?MODULE_TYPES),
  wxListBox:setSelection(ModuleTypeList, 0),

  UpdatePath = fun(Filename) ->
    ProjPath = case wxChoice:getString(ProjectChoice, wxChoice:getSelection(ProjectChoice)) of
      "No Project" -> wx_misc:getHomeDir();
      _ ->
        filename:join(ide_proj_man:get_root(wxChoice:getClientData(ProjectChoice, wxChoice:getSelection(ProjectChoice))), get_default_folder_text(Dlg))
    end,
    ProjPathTc = wxXmlResource:xrcctrl(Dlg, "path_tc", wxTextCtrl),
    set_path_text(Dlg, ProjPathTc, ProjPath, Filename)
  end,
  UpdatePath(""),

  FileNameTc = wxXmlResource:xrcctrl(Dlg, "filename_tc", wxTextCtrl),
  CB = fun(_E,_O) ->
    Filename = wxTextCtrl:getValue(FileNameTc),
    check_if_finished(Dlg, Filename, wxXmlResource:xrcctrl(Dlg, "info_string", wxStaticText)),
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
  %% The following handlers crash the ERTS when destroying the dialog in Linux
  %% We suspect a bug in the erlang release we are currently using (R16B01).
  %% We have used a work around for this below (using userData to match on the event).
  %% In future versions of erlang the following may work...
  
  %% Listbox 1 handler
  %CB1 = fun(E, O) ->
  %    wxListBox:enable(ModuleTypeList, [{enable, not wxListBox:isEnabled(ModuleTypeList)}]),
  %    UpdatePath(wxTextCtrl:getValue(FileNameTc))
  %end,
  %wxDialog:connect(FileTypeList, command_listbox_selected, [{callback, CB1}]),

  %% Listbox 2 handler
  %CB2 = fun(_E,_O) ->
  %  UpdatePath(wxTextCtrl:getValue(FileNameTc))
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
        DirectoryChoice = ide_lib_dlg_wx:get_existing_dir(Parent),
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
            ide_proj_man:get_root(ProjectId)
        end,
        custom_browse_dialog(Dlg, filename:join(Path, get_default_folder_text(Dlg)), ProjectId)
    end
  end,
  wxButton:connect(BrowseBtn, command_button_clicked, [{callback, Browse}]),

  %% Back/next buttons
  NextBtn1 = wxXmlResource:xrcctrl(Dlg, "next_btn", wxButton),
  BackBtn2 = wxXmlResource:xrcctrl(Dlg, "back_btn2", wxButton),

  Panel1 = wxXmlResource:xrcctrl(Dlg, "panel_1", wxPanel),
  Panel2 = wxXmlResource:xrcctrl(Dlg, "panel_2", wxPanel),
  Swap = fun(#wx{userData={Show, Hide}}, _O) ->
    wxPanel:hide(Hide),
    wxPanel:show(Show)
  end,

  wxButton:connect(BackBtn2, command_button_clicked, [{callback, Swap}, {userData, {Panel1, Panel2}}]),
  wxButton:connect(NextBtn1, command_button_clicked, [{callback, Swap}, {userData, {Panel2, Panel1}}]),

  %% Overide OK handler, normal event
  wxButton:connect(Dlg, command_button_clicked, [{id, ?wxID_OK}]),

  State=#state{
    dlg=Dlg
  },
  {Dlg, State}.


handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

handle_call(shutdown, _From, State) ->
  {stop, normal, ok, State};
handle_call(get_project_id, _From, State) ->
  {reply, State#state.project_id, State};
handle_call(get_path, _From, State) ->
  {reply, State#state.path, State};
handle_call(get_type, _From, State) ->
  {reply, State#state.type, State}.

code_change(_, _, State) ->
  {ok, State}.

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
handle_event(#wx{userData=0, event=#wxCommand{type=command_listbox_selected}}, State=#state{dlg=Dlg}) ->
  ModuleTypeList = wxXmlResource:xrcctrl(Dlg, "mod_type_lb", wxListBox),
  FileNameTc = wxXmlResource:xrcctrl(Dlg, "filename_tc", wxTextCtrl),  
  wxListBox:enable(ModuleTypeList, [{enable, not wxListBox:isEnabled(ModuleTypeList)}]),
  update_path(Dlg, wxTextCtrl:getValue(FileNameTc)),
  {noreply, State};
handle_event(#wx{userData=1, event=#wxCommand{type=command_listbox_selected}}, State=#state{dlg=Dlg}) ->
  FileNameTc = wxXmlResource:xrcctrl(Dlg, "filename_tc", wxTextCtrl),  
  update_path(Dlg, wxTextCtrl:getValue(FileNameTc)),
  {noreply, State}.


%% =====================================================================
%% Internal functions
%% =====================================================================

update_path(Dlg, Filename) ->
  ProjectChoice = wxXmlResource:xrcctrl(Dlg, "proj_choice", wxChoice),
  ProjPath = case wxChoice:getString(ProjectChoice, wxChoice:getSelection(ProjectChoice)) of
    "No Project" -> 
      wx_misc:getHomeDir();
    _ ->
      filename:join(ide_proj_man:get_root(wxChoice:getClientData(ProjectChoice, wxChoice:getSelection(ProjectChoice))), get_default_folder_text(Dlg))
  end,
  ProjPathTc = wxXmlResource:xrcctrl(Dlg, "path_tc", wxTextCtrl),
  set_path_text(Dlg, ProjPathTc, ProjPath, Filename).
  

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
            {header, _Ext} ->  "include";
            {_Type, _Ext} -> "src"
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
  case length(Filename) of
    0 ->
      wxWindow:disable(Finish);
    _ ->
      case validate_name(Filename, Desc) of
        true ->
          wxWindow:enable(Finish);
        false ->
          wxWindow:disable(Finish)
      end
  end.


%% =====================================================================
%% @doc Validate the input Name.

-spec validate_name(string(), wxPanel:wxPanel()) -> boolean().

validate_name([], _) -> false;
validate_name(Str, Desc) ->
	case validate_name(Str) of
		nomatch ->
      % insert_desc(Desc, "Create a new file."),
			true;
		{match, [{Pos,_}]} ->
      % Bitmap = wxBitmap:new(wxImage:new("../icons/prohibition.png")),
      % insert_desc(Desc, "Illegal character \"" ++ [lists:nth(Pos + 1, Str)] ++ "\" in filename.", [{bitmap, Bitmap}]),
			false
	end.

-spec validate_name(string()) -> {match, {integer, integer}} | nomatch.

validate_name(Str) ->
	re:run(Str, "[/\]").


%% =====================================================================
%% @doc Add project labels and data to wxChoice.

-spec add_project_data(wxChoice:wxChoice(), [project_id()]) -> ok.

add_project_data(_, []) ->
  ok;
add_project_data(ProjectChoice, [ProjectId|Projects]) ->
  wxChoice:append(ProjectChoice, ide_proj_man:get_name(ProjectId), ProjectId),
  add_project_data(ProjectChoice, Projects).


%% =====================================================================
%% Custom tree dialog functions
%% =====================================================================

%% =====================================================================
%% @doc Create the custom tree dialog

custom_browse_dialog(Parent, Path, ProjectId) ->
  Xrc = wxXmlResource:get(),
  Dlg = wxDialog:new(),
  ide_lib_dlg_wx:win_var(Dlg),
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