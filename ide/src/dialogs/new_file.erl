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

-module(new_file).

-include_lib("wx/include/wx.hrl").

-behaviour(wx_object).

%% wx_object
-export([init/1, terminate/2, code_change/3, handle_event/2,
         handle_call/3, handle_cast/2, handle_info/2]).
%% API
-export([start/1]).

-record(state, {win,
                dialog1    :: wxPanel:wxPanel(),
                dialog2    :: wxPanel:wxPanel(),
                swap_sizer :: wxSizer:wxSizer(),
                projects   :: [project_manager:project_id()]
               }).

-define(FILE_TYPE_ERLANG, 0).
-define(FILE_TYPE_TEXT,   1).

-define(BACK_BUTTON,   9000).
-define(NEXT_BUTTON,   9001).
-define(FINISH_BUTTON, 9002).
-define(BROWSE_BUTTON, 9003).

-define(PROJECT_CHOICE,     9004).
-define(FILE_TYPE_CHOICE,   9005).
-define(MODULE_TYPE_CHOICE, 9006).
-define(DESCRIPTION_BOX,    9007).

-define(FILENAME_BOX, 9008).
-define(PROJECT_TEXT, 9009).
-define(FOLDER_BOX,   9010).
-define(PATH_TEXT,    9011).

-define(FILE_TYPES,   ["Erlang",
                       "Plain Text"]).

-define(MODULE_TYPES, ["Erlang Module (.erl)",
                       "Erlang Header File (.hrl)",
                       "OTP Application (.erl)",
                       "OTP Gen Server (.erl)",
                       "OTP Supervisor (.erl)"]).


%% =====================================================================
%% Client API
%% =====================================================================

start(Config) ->
  %wx_object:start_link({local, ?MODULE}, ?MODULE, Config, []).
  wx_object:start({local, ?MODULE}, ?MODULE, Config, []).


%% =====================================================================
%% Callback functions
%% =====================================================================


init({Parent, Projects, ActiveProject}) ->
  Dialog = wxDialog:new(Parent, ?wxID_ANY, "New File", [{size,{640, 500}},
                                                        {style, ?wxDEFAULT_DIALOG_STYLE bor
                                                                ?wxRESIZE_BORDER bor
                                                                ?wxDIALOG_EX_METAL}]),
	%% Conditional compilation OSX
	case os:type() of
		{_, darwin} ->
			wxWindow:setWindowVariant(Dialog, ?wxWINDOW_VARIANT_SMALL);
		 _ -> ok
	end,

  LRSizer = wxBoxSizer:new(?wxHORIZONTAL),
  wxSizer:addSpacer(LRSizer, 20),
  wxDialog:setSizer(Dialog, LRSizer),

  MainSizer = wxBoxSizer:new(?wxVERTICAL),
  wxSizer:add(LRSizer, MainSizer, [{proportion, 1}, {flag, ?wxEXPAND}]),
  wxSizer:addSpacer(MainSizer, 40),
  wxSizer:add(MainSizer, wxStaticText:new(Dialog, ?wxID_ANY, "New File"), []),
  wxSizer:addSpacer(MainSizer, 5),
  wxSizer:add(MainSizer, wxStaticLine:new(Dialog, [{style, ?wxLI_HORIZONTAL}]), [{flag, ?wxEXPAND}]),
  wxSizer:addSpacer(MainSizer, 20),

  %% Swap dialog
  SwapSizer = wxBoxSizer:new(?wxVERTICAL),
  Dialog1 = dialog1(Dialog, Projects, ActiveProject),
  wxSizer:add(SwapSizer, Dialog1,   [{proportion, 1}, {flag, ?wxEXPAND}]),
  wxSizer:add(MainSizer, SwapSizer, [{proportion, 1}, {flag, ?wxEXPAND}]),
  wxSizer:addSpacer(MainSizer, 20),

  %% Description Box
  wxSizer:add(MainSizer, wxStaticLine:new(Dialog, [{style, ?wxLI_HORIZONTAL}]), [{flag, ?wxEXPAND}]),
  wxSizer:addSpacer(MainSizer, 20),
  wxSizer:add(MainSizer, wxStaticText:new(Dialog, ?wxID_ANY, "Description"), []),
  wxSizer:addSpacer(MainSizer, 5),
  wxSizer:add(MainSizer, wxTextCtrl:new(Dialog, ?DESCRIPTION_BOX, [{size,{-1, 70}}, {style, ?wxTE_MULTILINE bor ?wxTE_READONLY}]), [{proportion, 0}, {flag, ?wxEXPAND}]),
  wxSizer:addSpacer(MainSizer, 40),

  %% Buttons
  wxSizer:add(MainSizer, wxStaticLine:new(Dialog, [{style, ?wxLI_HORIZONTAL}]), [{flag, ?wxEXPAND}]),
  wxSizer:addSpacer(MainSizer, 20),
  ButtonPanel = wxPanel:new(Dialog),
  ButtonSizer = wxBoxSizer:new(?wxHORIZONTAL),
  wxPanel:setSizer(ButtonPanel, ButtonSizer),
  wxSizer:add(ButtonSizer, wxButton:new(ButtonPanel, ?BACK_BUTTON,   [{label, "Back"}]),   [{border, 2}, {flag, ?wxALL}]),
  wxSizer:add(ButtonSizer, wxButton:new(ButtonPanel, ?NEXT_BUTTON,   [{label, "Next"}]),   [{border, 2}, {flag, ?wxALL}]),
  wxSizer:add(ButtonSizer, wxButton:new(ButtonPanel, ?FINISH_BUTTON, [{label, "Finish"}]), [{border, 2}, {flag, ?wxALL}]),
  wxSizer:add(ButtonSizer, wxButton:new(ButtonPanel, ?wxID_CANCEL,   [{label, "Cancel"}]), [{border, 2}, {flag, ?wxALL}]),

  wxSizer:add(MainSizer, ButtonPanel, [{proportion, 0}, {flag, ?wxALIGN_RIGHT}]),

  wxSizer:addSpacer(LRSizer, 20),
  wxSizer:addSpacer(MainSizer, 20),
  wxDialog:connect(ButtonPanel, command_button_clicked, [{skip, true}]),
  wxDialog:connect(Dialog, close_window, []),

  wxButton:disable(wxWindow:findWindow(Parent, ?BACK_BUTTON)),
  wxButton:disable(wxWindow:findWindow(Parent, ?FINISH_BUTTON)),

  {Dialog, #state{win=Dialog, dialog1=Dialog1, swap_sizer=SwapSizer, projects=Projects}}.


handle_cast(_Msg, State) ->
  io:format("handle_cast/2: NEW FILE DIALOG"),
  {noreply, State}.

handle_info(_Info, State) ->
  io:format("handle_info/2: NEW FILE DIALOG"),
  {noreply, State}.

handle_call(shutdown, _From, State=#state{win=Dialog}) ->
  wxDialog:destroy(Dialog),
  {stop, normal, ok, State};
handle_call(win, _From, State) ->
  {reply, State#state.win, State};
handle_call(dialog1, _From, State) ->
  {reply, State#state.dialog1, State};
handle_call(dialog2, _From, State) ->
    {reply, State#state.dialog2, State};
handle_call(projects, _From, State) ->
    {reply, State#state.projects, State}.

code_change(_, _, State) ->
  {stop, not_yet_implemented, State}.

terminate(_Reason, #state{win=Dialog}) ->
  io:format("TERMINATE NEW FILE DIALOG~n"),
  wxDialog:endModal(Dialog, ?wxID_CANCEL),
  wxDialog:destroy(Dialog).


%% =====================================================================
%% Event handlers
%% =====================================================================

handle_event(#wx{event=#wxClose{}}, State) ->
  {stop, normal, State};
handle_event(#wx{id=?wxID_CANCEL, event=#wxCommand{type=command_button_clicked}},
             State=#state{win=Dialog}) ->
  {stop, normal, State};
handle_event(#wx{id=?NEXT_BUTTON, event=#wxCommand{type=command_button_clicked}},
             State=#state{win=Parent, dialog1=Dialog1, dialog2=undefined, swap_sizer=Sz}) ->
  Dialog2 = dialog2(Parent),
  swap(Sz, Dialog1, Dialog2),
  wxButton:enable(wxWindow:findWindow(Parent, ?BACK_BUTTON)),
  wxButton:disable(wxWindow:findWindow(Parent, ?NEXT_BUTTON)),
  {ProjectName, ProjectPath} = get_project_choice(Parent),
  set_project_text(Parent, ProjectName),
  set_default_folder_text(Parent),
  set_default_path_text(Parent, ProjectPath),
  {noreply, State#state{dialog2=Dialog2}};
handle_event(#wx{id=?NEXT_BUTTON, event=#wxCommand{type=command_button_clicked}},
             State=#state{win=Parent, dialog1=Dialog1, dialog2=Dialog2, swap_sizer=Sz}) ->
  swap(Sz, Dialog1, Dialog2),
  wxButton:enable(wxWindow:findWindow(Parent, ?BACK_BUTTON)),
  wxButton:disable(wxWindow:findWindow(Parent, ?NEXT_BUTTON)),
  {ProjectName, ProjectPath} = get_project_choice(Parent),
  set_project_text(Parent, ProjectName),
  set_default_folder_text(Parent),
  set_default_path_text(Parent, ProjectPath),
  check_if_finished(Parent),
  {noreply, State};
handle_event(#wx{id=?BACK_BUTTON, event=#wxCommand{type=command_button_clicked}},
             State=#state{win=Parent, dialog1=Dialog1, dialog2=Dialog2, swap_sizer=Sz}) ->
  swap(Sz, Dialog2, Dialog1),
  wxButton:enable(wxWindow:findWindow(Parent, ?NEXT_BUTTON)),
  wxButton:disable(wxWindow:findWindow(Parent, ?BACK_BUTTON)),
  wxButton:disable(wxWindow:findWindow(Parent, ?FINISH_BUTTON)),
  {noreply, State};
handle_event(#wx{id=?BROWSE_BUTTON, event=#wxCommand{type=command_button_clicked}},
             State=#state{win=Parent}) ->
  {ProjectName, ProjectPath} = get_project_choice(Parent),
  case ProjectName of
    "No Project" ->
      DirectoryChoice = lib_dialog_wx:get_existing_dir(Parent),
      case DirectoryChoice of
        cancelled ->
          ok;
        _ ->
          set_path_text(Parent, DirectoryChoice),
          set_folder_text(Parent, "/" ++ filename:basename(DirectoryChoice))
      end;
    _ ->
      browse_dialog(Parent, ProjectPath ++ get_default_folder_text(Parent))
  end,
  {noreply, State};
handle_event(#wx{id=?FINISH_BUTTON, event=#wxCommand{type=command_button_clicked}},
             State=#state{win=Parent, dialog1=_Dialog1, dialog2=_Dialog2}) ->
  Filename = get_filename(Parent) ++ get_file_extension(Parent),
  Path = get_path_text(Parent),
  {_, ProjectPath} = get_project_choice(Parent),
  ide_io:create_new_file(Path, Filename),
  case get_project_choice(Parent) of
    {"No Project", _} ->
      ok;
    _ ->
      ide_projects_tree:refresh_project(ProjectPath)
  end,
  {stop, normal, State};
handle_event(#wx{id=?FILE_TYPE_CHOICE, event=#wxCommand{type=command_listbox_selected, commandInt=Index}},
             State=#state{win=Parent}) ->
  case Index of
    ?FILE_TYPE_ERLANG ->
      wxListBox:enable(wxWindow:findWindow(Parent, ?MODULE_TYPE_CHOICE));
    ?FILE_TYPE_TEXT ->
      wxListBox:disable(wxWindow:findWindow(Parent, ?MODULE_TYPE_CHOICE))
  end,
  {noreply, State};
handle_event(#wx{id=?FILENAME_BOX, event=#wxCommand{type=command_text_updated}},
             State=#state{win=Parent}) ->
  check_if_finished(Parent),
  {noreply, State}.


%% =====================================================================
%% Internal functions
%% =====================================================================

%% =====================================================================
%% @doc Create the first page of the New File dialog.

dialog1(Parent, Projects, ActiveProject) ->
  Dialog1 = wxPanel:new(Parent),
  DialogSizer1 = wxBoxSizer:new(?wxVERTICAL),
  wxPanel:setSizer(Dialog1, DialogSizer1),

  ProjectSizer = wxBoxSizer:new(?wxHORIZONTAL),
  wxSizer:add(DialogSizer1, ProjectSizer, [{proportion, 0}, {flag, ?wxEXPAND}]),
  wxSizer:add(ProjectSizer, wxStaticText:new(Dialog1, ?wxID_ANY, "Project:"),   []),
  wxSizer:addSpacer(ProjectSizer, 20),
  ProjectChoice = wxChoice:new(Dialog1, ?PROJECT_CHOICE),
  wxChoice:append(ProjectChoice, "No Project", wx_misc:getHomeDir()),
  add_project_data(ProjectChoice, Projects),
  wxSizer:add(ProjectSizer, ProjectChoice, [{proportion, 1}, {flag, ?wxEXPAND}]),
  wxChoice:setSelection(ProjectChoice, wxChoice:findString(ProjectChoice, ActiveProject)),
  wxSizer:addSpacer(DialogSizer1, 20),

  FileSizer = wxFlexGridSizer:new(2, 2, 5, 20),
  wxSizer:add(DialogSizer1, FileSizer, [{proportion, 1}, {flag, ?wxEXPAND}]),
  wxSizer:add(FileSizer, wxStaticText:new(Dialog1, ?wxID_ANY, "File Type:"), []),
  wxSizer:add(FileSizer, wxStaticText:new(Dialog1, ?wxID_ANY, "Module Type:"), []),
  FileTypeList = wxListBox:new(Dialog1, ?FILE_TYPE_CHOICE),
  wxSizer:add(FileSizer, FileTypeList, [{proportion, 1}, {flag, ?wxEXPAND}]),
  wxListBox:insertItems(FileTypeList, ?FILE_TYPES, 0),
  wxListBox:connect(FileTypeList, command_listbox_selected, [{skip, true}]),
  wxListBox:setSelection(FileTypeList, 0),

  ModuleTypeList = wxListBox:new(Dialog1, ?MODULE_TYPE_CHOICE),
  wxSizer:add(FileSizer, ModuleTypeList, [{proportion, 1}, {flag, ?wxEXPAND}]),
  wxListBox:insertItems(ModuleTypeList, ?MODULE_TYPES, 0),
  wxListBox:setSelection(ModuleTypeList, 0),
  wxSizer:addSpacer(FileSizer, 10),

  wxFlexGridSizer:addGrowableCol(FileSizer, 0),
  wxFlexGridSizer:addGrowableCol(FileSizer, 1),
  wxFlexGridSizer:addGrowableRow(FileSizer, 1),

  Dialog1.


%% =====================================================================
%% @doc Create the second page of the New File dialog.

dialog2(Parent) ->
  Dialog2 = wxPanel:new(Parent),
  DialogSizer2 = wxFlexGridSizer:new(4, 3, 20, 20),
  wxPanel:setSizer(Dialog2, DialogSizer2),

  wxSizer:add(DialogSizer2, wxStaticText:new(Dialog2, ?wxID_ANY, "File Name:"),   []),
  wxSizer:add(DialogSizer2, wxTextCtrl:new(Dialog2, ?FILENAME_BOX), [{proportion, 1}, {flag, ?wxEXPAND}]),
  wxSizer:add(DialogSizer2, 0, 0, []),

  wxSizer:add(DialogSizer2, wxStaticText:new(Dialog2, ?wxID_ANY, "Project Name:"), []),
  wxSizer:add(DialogSizer2, wxStaticText:new(Dialog2, ?PROJECT_TEXT, "Project Name goes here..."), [{proportion, 1}, {flag, ?wxEXPAND}]),
  wxSizer:add(DialogSizer2, 0, 0, []),

  wxSizer:add(DialogSizer2, wxStaticText:new(Dialog2, ?wxID_ANY, "Folder:"), []),
  wxSizer:add(DialogSizer2, wxTextCtrl:new(Dialog2, ?FOLDER_BOX, [{style, ?wxTE_READONLY}]), [{proportion, 1}, {flag, ?wxEXPAND}]),
  wxSizer:add(DialogSizer2, wxButton:new(Dialog2, ?BROWSE_BUTTON, [{label, "Browse"}])),

  wxSizer:add(DialogSizer2, wxStaticText:new(Dialog2, ?wxID_ANY, "Path:"), []),
  wxSizer:add(DialogSizer2, wxStaticText:new(Dialog2, ?PATH_TEXT, "Path goes here..."), [{proportion, 1}, {flag, ?wxEXPAND}]),
  wxSizer:add(DialogSizer2, 0, 0, []),

  wxFlexGridSizer:addGrowableCol(DialogSizer2, 1),
  wxFlexGridSizer:addGrowableRow(DialogSizer2, 3),

  wxPanel:connect(Dialog2, command_button_clicked, []),
  wxPanel:connect(wxWindow:findWindow(Parent, ?FILENAME_BOX), command_text_updated, []),

  Dialog2.


%% =====================================================================
%% @doc Create the browse dialog for browsing a project directory.

browse_dialog(Parent, Root) ->
  Dialog = wxDialog:new(Parent, ?wxID_ANY, "Choose Directory", [{size,{400, 300}},
                                                                {style, ?wxDEFAULT_DIALOG_STYLE bor
                                                                        ?wxRESIZE_BORDER bor
                                                                        ?wxDIALOG_EX_METAL}]),
  LRSizer = wxBoxSizer:new(?wxHORIZONTAL),
  wxSizer:addSpacer(LRSizer, 20),

  wxDialog:setSizer(Dialog, LRSizer),
  MainSizer = wxBoxSizer:new(?wxVERTICAL),

  wxSizer:add(LRSizer, MainSizer, [{proportion, 1}, {flag, ?wxEXPAND}]),
  wxSizer:addSpacer(MainSizer, 20),
  wxSizer:addSpacer(LRSizer, 20),

  TreeSizer = wxBoxSizer:new(?wxVERTICAL),
  Tree = create_tree(Dialog, Root),
  wxSizer:add(TreeSizer, Tree, [{proportion, 1}, {flag, ?wxEXPAND}]),
  wxSizer:add(MainSizer, TreeSizer, [{proportion, 1}, {flag, ?wxEXPAND}]),

  ButtonPanel = wxPanel:new(Dialog),
  ButtonSizer = wxBoxSizer:new(?wxHORIZONTAL),

  wxPanel:setSizer(ButtonPanel, ButtonSizer),
  wxSizer:add(ButtonSizer, wxButton:new(ButtonPanel, ?wxID_OK,     [{label, "OK"}]), [{border, 2}, {flag, ?wxALL}]),
  wxSizer:add(ButtonSizer, wxButton:new(ButtonPanel, ?wxID_CANCEL, [{label, "Cancel"}]), [{border, 2}, {flag, ?wxALL}]),
  wxSizer:addSpacer(MainSizer, 20),
  wxSizer:add(MainSizer, ButtonPanel, [{proportion, 1}, {flag, ?wxALIGN_RIGHT}]),

  ButtonHandler =
  fun(#wx{id=?wxID_CANCEL},O) ->
    wxEvent:skip(O);
  (#wx{id=?wxID_OK},O) ->
    wxEvent:skip(O),
    Selection = wxTreeCtrl:getSelection(Tree),
    %%io:format(wxTreeCtrl:getItemData(Tree, Selection) ++ "~n"),
    set_path_text(Parent, wxTreeCtrl:getItemData(Tree, Selection)),
    set_folder_text(Parent, "/" ++ wxTreeCtrl:getItemText(Tree, Selection))
  end,

  wxPanel:connect(ButtonPanel, command_button_clicked, [{callback, ButtonHandler}]),
  wxDialog:showModal(Dialog).


%% =====================================================================
%% @doc Set the project's name in the project name field in dialog 2.

set_project_text(Parent, ProjectName) ->
  ProjectText = wx:typeCast(wxWindow:findWindow(Parent, ?PROJECT_TEXT), wxStaticText),
  wxTextCtrl:setLabel(ProjectText, ProjectName).


%% =====================================================================
%% @doc Set the default folder depending on what file type is selected.

set_default_folder_text(Parent) ->
  Text = get_default_folder_text(Parent),
  FolderText = wx:typeCast(wxWindow:findWindow(Parent, ?FOLDER_BOX), wxTextCtrl),
  wxTextCtrl:setValue(FolderText, Text).


%% =====================================================================
%% @doc Set the folder text.

set_folder_text(Parent, Path) ->
  FolderText = wx:typeCast(wxWindow:findWindow(Parent, ?FOLDER_BOX), wxTextCtrl),
  wxTextCtrl:clear(FolderText),
  wxTextCtrl:writeText(FolderText, Path).


%% =====================================================================
%% @doc Set the project's default path in the project path field in dialog 2.

set_default_path_text(Parent, Project) ->
  PathTextBox = wx:typeCast(wxWindow:findWindow(Parent, ?PATH_TEXT), wxStaticText),
  wxStaticText:setLabel(PathTextBox, get_default_path_text(Parent, Project)).


%% =====================================================================
%% @doc Set the path text to a given path.

set_path_text(Parent, Path) ->
  PathTextBox = wx:typeCast(wxWindow:findWindow(Parent, ?PATH_TEXT), wxStaticText),
  wxStaticText:setLabel(PathTextBox, Path).


%% =====================================================================
%% @doc Get the project choice from the projects choice box.

get_project_choice(Parent) ->
  ProjectChoice = wx:typeCast(wxWindow:findWindow(Parent, ?PROJECT_CHOICE), wxChoice),
  Name = wxChoice:getString(ProjectChoice, wxChoice:getSelection(ProjectChoice)),
  Path = wxChoice:getClientData(ProjectChoice, wxChoice:getSelection(ProjectChoice)),
  {Name, Path}.


%% =====================================================================
%% @doc Get the default folder depending on what file type is selected.

get_default_folder_text(Parent) ->
  Project = wx:typeCast(wxWindow:findWindow(Parent, ?PROJECT_CHOICE), wxChoice),
  case wxChoice:getSelection(Project) of
    0 -> %% No Project
      %"/" ++ filename:basename(wxChoice:getClientData(Project, 0));
      "/";
    _ ->
      FileType = wx:typeCast(wxWindow:findWindow(Parent, ?FILE_TYPE_CHOICE), wxListBox),
      case wxListBox:getSelection(FileType) of
        ?FILE_TYPE_ERLANG ->
          ModuleType = wx:typeCast(wxWindow:findWindow(Parent, ?MODULE_TYPE_CHOICE), wxListBox),
          case wxListBox:getSelection(ModuleType) of
            1 ->
              "/include";
            _ ->
              "/src"
          end;
        ?FILE_TYPE_TEXT ->
          "/"
      end
  end.


%% =====================================================================
%% @doc Get the default path based on which project is selected.

get_default_path_text(Parent, Project) ->
  case Project of
    "No Project" ->
      wx_misc:getHomeDir();
    _ ->
      FolderText = wx:typeCast(wxWindow:findWindow(Parent, ?FOLDER_BOX), wxTextCtrl),
      Folder = wxTextCtrl:getLineText(FolderText, 0),
      Project ++ Folder
  end.


%% =====================================================================
%% @doc Set the project's default path in the project path field in dialog 2.

get_path_text(Parent) ->
  PathTextBox = wx:typeCast(wxWindow:findWindow(Parent, ?PATH_TEXT), wxStaticText),
  wxStaticText:getLabel(PathTextBox).


%% =====================================================================
%% @doc Get the specified filename from the file name TextCtrl.

get_filename(Parent) ->
  FilenameBox = wx:typeCast(wxWindow:findWindow(Parent, ?FILENAME_BOX), wxTextCtrl),
  wxTextCtrl:getLineText(FilenameBox, 0).


%% =====================================================================
%% @doc Get the file extension that should be used when creating new file.

get_file_extension(Parent) ->
  FileType = wx:typeCast(wxWindow:findWindow(Parent, ?FILE_TYPE_CHOICE), wxListBox),
  case wxListBox:getSelection(FileType) of
    ?FILE_TYPE_ERLANG ->
      ModuleType = wx:typeCast(wxWindow:findWindow(Parent, ?MODULE_TYPE_CHOICE), wxListBox),
      case wxListBox:getSelection(ModuleType) of
        1 -> %% Erlang header file
          ".hrl";
        _ ->
          ".erl"
      end;
    ?FILE_TYPE_TEXT ->
      ".txt"
  end.


%% =====================================================================
%% @doc Check if the dialog is in a finished state.

check_if_finished(Parent) ->
  Filename = get_filename(Parent),
  case length(Filename) of
    0 ->
      wxListBox:disable(wxWindow:findWindow(Parent, ?FINISH_BUTTON));
    _ ->
      wxListBox:enable(wxWindow:findWindow(Parent, ?FINISH_BUTTON))
  end.


%% =====================================================================
%% @doc Swap Dialog1 with Dialog2.

swap(Sizer, Dialog1, Dialog2) ->
  wxSizer:detach(Sizer, 0),
  wxPanel:hide(Dialog1),
  wxSizer:add(Sizer, Dialog2, [{proportion, 1}, {flag, ?wxEXPAND}]),
  wxSizer:show(Sizer, Dialog2),
  wxSizer:layout(Sizer).


%% =====================================================================
%% @doc Add project labels and data to wxChoice.

add_project_data(_, []) ->
  ok;
add_project_data(ProjectChoice, [ProjectId|Projects]) ->
  wxChoice:append(ProjectChoice, project_manager:get_name(ProjectId), project_manager:get_root(ProjectId)),
  add_project_data(ProjectChoice, Projects).


%% =====================================================================
%% Tree creation functions
%% =====================================================================

%% =====================================================================
%% @doc Create the directory tree structure for the Browse Dialog.

create_tree(Parent, Root) ->
  Tree = wxTreeCtrl:new(Parent, [{style, ?wxTR_HAS_BUTTONS bor
                                         ?wxTR_FULL_ROW_HIGHLIGHT}]),
  wxTreeCtrl:setIndent(Tree, 10),
	ImgList = wxImageList:new(16,16),
	wxImageList:add(ImgList, wxArtProvider:getBitmap("wxART_FOLDER", [{client,"wxART_MENU"}])),
	wxTreeCtrl:assignImageList(Tree, ImgList),
  RootItem = wxTreeCtrl:addRoot(Tree, filename:basename(Root), [{data, Root}]),
  build_tree(Tree, RootItem),
  wxTreeCtrl:expand(Tree, RootItem),
  Tree.

%% =====================================================================
%% @doc Build the tree structure for a given tree item.

build_tree(Tree, Item) ->
  Root = wxTreeCtrl:getItemData(Tree, Item),
  Files = filelib:wildcard(Root ++ "/*"),
  add_files(Tree, Item, Files).

%% =====================================================================
%% @doc Add the files to the given tree item.

add_files(_, _, []) ->
	ok;
add_files(Tree, Root, [File|Files]) ->
	FileName = filename:basename(File),
	IsDir = filelib:is_dir(File),
	case IsDir of
		true ->
			Child = wxTreeCtrl:appendItem(Tree, Root, FileName, [{data, File}]),
			wxTreeCtrl:setItemImage(Tree, Child, 0),
			build_tree(Tree, Child);
		_ ->
			ok
	end,
	add_files(Tree, Root, Files).
