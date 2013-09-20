%% The main GUI for the IDE
%% ide.erl

-module(ide).

-include_lib("wx/include/wx.hrl").
-include("ide.hrl").

-behaviour(wx_object).
-export([start/0, init/1, terminate/2,  code_change/3,
         handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).

-export([toggle_pane/1]).

%% The record containing the State.
-record(state, {win,
                % env,                                           %% The wx environment
                workspace :: wxAuiNotebook:wxAuiNotebook(),    %% Notebook
                utilities,                                     %% The utilities pane
                left_pane,                                     %% The test pane
                workspace_manager,                             %% Tabbed UI manager for editors
                sash_v :: wxSpliiterWindow:wxSplitterWindow(), %% The vertical splitter
                sash_h :: wxSpliiterWindow:wxSplitterWindow(), %% The horizontal splitter
                sash_v_pos :: integer(),
                sash_h_pos :: integer(),
                status_bar :: wxPanel:wxPanel()
                }).

-define(DEFAULT_FRAME_WIDTH,  1300).
-define(DEFAULT_FRAME_HEIGHT, 731).
-define(DEFAULT_UTIL_HEIGHT,  200).
-define(DEFAULT_TEST_WIDTH,   200).

-define(SASH_VERTICAL, 1).
-define(SASH_HORIZONTAL, 2).
-define(SASH_VERT_DEFAULT_POS, 200).
-define(SASH_HOR_DEFAULT_POS, -250).

-define(ID_DIALOG_TEXT, 9001).


%% =====================================================================
%% @doc Start the erlang IDE

start() ->
  start([]).

start(Args) ->
	wx_object:start({local, ?MODULE}, ?MODULE, Args, [{debug, [log]}]).
	%% Trap the error {error,{already_started,Pid()}} to prevent the app from
	%% being opened twice.


%% =====================================================================
%% @doc Initialise the IDE

init(Options) ->
	wx:new(Options),
	process_flag(trap_exit, true),

	%% Load user prefs - should be started by OTP Application and not here
	user_prefs:new([{wxe_server, wx:get_env()}]),

	Frame = wxFrame:new(wx:null(), ?wxID_ANY, "Erlang IDE", [{size,{?DEFAULT_FRAME_WIDTH,?DEFAULT_FRAME_HEIGHT}}]),
	wxFrame:connect(Frame, close_window),
	wxFrame:setMinSize(Frame, {300,200}),

	FrameSizer = wxBoxSizer:new(?wxVERTICAL),
	wxWindow:setSizer(Frame, FrameSizer),

	SplitterTopBottom = wxSplitterWindow:new(Frame, [{id, ?SASH_HORIZONTAL},
		{style, ?wxSP_3DSASH bor ?wxSP_LIVE_UPDATE}]),
	SplitterLeftRight = wxSplitterWindow:new(SplitterTopBottom, [{id, ?SASH_VERTICAL},
		{style, ?wxSP_3DSASH bor ?wxSP_LIVE_UPDATE}]),

	wxSplitterWindow:setSashGravity(SplitterTopBottom, 0.5),
	wxSplitterWindow:setSashGravity(SplitterLeftRight, 0.60),

	wxSizer:add(FrameSizer, SplitterTopBottom, [{flag, ?wxEXPAND}, {proportion, 1}]),

	%% Status bar %%
	StatusBar = ide_status_bar:new([{parent, Frame}]),

	%% Menubar %%
  {Menu, MenuTab} = ide_menu:create([{parent, Frame}]),

	wxSizer:add(FrameSizer, StatusBar, [{flag, ?wxEXPAND},
                                        {proportion, 0}]),

	%% The workspace/text editors %%
	% Manager = wxAuiManager:new([{managed_wnd, Frame}]),
	% EditorWindowPaneInfo = wxAuiPaneInfo:centrePane(wxAuiPaneInfo:new()),
	% {Workspace, TabId} = create_editor(SplitterLeftRight, Manager, EditorWindowPaneInfo, StatusBar, ?DEFAULT_TAB_LABEL),
	Workspace = create_workspace(SplitterLeftRight, StatusBar),

	%% The left window
	LeftWindow = create_left_window(SplitterLeftRight),

	%% The bottom pane/utility window
	Utilities = create_utils(SplitterTopBottom),

	wxSplitterWindow:splitVertically(SplitterLeftRight, LeftWindow, Workspace,
	                  [{sashPosition, ?SASH_VERT_DEFAULT_POS}]),

	wxSplitterWindow:splitHorizontally(SplitterTopBottom, SplitterLeftRight, Utilities,
                  [{sashPosition, ?SASH_HOR_DEFAULT_POS}]),

	wxSizer:layout(FrameSizer),
	wxFrame:center(Frame),
	wxFrame:show(Frame),

	wxSplitterWindow:setSashGravity(SplitterTopBottom, 1.0), % Only the top window grows on resize
	wxSplitterWindow:setSashGravity(SplitterLeftRight, 0.0), % Only the right window grows

  wxSplitterWindow:connect(Frame, command_splitter_sash_pos_changed,  [{userData, SplitterLeftRight}]),
  wxSplitterWindow:connect(Frame, command_splitter_sash_pos_changing, [{userData, SplitterLeftRight}]),
  wxSplitterWindow:connect(Frame, command_splitter_doubleclicked),

	%% Testing accelerator table
	% AccelTab = wxAcceleratorTable:new(1,
	% [wxAcceleratorEntry:new([{flags, ?wxACCEL_NORMAL}, {keyCode, ?WXK_SPACE}, {cmd, ?MENU_ID_FONT}])]),
	% wxFrame:setAcceleratorTable(Frame, AccelTab),

	toggle_menu_group(Menu, 1, MenuTab, {enable, false}),

  State = #state{win=Frame},
  {Frame, State#state{
            left_pane=LeftWindow,
            utilities=Utilities,
            status_bar=StatusBar,
            sash_v_pos=?SASH_VERT_DEFAULT_POS,
            sash_h_pos=?SASH_HOR_DEFAULT_POS,
            sash_v=SplitterLeftRight,
            sash_h=SplitterTopBottom,
						workspace=Workspace
            }}.

%% =====================================================================
%% OTP callbacks
%%
%% =====================================================================

%% Deal with trapped exit signals
handle_info({'EXIT',_, wx_deleted}, State) ->
  io:format("Got Info 1~n"),
  {noreply,State};
handle_info({'EXIT',_, shutdown}, State) ->
  io:format("Got Info 2~n"),
  {noreply,State};
handle_info({'EXIT',A, normal}, State) ->
  io:format("Got Info 3~n~p~n", [A]),
  {noreply,State};
handle_info(Msg, State) ->
  io:format("Got Info ~p~n",[Msg]),
  {noreply,State}.

%% @doc Get the frames sash positions
handle_call(splitter, _From, State) ->
	{reply, {State#state.sash_v,
			 State#state.sash_h,
             State#state.sash_v_pos,
             State#state.sash_h_pos,
             State#state.workspace,
             State#state.left_pane,
             State#state.utilities}, State};
handle_call(frame, _From, State) ->
	{reply, State#state.win, State};
handle_call(Msg, _From, State) ->
  demo:format(State#state{}, "Got Call ~p\n", [Msg]),
  {reply,{error, nyi}, State}.

handle_cast(Msg, State) ->
  io:format("Got cast ~p~n",[Msg]),
  {noreply,State}.

%% =====================================================================
%% Event handlers
%%
%% =====================================================================

%% Window close event
handle_event(#wx{event=#wxClose{}}, State) ->
  io:format("~p Closing window ~n",[self()]),
  {stop, normal, State};
	

%% =====================================================================
%% Sash drag handlers
%%
%% =====================================================================

%% Vertical sash dragged
handle_event(_W=#wx{id=?SASH_VERTICAL, event=#wxSplitter{type=command_splitter_sash_pos_changed}=_E}, State) ->
  Pos = wxSplitterWindow:getSashPosition(State#state.sash_v),
  %% Don't save the pos if the sash is dragged to zero, as the sash
  %% will revert to the middle when shown again (on mac definitely,
	%% probably on all platforms, THIS SHOULD BE CHECKED AGAIN on R16B01 and wxWidgets 2.9.4)
  if
    Pos =:= 0 ->
    	NewPos = State#state.sash_v_pos;
    true ->
    	NewPos = Pos
    end,
  {noreply, State#state{sash_v_pos=NewPos}};

%% Horizontal sash dragged
handle_event(_W=#wx{id=?SASH_HORIZONTAL, event=#wxSplitter{type=command_splitter_sash_pos_changed}=_E}, State) ->
  Pos = wxSplitterWindow:getSashPosition(State#state.sash_h),
  if
    Pos =:= 0 ->
    	NewPos = State#state.sash_h_pos;
    true ->
    	NewPos = Pos
  end,
  wxWindow:refresh(State#state.sash_v),
  wxWindow:update(State#state.sash_v),
  {noreply, State#state{sash_h_pos=NewPos}};

handle_event(#wx{event = #wxSplitter{type = command_splitter_sash_pos_changing} = _E}, State) ->
  io:format("Sash position changing ~n"),
  {noreply, State};

handle_event(#wx{event = #wxSplitter{type = command_splitter_doubleclicked} = _E}, State) ->
  io:format("Sash double clicked ~n"),
  {noreply, State};


%% =====================================================================
%% Menu handlers
%%
%% =====================================================================

%% See ticket #5
%% Although a temporary fix has been implemented for ticket #5, using this handler
%% would be the preferred option
handle_event(#wx{id=Id, event=#wxMenu{type=menu_close}},
           State=#state{status_bar=Sb}) ->
  ide_status_bar:set_text(Sb, {field, help}, ?STATUS_BAR_HELP_DEFAULT),
{noreply, State};

%% Handle menu highlight events
handle_event(#wx{id=Id, userData={ets_table, TabId}, event=#wxMenu{type=menu_highlight}},
             State=#state{status_bar=Sb}) ->
	% ide_status_bar:set_text(Sb, {field, help}, "testing"),
  {noreply, State};

%% First handle the sub-menus
handle_event(E=#wx{id=Id, userData={theme_menu,Menu}, event=#wxCommand{type=command_menu_selected}},
             State=#state{status_bar=Sb}) ->
	Env = wx:get_env(),
	spawn(fun() -> wx:set_env(Env),
		doc_manager:set_theme(Menu)
	end),
	{noreply, State};

handle_event(E=#wx{id=Id, userData=Menu, event=#wxCommand{type=command_menu_selected}},
             State=#state{status_bar=Sb}) when Id >= ?MENU_ID_TAB_WIDTH_LOWEST,
						 Id =< ?MENU_ID_TAB_WIDTH_HIGHEST  ->
	Env = wx:get_env(),
	spawn(fun() -> wx:set_env(Env),
		[editor:set_tab_width(Ed, list_to_integer(wxMenu:getLabel(Menu, Id))) || {_,Ed} <- doc_manager:get_all_editors()]
	end),
	user_prefs:set_user_pref(tab_width, wxMenu:getLabel(Menu, Id)),
	{noreply, State};

%% The menu items from the ETS table
handle_event(E=#wx{id=Id, userData={ets_table, TabId}, event=#wxCommand{type=command_menu_selected}},
             State=#state{status_bar=Sb, win=Frame}) ->
	Result = case ets:lookup(TabId, Id) of
		[{MenuItemID, {Mod, Func, Args}, Options}] ->
			case proplists:get_value(update_label, Options) of
				undefined -> ok;
				Pos ->
					ide_menu:update_label(MenuItemID, wxMenuBar:getMenu(wxFrame:getMenuBar(Frame), Pos))
			end,
			case proplists:get_value(send_event, Options) of
				undefined -> {ok, {Mod,Func,Args}};
				true -> {ok, {Mod,Func,[E]}}
			end;
		[{_,{Mod,Func,Args}}] ->
			{ok, {Mod,Func,Args}};
		_ -> nomatch
	end,
	Env = wx:get_env(),
	case Result of
		{ok,{M,F,A}} -> 
			erlang:apply(M,F,A);
		nomatch -> 
			io:format("Not yet implemented~n")
	end,
  {noreply, State};

%% =====================================================================
%% Other handlers
%%
%% =====================================================================

%% Event catchall for testing
handle_event(Ev, State) ->
  io:format("IDE event catchall: ~p\n", [Ev]),
  {noreply, State}.

code_change(_, _, State) ->
  {stop, not_yet_implemented, State}.

terminate(_Reason, #state{win=Frame, workspace_manager=Manager}) ->
  %% Unregister any pids/ports that dont close automatically
  %% This is a bit nasty - an OTP Application which allows
  %% components that can be started and stopped as a unit might
  %% be a better choice.
  port:close_port(),
  erlang:unregister(port),
	user_prefs:stop(),
  %% Below is the necessary cleanup
  io:format("TERMINATE IDE~n"),
  wxFrame:destroy(Frame),
  wx:destroy().


%% =====================================================================
%% @doc Create the utilities panel
%%
%% @private

-spec create_utils(Parent) -> Result when
	Parent :: wxWindow:wxWindow(),
	Result :: wxPanel:wxPanel().

create_utils(Parent) ->
	UtilPanel = wxPanel:new(Parent, []),

	Utils = wxNotebook:new(UtilPanel, 8989, [{style, ?wxBORDER_NONE}]),

	UtilSizer = wxBoxSizer:new(?wxVERTICAL),
	wxPanel:setSizer(UtilPanel, UtilSizer),

	Console = ide_shell:new([{parent, Utils}]),
	%% Start the port that communicates with the external ERTs
	port:start(),
	wxNotebook:addPage(Utils, Console, "Console", []),

	Pman = wxPanel:new(Utils, []),
	wxNotebook:addPage(Utils, Pman, "Observer", []),

	Dialyser = wxPanel:new(Utils, []),
	wxNotebook:addPage(Utils, Dialyser, "Dialyser", []),

	Debugger = wxPanel:new(Utils, []),
	wxNotebook:addPage(Utils, Debugger, "Debugger", []),

	wxSizer:addSpacer(UtilSizer, 1),
	wxSizer:add(UtilSizer, Utils, [{proportion, 1}, {flag, ?wxEXPAND}]),

	UtilPanel.


%% =====================================================================
%% @doc

create_left_window(Parent) ->
	ImgList = wxImageList:new(24,24),
	wxImageList:add(ImgList, wxBitmap:new(wxImage:new("../icons/document-new.png"))),
	wxImageList:add(ImgList, wxBitmap:new(wxImage:new("../icons/document-open.png"))),
	wxImageList:add(ImgList, wxBitmap:new(wxImage:new("../icons/document-new.png"))),

	Toolbook = wxToolbook:new(Parent, ?wxID_ANY, [{style, ?wxBK_BUTTONBAR}]),
	wxToolbook:assignImageList(Toolbook, ImgList),

	ProjectsPanel = wxPanel:new(Toolbook),
	ProjectsSizer = wxBoxSizer:new(?wxVERTICAL),
	ProjectTree = ide_projects_tree:new(ProjectsPanel),
	wxSizer:add(ProjectsSizer, ProjectTree, [{flag, ?wxEXPAND}, {proportion, 1}]),
	wxPanel:setSizer(ProjectsPanel, ProjectsSizer),
	wxToolbook:addPage(Toolbook, ProjectsPanel, "Projects", [{imageId, 0}]),

	TestPanel = wxPanel:new(Toolbook),
	wxToolbook:addPage(Toolbook, TestPanel, "Tests", [{imageId, 1}]),

	FunctionsPanel = func_list:start([{parent, Toolbook}]),
	wxToolbook:addPage(Toolbook, FunctionsPanel, "Functions", [{imageId, 2}]),

	wxToolbook:setSelection(Toolbook, 0), %% Default to projects
	Toolbook.

%% =====================================================================
%% @doc

create_workspace(Parent, StatusBar) ->
	doc_manager:new([{config, {Parent, StatusBar}}]).


%% =====================================================================
%% @doc Display or hide a given window pane

-spec toggle_pane(PaneType) -> Result when
	PaneType :: 'test' | 'util' | 'editor' | 'maxutil',
	Result :: 'ok'.

toggle_pane(PaneType) ->
    {V,H,Vp,Hp,W,T,U} = wx_object:call(?MODULE, splitter),
	case PaneType of
		test ->
            case wxSplitterWindow:isSplit(V) of
                true ->
                    wxSplitterWindow:unsplit(V,[{toRemove, T}]);
                false ->
                    wxSplitterWindow:splitVertically(V, T, W, [{sashPosition, Vp}])
            end;
		util ->
            case wxSplitterWindow:isSplit(H) of
                true ->
					wxSplitterWindow:unsplit(H,[{toRemove, U}]);
				false ->
					wxSplitterWindow:splitHorizontally(H, V, U, [{sashPosition, Hp}])
			end;
		editor ->
			case wxSplitterWindow:isSplit(H) of
				true ->
					wxSplitterWindow:unsplit(H,[{toRemove, U}]),
					wxSplitterWindow:unsplit(V,[{toRemove, T}]);
				false ->
					case wxSplitterWindow:isShown(U) of
						true ->
							wxSplitterWindow:splitHorizontally(H, V, U, [{sashPosition, Hp}]),
							wxSplitterWindow:unsplit(H,[{toRemove, U}]),
							wxSplitterWindow:unsplit(V,[{toRemove, T}]);
						false ->
							wxSplitterWindow:splitHorizontally(H, V, U, [{sashPosition, Hp}]),
							wxSplitterWindow:splitVertically(V, T, W, [{sashPosition, Vp}])
					end
			end;
		maxutil ->
			case wxSplitterWindow:isSplit(H) of
				true ->
					wxSplitterWindow:unsplit(H,[{toRemove, V}]);
				false ->
				    case wxSplitterWindow:isShown(U) of
						true ->
							wxSplitterWindow:splitHorizontally(H, V, U, [{sashPosition, Hp}]),
							wxSplitterWindow:splitVertically(V, T, W, [{sashPosition, Vp}]);
						false ->
							wxSplitterWindow:splitHorizontally(H, V, U, [{sashPosition, Hp}]),
							wxSplitterWindow:unsplit(H,[{toRemove, V}])
					end
			end
	end,
	ok.


%% =====================================================================
%% @doc Enable/disable a menu group
%% @private

toggle_menu_group(Mb, Mask, TabId, {enable, Bool}=Toggle) ->
	ets:foldl(
	fun({Id,_}, DontCare) ->
		DontCare;
	({Id,_,Options}, DontCare) ->
		case proplists:get_value(group, Options) of
			undefined -> ok;
			Groups ->
				toggle_menu_item(Mb, Mask, Id, Groups, Toggle)
		end,
		DontCare
	end, notused, TabId).


%% =====================================================================
%% @doc Enable/disable a menu item
%% @private

-spec toggle_menu_item(Mb, Mask, Id, Groups, Enable) -> 'ok' when
	Mb :: wxMenuBar:wxMenuBar(),
	Mask :: integer(), % Defines which groups to affect
	Id :: integer(),	% The menu item id
	Groups :: integer(), % The groups associated to the menu item with Id
	Enable :: {'enable', boolean()}.

toggle_menu_item(Mb, Mask, Id, Groups, Enable) ->
	case (Mask band Groups) of
		0 -> ok;
		_ ->
			wxMenuItem:enable(wxMenuBar:findItem(Mb, Id), [{enable, false}])
	end.

