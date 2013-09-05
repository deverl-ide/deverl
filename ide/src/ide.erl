%% The main GUI for the IDE
%% ide.erl

-module(ide).

-include_lib("wx/include/wx.hrl").
-include("ide.hrl").

-behaviour(wx_object).
%% wx_objects callbacks
-export([start/0, init/1, terminate/2,  code_change/3,
         handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).

%% API         
% -export([
% 		add_editor/0, 
% 		add_editor/1, 
% 		add_editor_with_contents/3,
% 		close_selected_editor/0, 
% 		close_all_editors/0,
% 		toggle_pane/1, 
% 		get_selected_editor/0, 
% 		get_all_editors/0, 
% 		update_styles/1, 
% 		save_current_file/0,
% 		save_new/0, 
% 		save_all/0,
% 		open_file/1,
% 		open_dialog/1,
% 		find_replace/1,
% 		get_current_theme_name/0,
% 		set_theme/1,
% 		set_line_wrap/1,
% 		set_line_margin_visible/1,
% 		set_indent_tabs/1,
% 		set_indent_guides/1,
% 		indent_line_right/0,
% 		indent_line_left/0,
% 		go_to_line/1,
% 		comment/0,
% 		zoom_in/0,
% 		zoom_out/0,
% 		transform_selection/1,
% 		load_file/0,
% 		]).


%% The record containing the State.
-record(state, {win,  
                env,                                           %% The wx environment
                workspace :: wxAuiNotebook:wxAuiNotebook(),    %% Notebook
                utilities,                                     %% The utilities pane
                left_pane,                                     %% The test pane
                workspace_manager,                             %% Tabbed UI manager for editors
                sash_v :: wxSpliiterWindow:wxSplitterWindow(), %% The vertical splitter
                sash_h :: wxSpliiterWindow:wxSplitterWindow(), %% The horizontal splitter
                sash_v_pos :: integer(),
                sash_h_pos :: integer(),
                status_bar :: wxPanel:wxPanel(),
                }).

-define(DEFAULT_FRAME_WIDTH,  1300).
-define(DEFAULT_FRAME_HEIGHT, 731).
-define(DEFAULT_UTIL_HEIGHT,  200).
-define(DEFAULT_TEST_WIDTH,   200).

-define(SASH_VERTICAL, 1).
-define(SASH_HORIZONTAL, 2).
-define(SASH_VERT_DEFAULT_POS, 200).
-define(SASH_HOR_DEFAULT_POS, -250).

-define(ID_DIALOG, 9000).
-define(ID_DIALOG_TEXT, 9001).

-define(ID_WORKSPACE, 3211).


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

	% Following two lines, see platforms.txt <1> 
	% After upgrading to 2.9.4 these have no effect on mac
	% wxSplitterWindow:setSashSize(SplitterTopBottom, 10),
	% wxSplitterWindow:setSashSize(SplitterLeftRight, 10),
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
	Manager = wxAuiManager:new([{managed_wnd, Frame}]),
	EditorWindowPaneInfo = wxAuiPaneInfo:centrePane(wxAuiPaneInfo:new()), 
	% {Workspace, TabId} = create_editor(SplitterLeftRight, Manager, EditorWindowPaneInfo, StatusBar, ?DEFAULT_TAB_LABEL),
	Workspace = create_workspace(SplitterLeftRight, Manager, EditorWindowPaneInfo, StatusBar),

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
  {Frame, State#state{workspace=Workspace, 
            workspace_manager=Manager,
            left_pane=LeftWindow,
            utilities=Utilities,
            status_bar=StatusBar,
            sash_v_pos=?SASH_VERT_DEFAULT_POS,
            sash_h_pos=?SASH_HOR_DEFAULT_POS,
            sash_v=SplitterLeftRight,
            sash_h=SplitterTopBottom,
            editor_pids=TabId
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
%% @doc Return the workspace
handle_call(workspace, _From, State) ->
  {reply, {State#state.workspace, 
           State#state.status_bar,
           State#state.editor_pids}, State};
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
%% AUI handlers
%% 
%% =====================================================================

%% Not connected currently
handle_event(#wx{event = #wxAuiManager{type = aui_render} = _E}, State) ->
  io:format("render:~n"),   
  {noreply, State};
    
handle_event(#wx{obj = _Workspace, event = #wxAuiNotebook{type = command_auinotebook_page_changed, 
			selection = Index}}, State) ->
  %% Make sure editor knows (needs to update sb)
  editor:selected(get_editor_pid(Index, State#state.workspace, State#state.editor_pids), State#state.status_bar),
  {noreply, State}; 
    
handle_event(#wx{event=#wxAuiNotebook{type=command_auinotebook_bg_dclick}}, State) ->
  add_editor(State#state.workspace, State#state.status_bar, 
             State#state.editor_pids),
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
		set_theme(Menu)
	end),
	{noreply, State};
	
handle_event(E=#wx{id=Id, userData=Menu, event=#wxCommand{type=command_menu_selected}},
             State=#state{status_bar=Sb}) when Id >= ?MENU_ID_TAB_WIDTH_LOWEST,
						 Id =< ?MENU_ID_TAB_WIDTH_HIGHEST  -> 
	Env = wx:get_env(),
	spawn(fun() -> wx:set_env(Env),
		[editor:set_tab_width(Ed, list_to_integer(wxMenu:getLabel(Menu, Id))) || {_,Ed} <- get_all_editors()]
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
		{ok,{M,F,A}} -> spawn(fun() -> wx:set_env(Env), erlang:apply(M,F,A) end);
		nomatch -> io:format("Not yet implemented~n")
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
  wxAuiManager:unInit(Manager),
  wxAuiManager:destroy(Manager),
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
	ide_side_bar:new(Parent).


create_workspace(Parent, Manager, PaneInfo, StatusBar) ->
	doc_manager:build_workspace(Parent, Manager, PaneInfo, StatusBar).
	
	
% %% =====================================================================
% %% @doc Create the workspace with the initial editor
% %% @private  
% 
% create_editor(Parent, Manager, Pane, Sb, Filename) ->
% 	Style = (0
% 			bor ?wxAUI_NB_TOP
% 			bor ?wxAUI_NB_WINDOWLIST_BUTTON
% 			bor ?wxAUI_NB_TAB_MOVE
% 			bor ?wxAUI_NB_SCROLL_BUTTONS
% 			bor ?wxAUI_NB_CLOSE_ON_ALL_TABS
% 			),
%     
% 	Workspace = wxAuiNotebook:new(Parent, [{id, ?ID_WORKSPACE}, {style, Style}]),  
% 	Editor = editor:start([{parent, Workspace}, {status_bar, Sb},
%                            {font, user_prefs:get_user_pref({pref, font})}]), %% Returns an editor instance inside a wxPanel
%   
% 	TabId = ets:new(editors, [public]),
% 	{_,Id,_,Pid} = Editor,
% 	ets:insert(TabId,{Id, Pid}),
% 
% 	wxAuiNotebook:addPage(Workspace, Editor, Filename, []),
%   
% 	wxAuiManager:addPane(Manager, Workspace, Pane),
%   
% 	Close = fun(_,O) ->
% 				wxNotifyEvent:veto(O),
% 				close_selected_editor()
% 			end,
%   
% 	wxAuiNotebook:connect(Workspace, command_auinotebook_bg_dclick, []),
% 	wxAuiNotebook:connect(Workspace, command_auinotebook_page_close, [{callback,Close},{userData,TabId}]),
% 	wxAuiNotebook:connect(Workspace, command_auinotebook_page_changed),   
%     
% 	{Workspace, TabId}.
	
	
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
  
	
% %% =====================================================================
% %% @doc 
% %%
% %% @private
% 
% add_editor(Workspace, Sb, TabId) ->
% 	add_editor(Workspace, ?DEFAULT_TAB_LABEL, Sb, TabId),
% 	Workspace.
%  
% 
% %% =====================================================================
% %% @doc Create a new editor instance in the notebook  
% 
% add_editor() -> 
% 	add_editor(?DEFAULT_TAB_LABEL).
%   
% %% @doc Create a new editor with specified filename
% add_editor(Filename) ->
% 	{Workspace, Sb, TabId} = wx_object:call(?MODULE, workspace), 
% 	add_editor(Workspace, Filename, Sb, TabId),
% 	ok.
%   
% %% @private
% add_editor(Workspace, Filename, Sb, TabId) ->
% 	Editor = editor:start([{parent, Workspace}, {status_bar, Sb}, {font,user_prefs:get_user_pref({pref, font})}]),
% 	wxAuiNotebook:addPage(Workspace, Editor, Filename, [{select, true}]),
% 	{_,Id,_,Pid} = Editor,
% 	ets:insert_new(TabId,{Id, Pid}),
% 	ok.
%   
% %% @doc Create an editor from an existing file
% add_editor_with_contents(Path, Filename, Contents) -> 
% 		{Workspace, Sb, TabId} = wx_object:call(?MODULE, workspace), 
% 		Editor = editor:start([{parent, Workspace}, {status_bar, Sb}, {font,user_prefs:get_user_pref({pref, font})}, 
% 							   {file, {Path, Filename, Contents}}]),
% 		wxAuiNotebook:addPage(Workspace, Editor, Filename, [{select, true}]),
% 		{_,Id,_,Pid} = Editor,
% 		ets:insert_new(TabId,{Id, Pid}),
% 		ok.

	
% %% =====================================================================
% %% @doc Get the Pid of an editor instance at position Index.
% 
% -spec get_editor_pid(Index) -> Result when
% 	Index :: integer(),
% 	Result :: pid().  
% 
% get_editor_pid(Index) ->
% 	{Workspace,_,PidTable} = wx_object:call(?MODULE, workspace), 
% 	get_editor_pid(Index, Workspace, PidTable).
%   
% -spec get_editor_pid(Index, Workspace, PidTable) -> Result when
% 	Index :: integer(),
% 	Workspace :: wxAuiNotebook:wxAuiNotebook(),
% 	PidTable :: term(),
% 	Result :: pid().  
% 
% get_editor_pid(Index, Workspace, PidTable) ->
% 	{_,Key,_,_} = wxAuiNotebook:getPage(Workspace, Index),
% 	[{_,Pid}] = ets:lookup(PidTable, Key),
% 	Pid.
% 
% 	
% %% =====================================================================
% %% @doc Get the Pid of the currently selected editor.
% 	
% -spec get_selected_editor() -> Result when
% 	Result :: {'error', 'no_open_editor'} | 
% 			  {'ok', {integer(), pid()}}.
%             
% get_selected_editor() ->
% 	{Workspace,_,_} = wx_object:call(?MODULE, workspace), 
% 	case wxAuiNotebook:getSelection(Workspace) of %% Get the index of the tab
% 		-1 -> %% no editor instance
% 				{error, no_open_editor};
% 		Index ->
% 				{ok, {Index, get_editor_pid(Index)}}
% 	end.
% 	
% 
% %% =====================================================================
% %% @doc Get all open editor instances.
% %% Returns a list of tuples of the form: {Index, EditorPid}, where
% %% Index starts at 0.
%   
% -spec get_all_editors() -> Result when
% 	Result :: [{integer(), pid()}].
% 	-module(document_io).
% 
% 
% 
% get_all_editors() ->
% 	{Workspace,_,_} = wx_object:call(?MODULE, workspace), 
% 	Count = wxAuiNotebook:getPageCount(Workspace),
% 	get_all_editors(Workspace, Count - 1, []).
% 
% get_all_editors(_, -1, Acc) -> 
% 	Acc;
% 
% get_all_editors(Workspace, Count, Acc) ->
% 	get_all_editors(Workspace, Count -1, [{Count, get_editor_pid(Count)} | Acc]).


% %% =====================================================================
% %% Open/save/close editor functions
% %% 
% %% =====================================================================
% 
% %% =====================================================================
% %% @doc Save the currently selected file to disk
% 
% -spec save_current_file() -> 'ok'. %% Not yet done
% 
% save_current_file() ->
% 	case get_selected_editor() of
% 		{error, no_open_editor} ->
% 			%% Toolbar/menubar buttons should be disabled to prevent this action
% 			io:format("No editor open.~n");
% 		{ok, {Index, Pid}} ->
% 			save_file(Index, Pid)
% 	end.
% 
% 
% %% =====================================================================
% %% @doc Save the contents of the editor Pid located at index Index.
%   
% save_file(Index, Pid) ->  
% 	{Workspace,Sb,_} = wx_object:call(?MODULE, workspace), 
% 	case editor:save_status(Pid) of
% 		{save_status, new_file} ->
% 			save_new(Index, Workspace, Sb, Pid);
% 		{save_status, no_file} ->
% 			%% Display save dialog
% 			save_new(Index, Workspace, Sb, Pid);
% 		{save_status, unmodified} ->
% 			%% Document is unmodified, no need to save
% 			ide_status_bar:set_text_timeout(Sb, {field, help}, "Document already saved.");
% 		{save_status, Path, Fn} ->
% 			%% Document already exists, overwrite
% 			Contents = editor:get_text(Pid),
% 			ide_io:save(Path, Contents),
% 			editor:set_savepoint(Pid, Path, Fn),
% 			ide_status_bar:set_text_timeout(Sb, {field, help}, "Document saved.")
% 	end.
% 
% 
% %% =====================================================================
% %% @doc Save the erlang editor with pid Pid located at index position 
% %% Index to disk. The user will be prompted for a save path.
% %%
% %% @private
% 
% -spec save_new(Index, Workspace, Sb, Pid) -> Result when
% 	Index :: integer(),
% 	Workspace :: wxAuiNotebook:wxAuiNotebook(),
% 	Sb :: ide_status_bar:status_bar(),
% 	Pid :: pid(),
% 	Result :: {save, cancelled} %% User cancelled save
% 			| {save, complete}. %% Save success 
% 
% save_new(Index, Workspace, Sb, Pid) ->
% 	Contents = editor:get_text(Pid),
% 	case ide_io:save_as(Workspace, Contents) of
% 		{cancel} ->
% 			ide_status_bar:set_text_timeout(Sb, {field, help}, "Document not saved."),
% 			{save, cancelled};
% 		{ok, {Path, Filename}}  ->
% 			wxAuiNotebook:setPageText(Workspace, Index, Filename),
% 			editor:set_savepoint(Pid, Path, Filename),
% 			ide_status_bar:set_text_timeout(Sb, {field, help}, "Document saved."),
% 			{save, complete}
% 	end.
%   
% -spec save_new() -> Result when
%   Result :: {save, cancelled}
%           | {save, complete}.
%   
% save_new() ->
% 	case get_selected_editor() of
% 		{error, no_open_editor} ->
% 			%% Toolbar/menubar buttons should be disabled to prevent this action
% 			io:format("No editor open.~n");
% 		{ok, {Index, Pid}} ->
% 			{Workspace,Sb,_,_} = wx_object:call(?MODULE, workspace),
% 			save_new(Index, Workspace, Sb, Pid)
% 	end.
% 
% 
% %% =====================================================================
% %% @doc Save all open editors.
% 
% save_all() ->
% 	Fun = fun({Index, Pid}) ->
% 		      save_file(Index, Pid)
% 		  end,
% 	lists:map(Fun, get_all_editors()).
% 
% 
% %% =====================================================================
% %% @doc
% 
% -spec open_file(Frame) -> 'ok' when
% 	Frame :: wxWindow:wxWindow().
% 
% open_file(Frame) ->
% 	case ide_io:open(Frame) of
% 		{cancel} ->
% 			ok;
% 		{Path, Filename, Contents} ->
% 			add_editor_with_contents(Path, Filename, Contents),
% 			ok
% 	end.
% 
% 
% %% =====================================================================
% %% @doc Close the selected editor
% 	
% close_selected_editor() ->
% 	case get_selected_editor() of
% 		{error, _} ->
% 			{error, no_open_editor};
% 		{ok, {Index, EditorPid}} ->
% 			close_editor(EditorPid, Index)
% 	end.
% 
% 
% %% =====================================================================
% %% @doc Close the selected editor
% 
% close_editor(EditorPid, Index) ->
% 	{Workspace,_Sb,Tab} = wx_object:call(?MODULE, workspace),
% 	case editor:save_status(EditorPid) of
% 		{save_status, new_file} ->
% 			ets:delete(Tab, editor:get_id(EditorPid)),
%       wxAuiNotebook:deletePage(Workspace, Index);
% 		{save_status, unmodified} -> %% Go ahead, close the editor
% 			ets:delete(Tab, editor:get_id(EditorPid)),
%       wxAuiNotebook:deletePage(Workspace, Index);
% 		_ -> 
% 			io:format("Close dialog needs to be displayed.~n")
% 	end.
%   
%   
% %% =====================================================================
% %% @doc Close all editor instances
% 
% close_all_editors() ->
% 	Fun = fun({Index,Pid}) ->
% 		      close_editor(Pid, Index)
% 		  end,
% 	lists:map(Fun, lists:reverse(get_all_editors())),
% 	ok.
% 
% 
% %% =====================================================================
% %% @doc Open a dialog box for various functions
% %% Buttons = [{Label, Id, Function}]
% 
% open_dialog(Parent) ->
% 	Dialog = wxDialog:new(Parent, ?ID_DIALOG, "Title"),
%   
% 	Bs = wxBoxSizer:new(?wxHORIZONTAL),
% 	Ba = wxButton:new(Dialog, 1, [{label, "Nibble"}]),
% 	Bb = wxButton:new(Dialog, 2, [{label, "Nobble"}]),
% 	wxSizer:add(Bs, Ba),
% 	wxSizer:add(Bs, Bb),
%     
% 	Box  = wxBoxSizer:new(?wxVERTICAL),
% 
% 	wxSizer:add(Box, Bs,  [{border, 2}, {flag, ?wxALL bor ?wxEXPAND}]),    
% 	wxWindow:setSizer(Dialog, Box),
% 	wxSizer:fit(Box, Dialog),
% 	wxSizer:setSizeHints(Box,Dialog),
%   
% 	wxDialog:showModal(Dialog).
% 	
% 	



% %% =====================================================================
% %% @doc Change the font style across all open editors
% 
% update_styles(Frame) ->
% 	%% Display the system font picker
% 	FD = wxFontData:new(),
% 	wxFontData:setInitialFont(FD, user_prefs:get_user_pref({pref, font})),
% 	Dialog = wxFontDialog:new(Frame, FD),
% 	case wxDialog:showModal(Dialog) of
% 		?wxID_OK ->
% 			%% Get the user selected font, and update the editors
% 			Font = wxFontData:getChosenFont(wxFontDialog:getFontData(Dialog)),
% 			user_prefs:set_user_pref(font, Font),
% 			Fun = fun({_, Pid}) ->
% 				      editor:update_font(Pid, Font)
% 				  end,
% 			lists:map(Fun, get_all_editors()),
% 			ok;
% 		?wxID_CANCEL ->
% 			ok
% end. 
% 
% 
% %% =====================================================================
% %% @doc Show the find/replace dialog
% %% Might be better in editor.erl
% 
% find_replace(Parent) ->
%   FindData = find_replace_data:new(),
%   
%   %% This data will eventually be loaded from transient/permanent storage PREFS!!
%   find_replace_data:set_options(FindData, ?IGNORE_CASE bor ?WHOLE_WORD bor ?START_WORD),
%   find_replace_data:set_search_location(FindData, ?FIND_LOC_DOC),
%   
% 	case erlang:whereis(find_replace_dialog) of
% 		undefined ->
% 			find_replace_dialog:show(find_replace_dialog:new(Parent, FindData));
% 		Pid ->
% 			wxDialog:raise(find_replace_dialog:get_ref(Pid))
% 	end.
% 
% 		
% set_theme(ThemeMenu) ->
% 	{ok, Ckd} = get_checked_menu_item(wxMenu:getMenuItems(ThemeMenu)),
% 	Fun = fun({_, Pid}) ->
% 		      editor:set_theme(Pid, wxMenuItem:getLabel(Ckd), user_prefs:get_user_pref({pref, font}))
% 		  end,
% 	lists:map(Fun, get_all_editors()),
% 	user_prefs:set_user_pref(theme, wxMenuItem:getLabel(Ckd)).
% 
% get_current_theme_name() ->
% 	Frame = wx_object:call(?MODULE, frame),
% 	Mb = wxFrame:getMenuBar(Frame),
% 	Menu = wxMenuBar:findMenu(Mb, "View"),
% 	Item = wxMenu:findItem(wxMenuBar:getMenu(Mb, Menu), ?MENU_ID_THEME_SELECT),
% 	Itms = wxMenu:getMenuItems(wxMenuItem:getSubMenu(Item)),
% 	{ok, Ckd} = get_checked_menu_item(Itms),
% 	wxMenuItem:getLabel(Ckd).
% 	
% get_checked_menu_item([]) ->
% 	{error, nomatch};
% get_checked_menu_item([H|T]) ->
% 	case wxMenuItem:isChecked(H) of
% 		true ->
% 			{ok, H};
% 		_ ->
% 			get_checked_menu_item(T)
% 	end.
% 	
% set_line_wrap(Menu) ->
% 	Bool = wxMenuItem:isChecked(wxMenu:findItem(Menu, ?MENU_ID_LINE_WRAP)),
% 	Fun = fun({_, Pid}) ->
% 		      editor:set_line_wrap(Pid, Bool)
%   end,
% 	lists:map(Fun, get_all_editors()),
% 	user_prefs:set_user_pref(line_wrap, Bool).
% 
% set_line_margin_visible(Menu) ->
% 	Bool = wxMenuItem:isChecked(wxMenu:findItem(Menu, ?MENU_ID_LN_TOGGLE)),
% 	Fun = fun({_, Pid}) ->
% 		      editor:set_line_margin_visible(Pid, Bool)
%   end,
% 	lists:map(Fun, get_all_editors()),
% 	user_prefs:set_user_pref(show_line_no, Bool).
% 	
% set_indent_tabs(#wx{id=Id, event=#wxCommand{type=command_menu_selected}}) ->
% 	Cmd = case Id of
% 		?MENU_ID_INDENT_SPACES -> false;
% 		?MENU_ID_INDENT_TABS -> true
% 	end,
% 	Fun = fun({_, Pid}) ->
% 		      editor:set_use_tabs(Pid, Cmd)
%   end,
% 	lists:map(Fun, get_all_editors()),
% 	user_prefs:set_user_pref(use_tabs, Cmd).
% 		
% set_indent_guides(Menu) ->
% 	Bool = wxMenuItem:isChecked(wxMenu:findItem(Menu, ?MENU_ID_INDENT_GUIDES)),
% 	Fun = fun({_, Pid}) ->
% 		      editor:set_indent_guides(Pid, Bool)
%   end,
% 	lists:map(Fun, get_all_editors()),
% 	user_prefs:set_user_pref(indent_guides, Bool).
% 	
% indent_line_right() ->
% 	{ok,{_,Pid}} = get_selected_editor(),
% 	editor:indent_line_right(Pid),
% 	ok.
% 	
% indent_line_left() ->
% 	{ok,{_,Pid}} = get_selected_editor(),
% 	editor:indent_line_left(Pid),
% 	ok.
% 	
% comment() ->
% 	{ok,{_,Pid}} = get_selected_editor(),
% 	editor:comment(Pid),
% 	ok.
% 	
% zoom_in() ->
% 	{ok,{_,Pid}} = get_selected_editor(),
% 	editor:zoom_in(Pid).
% 	
% zoom_out() ->
% 	{ok,{_,Pid}} = get_selected_editor(),
% 	editor:zoom_out(Pid).	
% 	
% go_to_line(Parent) ->
% 	Dialog = wxDialog:new(Parent, ?wxID_ANY, "Go to Line"),
% 	%% Force events to propagate beyond this dialog
% 	wxDialog:setExtraStyle(Dialog, wxDialog:getExtraStyle(Dialog) band (bnot ?wxWS_EX_BLOCK_EVENTS)),
% 	
% 	Panel = wxPanel:new(Dialog),     
% 	Sz = wxBoxSizer:new(?wxVERTICAL),
% 	wxSizer:addSpacer(Sz, 10),
% 	
% 	wxSizer:add(Sz, wxStaticText:new(Panel, ?wxID_ANY, "Enter line:"), 
% 		[{border,10}, {flag, ?wxEXPAND bor ?wxLEFT}]),
% 	wxSizer:addSpacer(Sz, 7),
% 	Input = wxTextCtrl:new(Panel, ?wxID_ANY, []),
% 	wxSizer:add(Sz, Input, [{border,10}, {flag, ?wxEXPAND bor ?wxLEFT bor ?wxRIGHT}, {proportion, 1}]),
% 	wxSizer:addSpacer(Sz, 15),	
% 	
% 	ButtonSz = wxBoxSizer:new(?wxHORIZONTAL),
% 	wxSizer:addSpacer(ButtonSz, 10),	
% 	wxSizer:add(ButtonSz, wxButton:new(Panel, ?wxID_CANCEL, 
% 		[{label,"Cancel"}]), [{border,10}, {flag, ?wxEXPAND bor ?wxBOTTOM}]),
% 	DefButton = wxButton:new(Panel, ?wxID_OK, [{label,"Go"}]),
% 	wxButton:setDefault(DefButton),
% 	wxSizer:add(ButtonSz, DefButton, [{border,10}, {flag, ?wxEXPAND bor ?wxBOTTOM bor ?wxLEFT}]),
% 	wxSizer:addSpacer(ButtonSz, 10),	
% 	wxSizer:add(Sz, ButtonSz),
% 	
% 	Self = self(),
% 	wxButton:connect(DefButton, command_button_clicked, [{callback, fun(E,O)->Self ! done end}]),
% 		
% 	wxPanel:setSizer(Panel, Sz),	
% 	wxSizer:layout(Sz),
% 	wxSizer:setSizeHints(Sz, Dialog),
% 	wxDialog:show(Dialog),
% 	wxWindow:setFocusFromKbd(Input),
% 	
% 	receive
% 		done ->
% 			{Line, Column} = case string:tokens(wxTextCtrl:getValue(Input), ":") of
% 				[Ln | []] -> {Ln, 0};
% 				[Ln, Col | _ ] -> {Ln, Col}
% 			end,
% 			L = try
% 				list_to_integer(Line)
% 			catch _:_ -> 0
% 			end,
% 			C = try
% 				list_to_integer(Column)
% 			catch _:_ -> 0
% 			end,
% 			wxDialog:destroy(Dialog),
% 			{ok,{_,Ed}} = ide:get_selected_editor(),
% 			editor:go_to_position(Ed, {L, C})
% 	end,
% 	ok.

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
	
% transform_selection(#wx{id=Id, event=#wxCommand{type=command_menu_selected}}) ->
% 	Cmd = case Id of
% 		?MENU_ID_UC_SEL -> uppercase;
% 		?MENU_ID_LC_SEL -> lowercase
% 	end,
% 	{ok,{_,Ed}} = ide:get_selected_editor(),
% 	editor:transform_selection(Ed, {transform, Cmd}).