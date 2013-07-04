%% The main GUI for the IDE
%% ide.erl

-module(ide).

-include_lib("wx/include/wx.hrl").
-behaviour(wx_object).

%% wx_objects callbacks
-export([start/0, init/1, terminate/2,  code_change/3,
         handle_info/2, handle_call/3, handle_cast/2, handle_event/2,
         save_current_file/0]).

%% Client API         
-export([add_editor/0, add_editor/1, toggle_pane/1, get_selected_editor/0, 
         get_all_editors/0, update_styles/1, apply_to_all_editors/0,
         open_dialog/1]).

%% The record containing the State.
-record(state, {win,  
                env,                                           %% The wx environment
                workspace :: wxAuiNotebook:wxAuiNotebook(),    %% Notebook
                utilities,                                     %% The utilities pane
                test_pane,                                     %% The test pane
                workspace_manager,                             %% Tabbed UI manager for editors
                sash_v :: wxSpliiterWindow:wxSplitterWindow(), %% The vertical splitter
                sash_h :: wxSpliiterWindow:wxSplitterWindow(), %% The horizontal splitter
                sash_v_pos :: integer(),
                sash_h_pos :: integer(),
                status_bar :: wxPanel:wxPanel(),
                font :: wxFont:wxFont()                        %% The initial font used in the editors
                }).

-define(DEFAULT_FRAME_WIDTH,  1300).
-define(DEFAULT_FRAME_HEIGHT, 731).
-define(DEFAULT_UTIL_HEIGHT,  200).
-define(DEFAULT_TEST_WIDTH,   200).
-define(DEFAULT_FONT_SIZE, 12).

-define(DEFAULT_TAB_LABEL, "new_file").

-define(SASH_VERTICAL, 1).
-define(SASH_HORIZONTAL, 2).
-define(SASH_VERT_DEFAULT_POS, 200).
-define(SASH_HOR_DEFAULT_POS, -200).

-define(ID_WORKSPACE, 3211).

start() ->
  start([]).
  
start(Args) ->
  wx_object:start({local, ?MODULE}, ?MODULE, Args, [{debug, [log]}]).
  %% Trap the error {error,{already_started,Pid()}} to prevent the app from 
  %% being opened twice.

init(Options) ->
  wx:new(Options),
  process_flag(trap_exit, true),

  Frame = wxFrame:new(wx:null(), ?wxID_ANY, "Erlang IDE", [{size,{?DEFAULT_FRAME_WIDTH,?DEFAULT_FRAME_HEIGHT}}]),
  wxFrame:connect(Frame, close_window),
  wxFrame:setMinSize(Frame, {300,200}),
  
  FrameSizer = wxBoxSizer:new(?wxVERTICAL),
  wxWindow:setSizer(Frame, FrameSizer),  
  
  SplitterTopBottom = wxSplitterWindow:new(Frame, [{id, ?SASH_HORIZONTAL},{style, ?wxSP_NOBORDER}]),
  SplitterLeftRight = wxSplitterWindow:new(SplitterTopBottom, [{id, ?SASH_VERTICAL}, {style, ?wxSP_NOBORDER}]),

  %% Following two lines, see platforms.txt <1> 
  wxSplitterWindow:setSashSize(SplitterTopBottom, 8),
  wxSplitterWindow:setSashSize(SplitterLeftRight, 8),
  wxSplitterWindow:setSashGravity(SplitterTopBottom,   0.5),
  wxSplitterWindow:setSashGravity(SplitterLeftRight, 0.60),

  wxSizer:add(FrameSizer, SplitterTopBottom, [{flag, ?wxEXPAND}, {proportion, 1}]),

  %% Custom status bar %%
  StatusBar = customStatusBar:new([{parent, Frame}]),
  
  %% Menubar %%
  ide_menu:new([{parent, Frame}, {sb, StatusBar}]),
 
  wxSizer:add(FrameSizer, StatusBar, [{flag, ?wxEXPAND},
                                     {proportion, 0}]),      

  %% The workspace/text editors %%
  Manager = wxAuiManager:new([{managed_wnd, Frame}]),
  %% The centre pane/editor window
  EditorWindowPaneInfo = wxAuiPaneInfo:centrePane(wxAuiPaneInfo:new()), 
  wxAuiPaneInfo:name(EditorWindowPaneInfo, "EditorPane"),
  {Workspace, Font} = create_editor(SplitterLeftRight, Manager, EditorWindowPaneInfo, StatusBar, ?DEFAULT_TAB_LABEL),
  
  %% The left (test case) window
  TestWindow = wxPanel:new(SplitterLeftRight),
  TestSizer = wxBoxSizer:new(?wxVERTICAL),
  wxPanel:setSizer(TestWindow, TestSizer),
  TestT = wxTextCtrl:new(TestWindow, 8001, [{style, ?wxTE_MULTILINE}]), 
  wxSizer:add(TestSizer, TestT, [{flag, ?wxEXPAND}, {proportion, 1}]),
  
  %% The bottom pane/utility window
  Utilities = create_utils(SplitterTopBottom, Manager, ok),  
                                     
  wxSplitterWindow:splitVertically(SplitterLeftRight, TestWindow, Workspace,
		         [{sashPosition, ?SASH_VERT_DEFAULT_POS}]),
             
  wxSplitterWindow:splitHorizontally(SplitterTopBottom, SplitterLeftRight, Utilities,
				     [{sashPosition, ?SASH_HOR_DEFAULT_POS}]),
             
  wxSizer:layout(FrameSizer),
  wxFrame:center(Frame),
  wxFrame:show(Frame),
  
  wxSplitterWindow:setSashGravity(SplitterTopBottom,   1.0), %% Only the top window grows on resize
  wxSplitterWindow:setSashGravity(SplitterLeftRight, 0.0),   %% Only the right window grows
  
  wxSplitterWindow:connect(Frame, command_splitter_sash_pos_changed, [{userData, SplitterLeftRight}]),
  wxSplitterWindow:connect(Frame, command_splitter_sash_pos_changing, [{userData, SplitterLeftRight}]),
  wxSplitterWindow:connect(Frame, command_splitter_doubleclicked),

  State = #state{win=Frame},
  {Frame, State#state{workspace=Workspace, 
                      workspace_manager=Manager,
                      test_pane=TestWindow,
                      utilities=Utilities,
                      status_bar=StatusBar,
                      sash_v_pos=?SASH_VERT_DEFAULT_POS,
                      sash_h_pos=?SASH_HOR_DEFAULT_POS,
                      sash_v=SplitterLeftRight,
                      sash_h=SplitterTopBottom,
                      font=Font
                      }}.

%%%%%%%%%%%%%%%%%%%%%
%%%%% Callbacks %%%%%
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

handle_call(shutdown, _From, State=#state{win=Panel, workspace_manager=Manager}) ->
    wxAuiManager:unInit(Manager),
    wxAuiManager:destroy(Manager),
    wxPanel:destroy(Panel),
    {stop, normal, ok, State};
%% @doc Get the frames sash positions
handle_call(splitter, _From, State) ->
	  {reply, {State#state.sash_v,
             State#state.sash_h,
             State#state.sash_v_pos, 
             State#state.sash_h_pos,
             State#state.workspace,
             State#state.test_pane,
             State#state.utilities}, State};
%% @doc Return the workspace
handle_call(workspace, _From, State) ->
    {reply, {State#state.workspace, 
             State#state.status_bar,
             State#state.font}, State};
handle_call({update_font, Font}, _From, State) ->
    {reply, ok, State#state{font=Font}};
handle_call(Msg, _From, State) ->
    demo:format(State#state{}, "Got Call ~p\n", [Msg]),
    {reply,{error, nyi}, State}.
    
handle_cast(Msg, State) ->
    io:format("Got cast ~p~n",[Msg]),
    {noreply,State}.

%% Window close event 
handle_event(#wx{event=#wxClose{}}, State = #state{win=Frame}) ->
    io:format("~p Closing window ~n",[self()]),
    ok = wxFrame:setStatusText(Frame, "Closing...",[]),
    {stop, normal, State};
%% Splitter window events
handle_event(_W=#wx{id=?SASH_VERTICAL, event=#wxSplitter{type=command_splitter_sash_pos_changed}=_E}, 
             State) ->
    Pos = wxSplitterWindow:getSashPosition(State#state.sash_v),
    %% Don't save the pos if the sash is dragged to zero, as the sash will revert to the middle when shown again (on mac)
    if
      Pos =:= 0 ->
        NewPos = State#state.sash_v_pos;
      true ->
        NewPos = Pos
    end,
    {noreply, State#state{sash_v_pos=NewPos}};
handle_event(_W=#wx{id=?SASH_HORIZONTAL, event=#wxSplitter{type=command_splitter_sash_pos_changed}=_E}, 
             State) ->
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
handle_event(_W = #wx{event = #wxSplitter{type = command_splitter_sash_pos_changing} = _E}, 
             State) ->
    io:format("Sash position changing ~n"),    
    {noreply, State};
handle_event(_W = #wx{event = #wxSplitter{type = command_splitter_doubleclicked} = _E}, 
             State) ->
    io:format("Sash double clicked ~n"),    
    {noreply, State};
%% AuiManager events
handle_event(_W = #wx{event = #wxAuiManager{type = aui_render} = _E}, State) ->
    io:format("render:~n"),   
    {noreply, State};
%% AuiNotebook events
handle_event(#wx{obj = _Workspace,
		 event = #wxAuiNotebook{type = command_auinotebook_page_changed,
					selection = _Sel}}, State) ->
    io:format("changed page~n"),
    {noreply, State};
handle_event(#wx{event=#wxAuiNotebook{type=command_auinotebook_bg_dclick}}, State) ->
    add_editor(State#state.workspace, State#state.status_bar, State#state.font),
    {noreply, State};
handle_event(#wx{event = #wxAuiNotebook{type = command_auinotebook_page_close}}, State) ->
    io:fwrite("page closed~n"),
    {noreply, State};
%% Event catchall for testing
handle_event(Ev = #wx{}, State) ->
    io:format("aui event catchall: ~p\n", [Ev]),
    {noreply, State}.

code_change(_, _, State) ->
    {stop, not_yet_implemented, State}.

terminate(_Reason, _State) ->
    wx:destroy().

%%%%%%%%%%%%%%%%%%%%%
%%%%% Internals %%%%%

%% @doc Create the utilities panel
%% @private
create_utils(Parent, Manager, Pane) ->
  %% Notebook styles
  Style = (0
     bor ?wxAUI_NB_TOP
     bor ?wxAUI_NB_WINDOWLIST_BUTTON
     bor ?wxAUI_NB_TAB_MOVE
     bor ?wxAUI_NB_SCROLL_BUTTONS
    ),
  
  UtilPanel = wxPanel:new(Parent, []),
  
  % Utils = wxAuiNotebook:new(Parent, [{style, Style}]),
  Utils = wxNotebook:new(UtilPanel, 8989, [{style, ?wxBORDER_NONE}]),
  
  UtilSizer = wxBoxSizer:new(?wxVERTICAL),
  wxPanel:setSizer(UtilPanel, UtilSizer),
  
  Console = ide_shell:new([{parent, Utils}]),
  % wxAuiNotebook:addPage(Utils, Console, "Console", []),
  wxNotebook:addPage(Utils, Console, "Console", []),

  Pman = wxPanel:new(Utils, []),
  % wxAuiNotebook:addPage(Utils, Pman, "Process Manager", []),
  wxNotebook:addPage(Utils, Pman, "Process Manager", []),

  Dialyser = wxPanel:new(Utils, []),
  % wxAuiNotebook:addPage(Utils, Dialyser, "Dialyser", []),
  wxNotebook:addPage(Utils, Dialyser, "Dialyser", []),
  
  Debugger = wxPanel:new(Utils, []),
  % wxAuiNotebook:addPage(Utils, Debugger, "Debugger", []),
  wxNotebook:addPage(Utils, Debugger, "Debugger", []),
  
  wxSizer:addSpacer(UtilSizer, 1),
  wxSizer:add(UtilSizer, Utils, [{proportion, 1}, {flag, ?wxEXPAND}]),

  % wxAuiManager:addPane(Manager, Utils, Pane),
  UtilPanel.

%% @doc Create the workspace with the initial editor
%% @private  
create_editor(Parent, Manager, Pane, Sb, Filename) ->
  Style = (0
     bor ?wxAUI_NB_TOP
     bor ?wxAUI_NB_WINDOWLIST_BUTTON
     bor ?wxAUI_NB_TAB_MOVE
     bor ?wxAUI_NB_SCROLL_BUTTONS
     bor ?wxAUI_NB_CLOSE_ON_ALL_TABS
    ),
    
  Workspace = wxAuiNotebook:new(Parent, [{id, ?ID_WORKSPACE}, {style, Style}]),
  Font = wxFont:new(?DEFAULT_FONT_SIZE, ?wxFONTFAMILY_TELETYPE, ?wxNORMAL, ?wxNORMAL,[]),
  
  Editor = editor:start([{parent, Workspace}, {status_bar, Sb},
                         {font, Font}]), %% Returns an editor instance inside a wxPanel
  
  wxAuiNotebook:addPage(Workspace, Editor, Filename, []),
  
  wxAuiManager:addPane(Manager, Workspace, Pane),
  
  wxAuiNotebook:connect(Workspace, command_auinotebook_bg_dclick, []),
  wxAuiNotebook:connect(Workspace, command_auinotebook_page_close, [{skip, true}]),
  wxAuiNotebook:connect(Workspace, command_auinotebook_page_changed), 
  {Workspace, Font}.
  
%% @doc Called internally
%% @private
add_editor(Workspace, Sb, Font) ->
  add_editor(Workspace, ?DEFAULT_TAB_LABEL, Sb, Font),
  Workspace.
  
%%%%%%%%%%%%%%%%%%%%%%
%%%%% Client API %%%%%
  
%% @doc Create a new editor instance in the notebook  
add_editor() -> 
  add_editor(?DEFAULT_TAB_LABEL).
%% @doc Create a new editor with specified filename
add_editor(Filename) ->
  {Workspace, Sb, Font} = wx_object:call(?MODULE, workspace), 
  add_editor(Workspace, Filename, Sb, Font),
  ok.
%% @private
add_editor(Workspace, Filename, Sb, Font) -> 
  Editor = editor:start([{parent, Workspace}, {status_bar, Sb}, {font,Font}]),
  wxAuiNotebook:addPage(Workspace, Editor, Filename, [{select, true}]),
  ok.
%% @doc Create an editor from an existing file
add_editor(Filename, Contents) -> 
  {Workspace, Sb, Font} = wx_object:call(?MODULE, workspace), 
  Editor = editor:start([{parent, Workspace}, {status_bar, Sb}, {font,Font}, {contents, Contents}]),
  wxAuiNotebook:addPage(Workspace, Editor, Filename, [{select, true}]),
  ok.

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
  
%% @doc Get the editor instance from a notebook tab
%% @private
-spec get_editor(Index, Workspace) -> Result when
  Index :: integer(),
  Workspace :: wxAuiNotebook:wxAuiNotebook(),
  Result :: wxStyledTextCtrl:wxStyledTextCtrl().
get_editor(Index, Workspace) ->
  W     = wxAuiNotebook:getPage(Workspace, Index), %% Get the top-level contents (::wxPanel() from editor.erl)
  [Children | _] = wxWindow:getChildren(W),        %% The first child is the STC
  Editor = wx:typeCast(Children, wxStyledTextCtrl),
  Editor.
  
%% @doc Get the editor contained within a workspace tab
-spec get_selected_editor() -> Result when
  Result :: wxStyledTextCtrl:wxStyledTextCtrl().
get_selected_editor() ->
  {Workspace,_,_} = wx_object:call(?MODULE, workspace), 
  Index = wxAuiNotebook:getSelection(Workspace),   %% Get the index of the tab
  W     = wxAuiNotebook:getPage(Workspace, Index), %% Get the top-level contents (::wxPanel() from editor.erl)
  get_editor(Index, Workspace).
  
%% @doc Get all open editor instances
-spec get_all_editors() -> Result when
  Result :: [wxStyledTextCtrl:wxStyledTextCtrl()].
get_all_editors() ->
  {Workspace,_,_} = wx_object:call(?MODULE, workspace),
  Count = wxAuiNotebook:getPageCount(Workspace),
  get_all_editors(Workspace, Count - 1, []).
%% @private
get_all_editors(Workspace, -1, Acc) -> Acc;
get_all_editors(Workspace, Count, Acc) ->
  get_all_editors(Workspace, Count -1, [get_editor(Count, Workspace) | Acc]).

%% @doc Change the font style across all open editors
update_styles(Frame) ->
  {_,_,CurrentFont} = wx_object:call(?MODULE, workspace), 
  FD = wxFontData:new(),
  wxFontData:setInitialFont(FD, CurrentFont),
  %% Display the system font picker
  Dialog = wxFontDialog:new(Frame, FD),
  wxDialog:showModal(Dialog),
  Font = wxFontData:getChosenFont(wxFontDialog:getFontData(Dialog)),
  wx_object:call(?MODULE, {update_font, Font}),
  Fun = fun(STC) ->
        editor:update_style(STC, Font)
        end,
  lists:map(Fun, get_all_editors()),
  ok.
  
%% @doc Save the contents of a file to disk
save_current_file() ->
  Ed = get_selected_editor(),
  Contents = wxStyledTextCtrl:getText(Ed),
  ide_io:save(Ed, Contents).
  
%% @doc Apply the given function to all open editor instances
%% EXAMPLE ON HOW TO CALL A FUNCTION ON ALL EDITORS
apply_to_all_editors() ->
  Fun = fun(STC) ->
        wxStyledTextCtrl:clearAll(STC)
        end,
  lists:map(Fun, get_all_editors()).

open_dialog(Frame) ->
	{Filename, Text} = ide_io:open_file(Frame),
	add_editor(Filename, Text).
