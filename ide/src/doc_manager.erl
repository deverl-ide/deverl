-module(document_io).

-include_lib("wx/include/wx.hrl").
-include("ide.hrl").

-behaviour(wx_object).

-export([new/1, init/1, terminate/2,  code_change/3,
         handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).
				 
% API         
-export([
	add_editor/0, 
	add_editor/1, 
	add_editor_with_contents/3,
	close_selected_editor/0, 
	close_all_editors/0,
	toggle_pane/1, 
	get_selected_editor/0, 
	get_all_editors/0, 
	update_styles/1, 
	save_current_file/0,
	save_new/0, 
	save_all/0,
	open_file/1,
	open_dialog/1,
	find_replace/1,
	get_current_theme_name/0,
	set_theme/1,
	set_line_wrap/1,
	set_line_margin_visible/1,
	set_indent_tabs/1,
	set_indent_guides/1,
	indent_line_right/0,
	indent_line_left/0,
	go_to_line/1,
	comment/0,
	zoom_in/0,
	zoom_out/0,
	transform_selection/1,
	load_file/0,
	]).
         
-record(state, {
								manager,
             		workspace :: wxAuiNotebook:wxAuiNotebook(),    %% Notebook
                editor_pids :: {integer(), pid()}              %% A table containing the Id returned when an editor is created, and the associated pid
                }).

-define(ID_WORKSPACE, 3211).


new(Config) ->
  wx_object:start_link({local, ?MODULE}, ?MODULE, Config, []).

init(Config) ->
	{Parent, Sb} = proplists:get_value(conf, Config),
	
	Manager = wxAuiManager:new([{managed_wnd, Parent}]),
	EditorWindowPaneInfo = wxAuiPaneInfo:centrePane(wxAuiPaneInfo:new()),
	
	Style = (0
			bor ?wxAUI_NB_TOP
			bor ?wxAUI_NB_WINDOWLIST_BUTTON
			bor ?wxAUI_NB_TAB_MOVE
			bor ?wxAUI_NB_SCROLL_BUTTONS
			bor ?wxAUI_NB_CLOSE_ON_ALL_TABS
			),
    
	Workspace = wxAuiNotebook:new(Parent, [{id, ?ID_WORKSPACE}, {style, Style}]),  
	Editor = editor:start([{parent, Workspace}, {status_bar, Sb},
                           {font, user_prefs:get_user_pref({pref, font})}]), %% Returns an editor instance inside a wxPanel
  
	TabId = ets:new(editors, [public]),
	{_,Id,_,Pid} = Editor,
	ets:insert(TabId,{Id, Pid}),

	wxAuiNotebook:addPage(Workspace, Editor, ?DEFAULT_TAB_LABEL, []),
  
	wxAuiManager:addPane(Manager, Workspace, Pane),
  
	Close = fun(_,O) ->
				wxNotifyEvent:veto(O),
				close_selected_editor()
			end,
  
	wxAuiNotebook:connect(Workspace, command_auinotebook_bg_dclick, []),
	wxAuiNotebook:connect(Workspace, command_auinotebook_page_close, [{callback,Close},{userData,TabId}]),
	wxAuiNotebook:connect(Workspace, command_auinotebook_page_changed),   
	
  {Workspace, #state{workspace=Workspace, manager=Manager, editor_pids=TabId}}.

handle_info(Msg, State) ->
    io:format("Got Info ~p~n",[Msg]),
    {noreply,State}.

handle_cast(Msg, State) ->
    io:format("Got cast ~p~n",[Msg]),
    {noreply,State}.
    
handle_call(Msg, _From, State) ->
  io:format("Got Call ~p~n",[Msg]),
  {reply,ok,State}.
  
code_change(_, _, State) ->
  {stop, ignore, State}.

terminate(_Reason, _) ->
  ok.

%% =====================================================================
%% AUI handlers
%% 
%% =====================================================================
    
handle_event(#wx{obj = _Workspace, event = #wxAuiNotebook{type = command_auinotebook_page_changed, 
			selection = Index}}, State) ->
  %% Make sure editor knows (needs to update sb)
  editor:selected(get_editor_pid(Index, State#state.workspace, State#state.editor_pids), State#state.status_bar),
  {noreply, State}; 
    
handle_event(#wx{event=#wxAuiNotebook{type=command_auinotebook_bg_dclick}}, State) ->
  add_editor(State#state.workspace, State#state.status_bar, 
             State#state.editor_pids),
  {noreply, State};

handle_event(Ev = #wx{}, State = #state{}) ->
  io:format("Got Event ~p~n",[Ev]),
  {noreply,State}.


%% =====================================================================
%% @doc 
%%
%% @private

add_editor(Workspace, Sb, TabId) ->
	add_editor(Workspace, ?DEFAULT_TAB_LABEL, Sb, TabId),
	Workspace.
 

%% =====================================================================
%% @doc Create a new editor instance in the notebook  

add_editor() -> 
	add_editor(?DEFAULT_TAB_LABEL).
  
%% @doc Create a new editor with specified filename
add_editor(Filename) ->
	{Workspace, Sb, TabId} = wx_object:call(?MODULE, workspace), 
	add_editor(Workspace, Filename, Sb, TabId),
	ok.
  
%% @private
add_editor(Workspace, Filename, Sb, TabId) ->
	Editor = editor:start([{parent, Workspace}, {status_bar, Sb}, {font,user_prefs:get_user_pref({pref, font})}]),
	wxAuiNotebook:addPage(Workspace, Editor, Filename, [{select, true}]),
	{_,Id,_,Pid} = Editor,
	ets:insert_new(TabId,{Id, Pid}),
	ok.
  
%% @doc Create an editor from an existing file
add_editor_with_contents(Path, Filename, Contents) -> 
		{Workspace, Sb, TabId} = wx_object:call(?MODULE, workspace), 
		Editor = editor:start([{parent, Workspace}, {status_bar, Sb}, {font,user_prefs:get_user_pref({pref, font})}, 
							   {file, {Path, Filename, Contents}}]),
		wxAuiNotebook:addPage(Workspace, Editor, Filename, [{select, true}]),
		{_,Id,_,Pid} = Editor,
		ets:insert_new(TabId,{Id, Pid}),
		ok.

%% =====================================================================
%% @doc Get the Pid of an editor instance at position Index.

-spec get_editor_pid(Index) -> Result when
	Index :: integer(),
	Result :: pid().  

get_editor_pid(Index) ->
	{Workspace,_,PidTable} = wx_object:call(?MODULE, workspace), 
	get_editor_pid(Index, Workspace, PidTable).
  
-spec get_editor_pid(Index, Workspace, PidTable) -> Result when
	Index :: integer(),
	Workspace :: wxAuiNotebook:wxAuiNotebook(),
	PidTable :: term(),
	Result :: pid().  

get_editor_pid(Index, Workspace, PidTable) ->
	{_,Key,_,_} = wxAuiNotebook:getPage(Workspace, Index),
	[{_,Pid}] = ets:lookup(PidTable, Key),
	Pid.

	
%% =====================================================================
%% @doc Get the Pid of the currently selected editor.
	
-spec get_selected_editor() -> Result when
	Result :: {'error', 'no_open_editor'} | 
			  {'ok', {integer(), pid()}}.
            
get_selected_editor() ->
	{Workspace,_,_} = wx_object:call(?MODULE, workspace), 
	case wxAuiNotebook:getSelection(Workspace) of %% Get the index of the tab
		-1 -> %% no editor instance
				{error, no_open_editor};
		Index ->
				{ok, {Index, get_editor_pid(Index)}}
	end.
	

%% =====================================================================
%% @doc Get all open editor instances.
%% Returns a list of tuples of the form: {Index, EditorPid}, where
%% Index starts at 0.
  
-spec get_all_editors() -> Result when
	Result :: [{integer(), pid()}].
	-module(document_io).

get_all_editors() ->
	{Workspace,_,_} = wx_object:call(?MODULE, workspace), 
	Count = wxAuiNotebook:getPageCount(Workspace),
	get_all_editors(Workspace, Count - 1, []).

get_all_editors(_, -1, Acc) -> 
	Acc;

get_all_editors(Workspace, Count, Acc) ->
	get_all_editors(Workspace, Count -1, [{Count, get_editor_pid(Count)} | Acc]).
	
	
%% =====================================================================
%% Open/save/close editor functions
%% 
%% =====================================================================

%% =====================================================================
%% @doc Save the currently selected file to disk

-spec save_current_file() -> 'ok'. %% Not yet done

save_current_file() ->
	case get_selected_editor() of
		{error, no_open_editor} ->
			%% Toolbar/menubar buttons should be disabled to prevent this action
			io:format("No editor open.~n");
		{ok, {Index, Pid}} ->
			save_file(Index, Pid)
	end.


%% =====================================================================
%% @doc Save the contents of the editor Pid located at index Index.
  
save_file(Index, Pid) ->  
	{Workspace,Sb,_} = wx_object:call(?MODULE, workspace), 
	case editor:save_status(Pid) of
		{save_status, new_file} ->
			save_new(Index, Workspace, Sb, Pid);
		{save_status, no_file} ->
			%% Display save dialog
			save_new(Index, Workspace, Sb, Pid);
		{save_status, unmodified} ->
			%% Document is unmodified, no need to save
			ide_status_bar:set_text_timeout(Sb, {field, help}, "Document already saved.");
		{save_status, Path, Fn} ->
			%% Document already exists, overwrite
			Contents = editor:get_text(Pid),
			ide_io:save(Path, Contents),
			editor:set_savepoint(Pid, Path, Fn),
			ide_status_bar:set_text_timeout(Sb, {field, help}, "Document saved.")
	end.


%% =====================================================================
%% @doc Save the erlang editor with pid Pid located at index position 
%% Index to disk. The user will be prompted for a save path.
%%
%% @private

-spec save_new(Index, Workspace, Sb, Pid) -> Result when
	Index :: integer(),
	Workspace :: wxAuiNotebook:wxAuiNotebook(),
	Sb :: ide_status_bar:status_bar(),
	Pid :: pid(),
	Result :: {save, cancelled} %% User cancelled save
			| {save, complete}. %% Save success 

save_new(Index, Workspace, Sb, Pid) ->
	Contents = editor:get_text(Pid),
	case ide_io:save_as(Workspace, Contents) of
		{cancel} ->
			ide_status_bar:set_text_timeout(Sb, {field, help}, "Document not saved."),
			{save, cancelled};
		{ok, {Path, Filename}}  ->
			wxAuiNotebook:setPageText(Workspace, Index, Filename),
			editor:set_savepoint(Pid, Path, Filename),
			ide_status_bar:set_text_timeout(Sb, {field, help}, "Document saved."),
			{save, complete}
	end.
  
-spec save_new() -> Result when
  Result :: {save, cancelled}
          | {save, complete}.
  
save_new() ->
	case get_selected_editor() of
		{error, no_open_editor} ->
			%% Toolbar/menubar buttons should be disabled to prevent this action
			io:format("No editor open.~n");
		{ok, {Index, Pid}} ->
			{Workspace,Sb,_,_} = wx_object:call(?MODULE, workspace),
			save_new(Index, Workspace, Sb, Pid)
	end.


%% =====================================================================
%% @doc Save all open editors.

save_all() ->
	Fun = fun({Index, Pid}) ->
		      save_file(Index, Pid)
		  end,
	lists:map(Fun, get_all_editors()).


%% =====================================================================
%% @doc

-spec open_file(Frame) -> 'ok' when
	Frame :: wxWindow:wxWindow().

open_file(Frame) ->
	case ide_io:open(Frame) of
		{cancel} ->
			ok;
		{Path, Filename, Contents} ->
			add_editor_with_contents(Path, Filename, Contents),
			ok
	end.


%% =====================================================================
%% @doc Close the selected editor
	
close_selected_editor() ->
	case get_selected_editor() of
		{error, _} ->
			{error, no_open_editor};
		{ok, {Index, EditorPid}} ->
			close_editor(EditorPid, Index)
	end.


%% =====================================================================
%% @doc Close the selected editor

close_editor(EditorPid, Index) ->
	{Workspace,_Sb,Tab} = wx_object:call(?MODULE, workspace),
	case editor:save_status(EditorPid) of
		{save_status, new_file} ->
			ets:delete(Tab, editor:get_id(EditorPid)),
      wxAuiNotebook:deletePage(Workspace, Index);
		{save_status, unmodified} -> %% Go ahead, close the editor
			ets:delete(Tab, editor:get_id(EditorPid)),
      wxAuiNotebook:deletePage(Workspace, Index);
		_ -> 
			io:format("Close dialog needs to be displayed.~n")
	end.
  
  
%% =====================================================================
%% @doc Close all editor instances

close_all_editors() ->
	Fun = fun({Index,Pid}) ->
		      close_editor(Pid, Index)
		  end,
	lists:map(Fun, lists:reverse(get_all_editors())),
	ok.


%% =====================================================================
%% @doc Open a dialog box for various functions
%% Buttons = [{Label, Id, Function}]

open_dialog(Parent) ->
	Dialog = wxDialog:new(Parent, ?ID_DIALOG, "Title"),
  
	Bs = wxBoxSizer:new(?wxHORIZONTAL),
	Ba = wxButton:new(Dialog, 1, [{label, "Nibble"}]),
	Bb = wxButton:new(Dialog, 2, [{label, "Nobble"}]),
	wxSizer:add(Bs, Ba),
	wxSizer:add(Bs, Bb),
    
	Box  = wxBoxSizer:new(?wxVERTICAL),

	wxSizer:add(Box, Bs,  [{border, 2}, {flag, ?wxALL bor ?wxEXPAND}]),    
	wxWindow:setSizer(Dialog, Box),
	wxSizer:fit(Box, Dialog),
	wxSizer:setSizeHints(Box,Dialog),
  
	wxDialog:showModal(Dialog).
  
	
%% =====================================================================
%% @doc Change the font style across all open editors

update_styles(Frame) ->
  %% Display the system font picker
  FD = wxFontData:new(),
  wxFontData:setInitialFont(FD, user_prefs:get_user_pref({pref, font})),
  Dialog = wxFontDialog:new(Frame, FD),
  case wxDialog:showModal(Dialog) of
    ?wxID_OK ->
      %% Get the user selected font, and update the editors
      Font = wxFontData:getChosenFont(wxFontDialog:getFontData(Dialog)),
      user_prefs:set_user_pref(font, Font),
      Fun = fun({_, Pid}) ->
              editor:update_font(Pid, Font)
					  end,
      lists:map(Fun, get_all_editors()),
      ok;
    ?wxID_CANCEL ->
				ok
	end. 


%% =====================================================================
%% @doc Show the find/replace dialog
%% Might be better in editor.erl

find_replace(Parent) ->
  FindData = find_replace_data:new(),
  
  %% This data will eventually be loaded from transient/permanent storage PREFS!!
  find_replace_data:set_options(FindData, ?IGNORE_CASE bor ?WHOLE_WORD bor ?START_WORD),
  find_replace_data:set_search_location(FindData, ?FIND_LOC_DOC),
  
  case erlang:whereis(find_replace_dialog) of
    undefined ->
      find_replace_dialog:show(find_replace_dialog:new(Parent, FindData));
    Pid ->
      wxDialog:raise(find_replace_dialog:get_ref(Pid))
  end.

		
set_theme(ThemeMenu) ->
  {ok, Ckd} = get_checked_menu_item(wxMenu:getMenuItems(ThemeMenu)),
  Fun = fun({_, Pid}) ->
          editor:set_theme(Pid, wxMenuItem:getLabel(Ckd), user_prefs:get_user_pref({pref, font}))
        end,
  lists:map(Fun, get_all_editors()),
  user_prefs:set_user_pref(theme, wxMenuItem:getLabel(Ckd)).

get_current_theme_name() ->
  Frame = wx_object:call(?MODULE, frame),
  Mb = wxFrame:getMenuBar(Frame),
  Menu = wxMenuBar:findMenu(Mb, "View"),
  Item = wxMenu:findItem(wxMenuBar:getMenu(Mb, Menu), ?MENU_ID_THEME_SELECT),
  Itms = wxMenu:getMenuItems(wxMenuItem:getSubMenu(Item)),
  {ok, Ckd} = get_checked_menu_item(Itms),
  wxMenuItem:getLabel(Ckd).
	
get_checked_menu_item([]) ->
  {error, nomatch};
get_checked_menu_item([H|T]) ->
  case wxMenuItem:isChecked(H) of
    true ->
      {ok, H};
    _ ->
      get_checked_menu_item(T)
  end.
	
set_line_wrap(Menu) ->
  Bool = wxMenuItem:isChecked(wxMenu:findItem(Menu, ?MENU_ID_LINE_WRAP)),
  Fun = fun({_, Pid}) ->
          editor:set_line_wrap(Pid, Bool)
  end,
  lists:map(Fun, get_all_editors()),
  user_prefs:set_user_pref(line_wrap, Bool).

set_line_margin_visible(Menu) ->
  Bool = wxMenuItem:isChecked(wxMenu:findItem(Menu, ?MENU_ID_LN_TOGGLE)),
  Fun = fun({_, Pid}) ->
          editor:set_line_margin_visible(Pid, Bool)
  end,
  lists:map(Fun, get_all_editors()),
  user_prefs:set_user_pref(show_line_no, Bool).

set_indent_tabs(#wx{id=Id, event=#wxCommand{type=command_menu_selected}}) ->
  Cmd = case Id of
    ?MENU_ID_INDENT_SPACES -> false;
    ?MENU_ID_INDENT_TABS -> true
  end,
  Fun = fun({_, Pid}) ->
          editor:set_use_tabs(Pid, Cmd)
  end,
  lists:map(Fun, get_all_editors()),
  user_prefs:set_user_pref(use_tabs, Cmd).
  
set_indent_guides(Menu) ->
  Bool = wxMenuItem:isChecked(wxMenu:findItem(Menu, ?MENU_ID_INDENT_GUIDES)),
  Fun = fun({_, Pid}) ->
          editor:set_indent_guides(Pid, Bool)
  end,
  lists:map(Fun, get_all_editors()),
  user_prefs:set_user_pref(indent_guides, Bool).

indent_line_right() ->
  {ok,{_,Pid}} = get_selected_editor(),
  editor:indent_line_right(Pid),
  ok.

indent_line_left() ->
  {ok,{_,Pid}} = get_selected_editor(),
  editor:indent_line_left(Pid),
  ok.

comment() ->
  {ok,{_,Pid}} = get_selected_editor(),
  editor:comment(Pid),
  ok.

zoom_in() ->
  {ok,{_,Pid}} = get_selected_editor(),
  editor:zoom_in(Pid).

zoom_out() ->
  {ok,{_,Pid}} = get_selected_editor(),
  editor:zoom_out(Pid).	

go_to_line(Parent) ->
  Dialog = wxDialog:new(Parent, ?wxID_ANY, "Go to Line"),
  %% Force events to propagate beyond this dialog
  wxDialog:setExtraStyle(Dialog, wxDialog:getExtraStyle(Dialog) band (bnot ?wxWS_EX_BLOCK_EVENTS)),

  Panel = wxPanel:new(Dialog),     
  Sz = wxBoxSizer:new(?wxVERTICAL),
  wxSizer:addSpacer(Sz, 10),

  wxSizer:add(Sz, wxStaticText:new(Panel, ?wxID_ANY, "Enter line:"), 
    [{border,10}, {flag, ?wxEXPAND bor ?wxLEFT}]),
  wxSizer:addSpacer(Sz, 7),
  Input = wxTextCtrl:new(Panel, ?wxID_ANY, []),
  wxSizer:add(Sz, Input, [{border,10}, {flag, ?wxEXPAND bor ?wxLEFT bor ?wxRIGHT}, {proportion, 1}]),
  wxSizer:addSpacer(Sz, 15),	

  ButtonSz = wxBoxSizer:new(?wxHORIZONTAL),
  wxSizer:addSpacer(ButtonSz, 10),	
  wxSizer:add(ButtonSz, wxButton:new(Panel, ?wxID_CANCEL, 
    [{label,"Cancel"}]), [{border,10}, {flag, ?wxEXPAND bor ?wxBOTTOM}]),
  DefButton = wxButton:new(Panel, ?wxID_OK, [{label,"Go"}]),
  wxButton:setDefault(DefButton),
  wxSizer:add(ButtonSz, DefButton, [{border,10}, {flag, ?wxEXPAND bor ?wxBOTTOM bor ?wxLEFT}]),
  wxSizer:addSpacer(ButtonSz, 10),	
  wxSizer:add(Sz, ButtonSz),

  Self = self(),
  wxButton:connect(DefButton, command_button_clicked, [{callback, fun(E,O)->Self ! done end}]),
  
  wxPanel:setSizer(Panel, Sz),	
  wxSizer:layout(Sz),
  wxSizer:setSizeHints(Sz, Dialog),
  wxDialog:show(Dialog),
  wxWindow:setFocusFromKbd(Input),

  receive
    done ->
      {Line, Column} = case string:tokens(wxTextCtrl:getValue(Input), ":") of
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
      wxDialog:destroy(Dialog),
      {ok,{_,Ed}} = ide:get_selected_editor(),
      editor:go_to_position(Ed, {L, C})
  end,
  ok.
		
		
transform_selection(#wx{id=Id, event=#wxCommand{type=command_menu_selected}}) ->
	Cmd = case Id of
		?MENU_ID_UC_SEL -> uppercase;
		?MENU_ID_LC_SEL -> lowercase
	end,
	{ok,{_,Ed}} = ide:get_selected_editor(),
	editor:transform_selection(Ed, {transform, Cmd}).
