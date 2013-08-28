-module(func_list).

-include_lib("wx/include/wx.hrl").

-behaviour(wx_object).
-export([
	init/1, 
	terminate/2,  
	code_change/3,
	handle_info/2, 
	handle_call/3, 
	handle_cast/2, 
	handle_event/2
	]).
	
-export([
	start/1,
	insert/2,
	set/1
	]).

-record(state, {panel,
            	  config,
								frame,
								list,
								matches,
								textctrl,
								select_on_click,
								ins_pt	%% The current insertion point in the text ctrl
            	 }).
							 
-define(SEARCH_TEXT_DEFAULT, "Search functions..").
           
start(Config) ->
	wx_object:start_link({local, ?MODULE}, ?MODULE, Config, []).

init(Config) ->
	Parent = proplists:get_value(parent, Config),	
	Panel = wxPanel:new(Parent),
	Sz = wxStaticBoxSizer:new(?wxVERTICAL, Panel),
	Sb = wxStaticBoxSizer:getStaticBox(Sz),
	wxStaticBox:setBackgroundColour(Sb, {200,200,200}),
	wxSizer:addSpacer(Sz, 10),
	
	Search = wxTextCtrl:new(Panel, ?wxID_ANY, [{style, ?wxTE_PROCESS_ENTER}]),
	wxTextCtrl:setFont(Search, wxFont:new(11,?wxFONTFAMILY_DEFAULT,?wxFONTSTYLE_NORMAL,?wxNORMAL)),
	wxTextCtrl:setValue(Search, ?SEARCH_TEXT_DEFAULT),
  wxSizer:add(Sz, Search, [{flag, ?wxEXPAND bor ?wxALIGN_CENTER_VERTICAL}, {proportion, 0}]),	
	wxSizer:addSpacer(Sz, 10),
	wxTextCtrl:connect(Search, command_text_enter, [{skip, true}]),
	wxTextCtrl:connect(Search, key_down, [{skip, true}]),
	wxTextCtrl:connect(Search, set_focus, [{skip, true}]),
	wxTextCtrl:connect(Search, left_up, [{skip, true}]),
	
														
	List = wxListBox:new(Panel, 2, [{style, ?wxLB_SINGLE}]),	
	wxSizer:add(Sz, List, [{flag, ?wxEXPAND}, {proportion, 1}]),
	wxListBox:connect(List, command_listbox_selected, []),
	% wxListBox:insertItems(List, ["start",
	% "init",
	% "list_to_integer",
	% "update_line_margin",
	% "handle_info",
	% "handle_call",
	% "handle_call",
	% "handle_call",
	% "handle_call",
	% "handle_call",
	% "handle_call",
	% "handle_call",
	% "handle_cast",
	% "update_sb_line",
	% "update_sb_selection",
	% "update_sb_selection",
	% "update_sb_line",
	% "update_line_margin",
	% "parse_functions",
	% "update_sb_line",
	% "handle_event",
	% "handle_event",
	% "handle_event",
	% "code_change",
	% "terminate",
	% "to_indent",
	% "keywords",
	% "selected",
	% "update_sb_line",
	% "update_sb_line",
	% "update_sb_selection",
	% "position_to_x_y",
	% "get_caret_position",
	% "position_to_x_y",
	% "update_line_margin",
	% "set_linenumber_default",
	% "adjust_margin_width",
	% "save_status",
	% "set_savepoint",
	% "get_text",
	% "get_id",
	% "update_font",
	% "set_font_style",
	% "set_theme",
	% "setup_theme",
	% "set_theme_styles",
	% "apply_lexer_styles",
	% "set_default_styles",
	% "set_theme_styles",
	% "update_line_margin",
	% "set_font_style",
	% "set_marker_colour",
	% "set_linenumber_default",
	% "adjust_margin_width",
	% "apply_lexer_styles",
	% "set_font_style",
	% "update_line_margin",
	% "set_font_style",
	% "set_marker_colour",
	% "set_font_size",
	% "reset_styles_to_default",
	% "set_tab_width",
	% "set_use_tabs",
	% "set_indent_guides",
	% "set_line_wrap",
	% "set_line_margin_visible",
	% "find",
	% "find",
	% "find",
	% "find_all",
	% "find",
	% "replace_all",
	% "replace_all",
	% "replace_all",
	% "replace_all",
	% "replace",
	% "indent_line_left",
	% "indent_line_right",
	% "indent_line",
	% "lines",
	% "count_selected_lines",
	% "comment",
	% "correct_caret",
	% "count_commented_lines_re",
	% "length",
	% "regex_replace",
	% "remove_comments",
	% "regex_replace",
	% "insert_comments",
	% "regex_replace",
	% "single_line_comment",
	% "correct_caret",
	% "parse_functions",
	% "go_to_position",
	% "flash_current_line",
	% "flash_current_line",
	% "flash_current_line",
	% "flash_current_line",
	% "zoom_in",
	% "zoom_out",
	% "transform_uc_selection",
	% "transform_lc_selection",
	% "transform_selection"], 0),
		
	wxPanel:setSizer(Panel, Sz),
	
  {Panel, #state{panel=Panel, frame=Parent, list=List, textctrl=Search}}.
  
    
%% =====================================================================
%% @doc OTP behaviour callbacks

%% Window close event 
handle_event(#wx{event=#wxClose{}}, State) ->
  io:format("~p Closing window ~n",[self()]),
  {stop, normal, State};
	
handle_event(#wx{event=#wxKey{type=key_down, keyCode=Kc, controlDown=Ctrl}}, 
	State=#state{matches=Matches, textctrl=Search}) 
		when Ctrl =:= true orelse Kc =:= ?WXK_BACK orelse Kc =:= ?WXK_DELETE orelse
			 Kc =:= ?WXK_LEFT orelse Kc =:= ?WXK_RIGHT orelse Kc =:= ?WXK_ESCAPE ->
	% io:format("Trap~n"),	
	{noreply, State};
	
handle_event(#wx{event=#wxKey{type=key_down, keyCode=?WXK_DOWN}}, 
	State=#state{list=ListBox, matches=Matches, textctrl=Search, ins_pt=InsPt}) ->
	% io:format("TAB~n"),
	% io:format("Matches: ~p~n", [Matches]),
	M = case Matches of
		{PrevIndex, Items} -> 
			Index = getNextIndex(PrevIndex, Items),
			Value = lists:nth(Index,Items),
			auto_complete(Search, InsPt, Value),
			% wxTextCtrl:setInsertionPoint(Search, 3),
			select(ListBox, Value),
			{Index,Items};
		_ -> undefined
	end,
	{noreply, State#state{matches=M}};

handle_event(#wx{event=#wxKey{type=key_down, keyCode=?WXK_UP}}, 
	State=#state{list=ListBox, matches=Matches, textctrl=Search, ins_pt=InsPt}) ->
	% io:format("TAB~n"),
	% io:format("Matches: ~p~n", [Matches]),
	M = case Matches of
		{PrevIndex, Items} -> 
			Index = getPrevIndex(PrevIndex, Items),
			Value = lists:nth(Index,Items),
			auto_complete(Search, InsPt, Value),
			select(ListBox, Value),
			{Index,Items};
		_ -> undefined
	end,
	{noreply, State#state{matches=M}};

handle_event(#wx{event=#wxKey{type=key_down}}, 
	State=#state{list=ListBox, textctrl=Search}) ->
	Str = wxTextCtrl:getValue(Search),
	% io:format("Letter: ~p~n", [Str]),
	% io:format("command_text_updated~n"),
	Strings = get_strings(ListBox),
	% io:format("String:: ~p~n", [Strings]),
	InsPt = wxTextCtrl:getInsertionPoint(Search),
	
	Matches = case starts_with(Strings, Str) of
		[] -> undefined;
		L=[H|_] -> 
			auto_complete(Search, InsPt, H),
			select(ListBox, H),
			{1, L}
	end,
	% io:format("Matches: ~p~n", [Matches]),
	{noreply, State#state{matches=Matches, ins_pt=InsPt}};

handle_event(#wx{event=#wxCommand{type=command_text_enter, cmdString=Str}}, 
	State=#state{textctrl=Search, list=ListBox}) ->
	% io:format("enter~n"),	
	% wxTextCtrl:setInsertionPointEnd(Search),
	% send_to_editor(Str),
	{noreply, State};
	
handle_event(#wx{event=#wxCommand{type=command_listbox_selected, cmdString=Str}}, 
	State=#state{textctrl=Search, list=ListBox}) ->
	% io:format("List click: ~p~n", [E]),
	send_to_editor(Str),
	{noreply, State};
	
handle_event(#wx{event=#wxCommand{type=char, cmdString=Str}}, 
	State=#state{textctrl=Search, list=ListBox}) ->
	io:format("Tab~n"),
	{noreply, State};
	
handle_event(#wx{event=#wxFocus{type=set_focus}}, State) ->	
	%% Set a flag to ensure that the default search text is selected
	{noreply, State#state{select_on_click=true}};
	
handle_event(#wx{event=#wxMouse{type=left_up}},
	State=#state{select_on_click=true, textctrl=Search}) ->	
	wxTextCtrl:setSelection(Search, -1, -1),
	{noreply, State#state{select_on_click=false}};
	
handle_event(Ev, State) ->
  io:format("Got Event ~p~n",[Ev]),
  {noreply,State}.
	
handle_info(select_search, State=#state{textctrl=Search}) ->
  wxTextCtrl:setSelection(Search, -1, -1),
  {noreply,State};
handle_info(Msg, State) ->
  io:format( "Got Info ~p~n",[Msg]),
  {noreply,State}.

handle_call(list, _From, State=#state{list=List}) ->
  {reply,List,State};

handle_call(Msg, _From, State) ->
  io:format("Got Call ~p~n",[Msg]),
  {reply,ok,State}.

handle_cast(Msg, State) ->
  io:format("Got cast ~p~n",[Msg]),
  {noreply,State}.

code_change(_, _, State) ->
  {stop, ignore, State}.

terminate(_Reason, #state{panel=Panel}) ->
	io:format("TERMINATE FUNCTIONS~n"),
  wxPanel:destroy(Panel),
  ok.
	
auto_complete(TextCtrl, InsPt, Value) ->
	% InsPt = wxTextCtrl:getInsertionPoint(TextCtrl),
	wxTextCtrl:setValue(TextCtrl, Value),
	wxTextCtrl:setSelection(TextCtrl, InsPt, -1).
	
%% Returns the first index when the current index is out of range
%% Creates a circular list
getNextIndex(Index, Items) when Index + 1 > length(Items) -> 1;
getNextIndex(Index, Items) -> Index + 1.
getPrevIndex(Index, Items) when Index - 1 < 1 -> length(Items);
getPrevIndex(Index, Items) -> Index - 1.

starts_with(List, Str) ->
	Length = length(Str),
	Matches = lists:foldr(fun(Item, Acc) ->
		% io:format("~p~n", [Item]),
		case string:substr(Item, 1, Length) of
			Str -> [Item | Acc];
			_ -> Acc
		end
	end, [], List),
	Matches.
	
get_strings(ListBox) ->
	[wxListBox:getString(ListBox, N) || N <- lists:seq(0,wxListBox:getCount(ListBox))].

insert([], Pos) ->
	ok;
insert([H|T]=Items, Pos) ->
	ListBox = wx_object:call(?MODULE, list),
	wxListBox:insertItems(ListBox, Items, Pos),
	ok.
	
set(Items) ->
	ListBox = wx_object:call(?MODULE, list),
	wxListBox:clear(ListBox),
	wxListBox:insertItems(ListBox, Items, 0).
		
remove_dups(List) ->
	sets:to_list(sets:from_list(List)).
	
select(ListBox, Item) ->
	N = wxListBox:findString(ListBox, Item),
	wxListBox:select(ListBox, N),
	wxListBox:setFirstItem(ListBox, N).
	
send_to_editor(Str) ->
	io:format("To editor: ~p~n", [Str]),
	ok.