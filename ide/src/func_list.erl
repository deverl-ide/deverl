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
								editor_pid,
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
	EditorPid = proplists:get_value(editor, Config),
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
	wxPanel:setSizer(Panel, Sz),
	
  {Panel, #state{panel=Panel, frame=Parent, list=List, textctrl=Search, editor_pid=EditorPid}}.
  
    
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
	State=#state{textctrl=Search, list=ListBox, editor_pid=Ed}) ->
	% io:format("enter~n"),	
	% wxTextCtrl:setInsertionPointEnd(Search),
	send_to_editor(Str, Ed),
	{noreply, State};
	
handle_event(#wx{event=#wxCommand{type=command_listbox_selected, cmdString=Str}}, 
	State=#state{textctrl=Search, list=ListBox, editor_pid=Ed}) ->
	% io:format("List click: ~p~n", [E]),
	send_to_editor(Str, Ed),
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

handle_call({list,AssocEditor}, _From, State=#state{list=List}) ->
  {reply,List,State#state{editor_pid=AssocEditor}};

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
	{ok, {_,Pid}} = ide:get_selected_editor(),
	ListBox = wx_object:call(?MODULE, {list, Pid}),
	wxListBox:clear(ListBox),
	wxListBox:insertItems(ListBox, Items, 0).
		
remove_dups(List) ->
	sets:to_list(sets:from_list(List)).
	
select(ListBox, Item) ->
	N = wxListBox:findString(ListBox, Item),
	wxListBox:select(ListBox, N),
	wxListBox:setFirstItem(ListBox, N).
	
send_to_editor(Str, Editor) ->
	io:format("To editor: ~p~n", [Str]),
	io:format("Editor: ~p~n", [Editor]),
	editor:find(Editor, Str),
	ok.