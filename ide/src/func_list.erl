-module(func_list).

-include_lib("wx/include/wx.hrl").
-include("../include/ide.hrl").

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
	set/1
	]).

-record(state, {panel,
            	  config,
								frame,
								editor_pid,
								list,
								textctrl,
								start,
								search_str
            	 }).

-define(SEARCH_TEXT_DEFAULT, "Search symbols..").


start(Config) ->
	wx_object:start_link({local, ?MODULE}, ?MODULE, Config, []).

init(Config) ->
	Parent = proplists:get_value(parent, Config),
	EditorPid = proplists:get_value(editor, Config),
	Panel = wxPanel:new(Parent),
	Sz = wxBoxSizer:new(?wxVERTICAL),
  
	Search = wxTextCtrl:new(Panel, ?WINDOW_FUNCTION_SEARCH, [{style, ?wxTE_PROCESS_ENTER}]),
	wxTextCtrl:setValue(Search, ?SEARCH_TEXT_DEFAULT),
  wxSizer:addSpacer(Sz, 2),
	wxSizer:add(Sz, Search, [{flag, ?wxEXPAND bor ?wxALL}, {border, 2}, {proportion, 0}]),

	wxTextCtrl:connect(Search, command_text_enter, [{skip, true}]),
	wxTextCtrl:connect(Search, key_down, [{skip, true}]),
	wxTextCtrl:connect(Search, set_focus, [{skip, true}]),
	wxTextCtrl:connect(Search, kill_focus, [{skip, true}]),

	List = wxListCtrl:new(Panel, [{style, ?wxLC_REPORT bor ?wxLC_NO_HEADER bor ?wxLC_SINGLE_SEL bor ?wxBORDER_NONE}]),
	wxSizer:add(Sz, List, [{flag, ?wxEXPAND bor ?wxALL}, {border, 2}, {proportion, 1}]),
	wxListCtrl:insertColumn(List, 0, "Heading", []),

	{W,H} = wxListCtrl:getSize(List),
	wxListCtrl:setColumnWidth(List, 0, W),

	%% Insert
	% Insert =
	% fun({Name, Ln}) ->
	% 	Index = wxListCtrl:insertItem(List, 0, Name),
	% 	wxListCtrl:setItemData(List, Index, Ln)
	% end,
	% L = [
	% {"goggle",2},{"record2",5},{"food",20},{"func2",30},
	% {"record3",32},{"record4",55},{"func3",60},{"func4",70},
	% {"record5",72},{"more",75},{"func5",80},{"func6",90},
	% {"record6",92},{"record8",95},{"func7",100},{"func8",130},
	% {"raaa",132},{"boob",135},{"nipp",140},{"bottom",143},
	% {"fool",152},{"record6",155},{"func5",160},{"func6",180},
	% {"record6",192},{"trap",195},{"func7",200},{"mong",230}
	% ],
	% lists:foreach(Insert, L),

	%% Background

	%   Fun =
	% fun(Item) ->
	% 	case Item rem 2 of
	% 	    0 ->
	% 				wxListCtrl:setItemBackgroundColour(List, Item, ?ROW_BG_EVEN);
	% 	    _ ->
	% 		 		wxListCtrl:setItemBackgroundColour(List, Item, ?ROW_BG_ODD)
	% 	end
	% end,
	% wx:foreach(Fun, lists:seq(0,length(L)-1)),

	wxListCtrl:connect(List, command_list_item_selected, []),

	wxPanel:setSizer(Panel, Sz),


  {Panel, #state{panel=Panel, frame=Parent, start=0, list=List, textctrl=Search, editor_pid=EditorPid}}.


%% =====================================================================
%% @doc OTP behaviour callbacks

%% Window close event
handle_event(#wx{event=#wxClose{}}, State) ->
  {stop, normal, State};

handle_event(#wx{event=#wxKey{type=key_down, keyCode=Kc, controlDown=Ctrl}}, State)
		when Ctrl =:= true orelse Kc =:= ?WXK_BACK orelse Kc =:= ?WXK_DELETE orelse
			 Kc =:= ?WXK_LEFT orelse Kc =:= ?WXK_RIGHT orelse Kc =:= ?WXK_ESCAPE ->
	{noreply, State};

handle_event(#wx{event=#wxKey{type=key_down, keyCode=?WXK_DOWN}},
	State=#state{list=ListCtrl, textctrl=Search, start=Start, search_str=Str}) ->
	Item = find_item(ListCtrl, Str, Start),
	select_item(ListCtrl, Item),
	wxTextCtrl:setValue(Search, wxListCtrl:getItemText(ListCtrl, Item)),
	wxTextCtrl:setInsertionPointEnd(Search),
	{noreply, State#state{start=Item+1}};

handle_event(#wx{event=#wxKey{type=key_down}},
	State=#state{list=ListCtrl, textctrl=Search, start=Start}) ->
	Str = wxTextCtrl:getValue(Search),
	StartIndex = case find_item(ListCtrl, Str, Start) of
		-1 -> -1;
		Item ->
			select_item(ListCtrl, Item),
			autofill(Search, Str, wxListCtrl:getItemText(ListCtrl, Item)),
			Item + 1
	end,
	{noreply, State#state{start=StartIndex, search_str=Str}};

handle_event(#wx{event=#wxCommand{type=command_text_enter, cmdString=Str}},
	State=#state{textctrl=Search, list=ListBox, editor_pid=Ed}) ->
	% send_to_editor(Str, Ed),
  % io:format("Symbol: ~p Line: ~p~n", [Str, undefined]),
	{noreply, State};

handle_event(#wx{event=#wxList{type=command_list_item_selected, itemIndex=Item}},
	State=#state{list=ListCtrl, editor_pid=Ed}) ->
	% send_to_editor(Str, Ed),
  % io:format("List selected: ~p~n", [Item]),
  % io:format("Symbol: ~p Line: ~p~n", [wxListCtrl:getItemText(ListCtrl, Item), wxListCtrl:getItemData(ListCtrl, Item)]),
	{noreply, State};

handle_event(#wx{event=#wxFocus{type=set_focus}}, State) ->
	wxTextCtrl:clear(State#state.textctrl),
	{noreply, State};

handle_event(#wx{event=#wxFocus{type=kill_focus}}, State) ->
	wxTextCtrl:setValue(State#state.textctrl, ?SEARCH_TEXT_DEFAULT),
	{noreply, State};

handle_event(Ev, State) ->
  io:format("Got Event ~p~n",[Ev]),
  {noreply,State}.

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
  wxPanel:destroy(Panel),
  ok.

% set(Items) ->
% 	{ok, {_,Pid}} = doc_manager:get_active_document(),
% 	ListCtrl = wx_object:call(?MODULE, {list, Pid}),
% 	wxListCtrl:deleteAllItems(ListCtrl),
% 	% insert_items(ListCtrl, Items),
% 	set_acc(ListCtrl, Items),
% 	ok.
	
set(Items) ->
	Ref = doc_manager:get_active_document_ref(),
	ListCtrl = wx_object:call(?MODULE, {list, Ref}),
	wxListCtrl:deleteAllItems(ListCtrl),
	% insert_items(ListCtrl, Items),
	set_acc(ListCtrl, Items),
	ok.

set_acc(ListCtrl, Items) ->
	lists:foldl(
	fun([Name], Acc) ->
		wxListCtrl:insertItem(ListCtrl, Acc, Name),
		set_item_background(ListCtrl, Acc),
		Acc+1
	end,
	0, Items).

set_item_background(ListCtrl, Item) ->
	case Item rem 2 of
	  0 ->
			wxListCtrl:setItemBackgroundColour(ListCtrl, Item, ?ROW_BG_EVEN);
	  _ ->
	 		wxListCtrl:setItemBackgroundColour(ListCtrl, Item, ?ROW_BG_ODD)
	end.

insert_items(ListCtrl, Items) ->
	Insert =
	fun([Name]) ->
		wxListCtrl:insertItem(ListCtrl, 0, Name)
	end,
	wx:foreach(Insert, Items).

send_to_editor(Str, Editor) ->
	editor:fn_list(Editor, Str),
	ok.

find_item(ListCtrl, Str, Start) ->
	wxListCtrl:findItem(ListCtrl, Start, Str, [{partial, true}]).

select_item(ListCtrl, Item) ->
	wxListCtrl:setItemState(ListCtrl, Item, ?wxLIST_STATE_SELECTED, ?wxLIST_STATE_SELECTED),
	wxListCtrl:ensureVisible(ListCtrl, Item).

autofill(Search, Str, FillStr) ->
	wxTextCtrl:setValue(Search, FillStr),
	wxTextCtrl:setSelection(Search, length(Str), -1),
	ok.
