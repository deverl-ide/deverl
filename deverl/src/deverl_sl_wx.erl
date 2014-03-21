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
%% @doc Display the symbol list from the editor.
%% @end
%% =====================================================================

-module(deverl_sl_wx).

-include_lib("wx/include/wx.hrl").
-include("deverl.hrl").

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
	set/2
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


%% =====================================================================
%% Client API
%% =====================================================================

%% =====================================================================
%% @doc

start(Config) ->
	wx_object:start_link({local, ?MODULE}, ?MODULE, Config, []).


%% =====================================================================
%% @doc

set(Items, EditorRef) ->
	wx_object:cast(?MODULE, {set, Items, EditorRef}).


%% =====================================================================
%% Callback functions
%% =====================================================================
%% @hidden
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

	{W,_H} = wxListCtrl:getSize(List),
	wxListCtrl:setColumnWidth(List, 0, W),

	wxListCtrl:connect(List, command_list_item_activated, []),

	wxPanel:setSizer(Panel, Sz),


  {Panel, #state{panel=Panel, frame=Parent, start=0, list=List, textctrl=Search, editor_pid=EditorPid}}.


%% =====================================================================
%% @doc OTP behaviour callbacks
%% @hidden
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
             State=#state{editor_pid=Ed}) ->
  send_to_editor(Str, Ed),
	{noreply, State};

handle_event(#wx{event=#wxList{type=command_list_item_activated, itemIndex=Item}},
	           State=#state{list=ListCtrl, editor_pid=Ed}) ->
  Str =  wxListCtrl:getItemText(ListCtrl, Item),
  send_to_editor(Str, Ed),
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
%% @hidden
handle_info(Msg, State) ->
  io:format( "Got Info ~p~n",[Msg]),
  {noreply,State}.
%% @hidden
handle_call(Msg, _From, State) ->
  io:format("Got Call ~p~n",[Msg]),
  {reply,ok,State}.
%% @hidden
handle_cast({set, Symbols, Editor}, State) ->  
	wxListCtrl:deleteAllItems(State#state.list),
  Set = sets:from_list(Symbols), %% To remove dups
  % lists:usort(List). %% Removes dup and sorts
  insert_items(State#state.list, lists:reverse(sets:to_list(Set))),
  {noreply,State#state{editor_pid=Editor}};

handle_cast(Msg, State) ->
  io:format("Got cast ~p~n",[Msg]),
  {noreply,State}.
%% @hidden
code_change(_, _, State) ->
  {stop, ignore, State}.
%% @hidden
terminate(_Reason, #state{panel=Panel}) ->
  wxPanel:destroy(Panel),
  ok.


%% =====================================================================
%% Internal functions
%% =====================================================================

%% =====================================================================
%% @doc

set_acc(ListCtrl, Items) ->
	lists:foldl(
	fun([Name], Acc) ->
		wxListCtrl:insertItem(ListCtrl, Acc, Name),
		set_item_background(ListCtrl, Acc),
		Acc+1
	end,
	0, Items).


%% =====================================================================
%% @doc

set_item_background(ListCtrl, Item) ->
	case Item rem 2 of
	  0 ->
			wxListCtrl:setItemBackgroundColour(ListCtrl, Item, ?ROW_BG_EVEN);
	  _ ->
	 		wxListCtrl:setItemBackgroundColour(ListCtrl, Item, ?ROW_BG_ODD)
	end.


%% =====================================================================
%% @doc

insert_items(ListCtrl, Items) ->
	Insert =
	fun([Name]) ->
		wxListCtrl:insertItem(ListCtrl, 0, Name)
	end,
	wx:foreach(Insert, Items).


%% =====================================================================
%% @doc

send_to_editor(Str, Editor) ->
	deverl_editor_wx:go_to_symbol(Editor, Str),
	ok.


%% =====================================================================
%% @doc

find_item(ListCtrl, Str, Start) ->
	wxListCtrl:findItem(ListCtrl, Start, Str, [{partial, true}]).


%% =====================================================================
%% @doc

select_item(ListCtrl, Item) ->
	wxListCtrl:setItemState(ListCtrl, Item, ?wxLIST_STATE_SELECTED, ?wxLIST_STATE_SELECTED),
	wxListCtrl:ensureVisible(ListCtrl, Item).


%% =====================================================================
%% @doc

autofill(Search, Str, FillStr) ->
	wxTextCtrl:setValue(Search, FillStr),
	wxTextCtrl:setSelection(Search, length(Str), -1),
	ok.
