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
%% @doc Displays the XRC based <em>Open Project</em> dialog.
%% Uses default CANCEL/close_window handlers.
%% @end
%% =====================================================================

-module(deverl_dlg_open_proj_wx).

-include_lib("wx/include/wx.hrl").

%% wx_object
-behaviour(wx_object).
-export([init/1, terminate/2, code_change/3,
	       handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).
				 
%% API
-export([
  new/2,
  set_focus/1,
  get_path/1,
  destroy/1
  ]).
	
%% Server state			 
-record(state, {dlg,
                path,
                list_ctrl
            	 }).


%% =====================================================================
%% Client API
%% =====================================================================

new(Parent, Projects) ->
  wx_object:start(?MODULE, {Parent, Projects}, []).

set_focus(This) ->
	wx_object:cast(This, setfocus).
	
get_path(This) ->
	wx_object:call(This, path).
	
destroy(This) ->
	wx_object:call(This, shutdown).
	
	
%% =====================================================================
%% Callback functions
%% =====================================================================
%% @hidden
init({Parent, Projects}) ->    
  Xrc = wxXmlResource:get(),
  Dlg = wxDialog:new(),
  deverl_lib_dlg_wx:win_variant(Dlg),
  wxXmlResource:loadDialog(Xrc, Dlg, Parent, "open_project"),

	ListCtrl = wxXmlResource:xrcctrl(Dlg, "listctrl", wxListCtrl),
    
  ListItem  = wxListItem:new(),
  Insert = fun(Header, Col) ->
	  wxListItem:setText(ListItem, Header),
	  wxListCtrl:insertColumn(ListCtrl, Col, ListItem),
	  Col + 1		  
  end,
  lists:foldl(Insert, 0, ["Project", "Last Modified", "Path"]),
  wxListItem:destroy(ListItem),
  
  insert_projects(ListCtrl, Projects),
  [wxListCtrl:setColumnWidth(ListCtrl, N, ?wxLIST_AUTOSIZE) || N <- lists:seq(0, wxListCtrl:getColumnCount(ListCtrl) - 1)],
 
  wxListCtrl:connect(ListCtrl, size, [{skip, true}]),
  wxListCtrl:connect(ListCtrl, command_list_item_selected),
  wxListCtrl:connect(ListCtrl, command_list_item_activated),

	State = #state{
		dlg=Dlg, 
    list_ctrl=ListCtrl
	},
    
	{Dlg, State}.

%% @hidden
handle_event(#wx{obj=ListCtrl, event=#wxList{type=command_list_item_selected, itemIndex=Idx}}, 
             State=#state{dlg=Dlg}) ->
  wxWindow:enable(wxWindow:findWindow(Dlg, ?wxID_OK)),
  {noreply, State#state{path=get_path_from_list(ListCtrl, Idx)}};

handle_event(#wx{obj=ListCtrl, event=#wxList{type=command_list_item_activated, itemIndex=Idx}}, 
             State=#state{dlg=Dialog}) ->
  wxDialog:endModal(Dialog, ?wxID_OK),
  {noreply, State#state{path=get_path_from_list(ListCtrl, Idx)}};

handle_event(#wx{event=#wxSize{size={Width0,_}}}, State = #state{list_ctrl=ListCtrl}) ->
  Width1 = wxListCtrl:getColumnWidth(ListCtrl, 0) + wxListCtrl:getColumnWidth(ListCtrl, 1),
  wxListCtrl:setColumnWidth(ListCtrl, 2, Width0 + Width1),
  {noreply, State}.
%% @hidden
handle_info(_Msg, State) ->
  {noreply,State}.
%% @hidden
handle_call(path, _From, State) ->
  {reply, State#state.path, State};
handle_call(shutdown, _From, State) ->
  {stop, normal, ok, State}.
%% @hidden
handle_cast(_, State) ->
  {noreply,State}.
%% @hidden
code_change(_, _, State) ->
  {stop, ignore, State}.
%% @hidden
terminate(_Reason, State) ->
  wxDialog:destroy(State#state.dlg),
	ok.
	

%% =====================================================================
%% Internal functions
%% =====================================================================

%% =====================================================================
%% @doc Add Projects to the listctrl.
	
insert_projects(ListCtrl, Projects) ->
  lists:foldl(
    fun(Path, Acc) ->
  		wxListCtrl:insertItem(ListCtrl, Acc, ""),
  		wxListCtrl:setItem(ListCtrl, Acc, 0, filename:basename(Path)),
  		wxListCtrl:setItem(ListCtrl, Acc, 1, deverl_lib_widgets:datetime_to_string(
        filelib:last_modified(Path))),
  		wxListCtrl:setItem(ListCtrl, Acc, 2, Path),
      deverl_lib_widgets:set_list_item_background(ListCtrl, Acc),
      Acc + 1
    end, 0, Projects).


%% =====================================================================
%% @doc Get the path from the record list at index Idx.

get_path_from_list(ListCtrl, Idx) ->
  ListItem = wxListItem:new(),
  wxListItem:setId(ListItem, Idx),
  wxListItem:setColumn(ListItem, 2),
  wxListItem:setMask(ListItem, ?wxLIST_MASK_TEXT),
  wxListCtrl:getItem(ListCtrl, ListItem),
  Path = wxListItem:getText(ListItem),
  wxListItem:destroy(ListItem),
  Path.