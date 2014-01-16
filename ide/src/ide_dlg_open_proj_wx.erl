%% =====================================================================
%% @author
%% @copyright
%% @title
%% @version
%% @doc The open project dlg.
%% @end
%% =====================================================================

-module(ide_dlg_open_proj_wx).

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
  wx_object:start({local, ?MODULE}, ?MODULE, {Parent, Projects}, []).

set_focus(This) ->
	wx_object:cast(This, setfocus).
	
get_path(This) ->
	wx_object:call(This, path).
	
destroy(This) ->
	wx_object:call(This, shutdown).
	
	
%% =====================================================================
%% Callback functions
%% =====================================================================

init({Parent, Projects}) ->    
  Xrc = wxXmlResource:get(),
  Dlg = wxDialog:new(),
  ide_lib_dlg_wx:win_var(Dlg),
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

handle_info(_Msg, State) ->
  {noreply,State}.

handle_call(path, _From, State) ->
  {reply, State#state.path, State};
handle_call(shutdown, _From, State) ->
  {stop, normal, ok, State}.

handle_cast(_, State) ->
  {noreply,State}.

code_change(_, _, State) ->
  {stop, ignore, State}.

terminate(_Reason, _State) ->
  % wxDialog:destroy(State#state.dlg),  %% segfault OSX wx3.0 erlR16B03 (see xrc sample directory)
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
  		wxListCtrl:setItem(ListCtrl, Acc, 1, ide_lib_widgets:datetime_to_string(
        filelib:last_modified(Path))),
  		wxListCtrl:setItem(ListCtrl, Acc, 2, Path),
      ide_lib_widgets:set_list_item_background(ListCtrl, Acc),
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