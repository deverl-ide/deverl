%% =====================================================================
%% @author
%% @copyright
%% @title
%% @version
%% @doc The open project dialog.
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
  start/2,
  set_focus/1,
  get_path/1,
  destroy/1
  ]).
			
%% inherited functions
-export([show/1, showModal/1]).
	
%% Server state			 
-record(state, {dialog,
                path,
                list_ctrl
            	 }).



%% =====================================================================
%% Client API
%% =====================================================================

start(Parent, Projects) ->
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

init(Config) ->
  wx:batch(fun() -> do_init(Config) end).

do_init({Parent, Projects}) ->
	Dialog = wxDialog:new(Parent, ?wxID_ANY, "Open Project", 
		[{size,{640,460}}, {style, ?wxDEFAULT_DIALOG_STYLE bor ?wxRESIZE_BORDER bor ?wxDIALOG_EX_METAL}]),
	wxDialog:centre(Dialog),
	
	%% Conditional compilation OSX
	case os:type() of
		{_, darwin} ->
			wxPanel:setWindowVariant(Dialog, ?wxWINDOW_VARIANT_SMALL);
		 _ -> ok
	end,
	
  LRSizer = wxBoxSizer:new(?wxHORIZONTAL),
  wxSizer:addSpacer(LRSizer, 20),

  VertSizer = wxBoxSizer:new(?wxVERTICAL),
  
	%% Header
  wxSizer:addSpacer(VertSizer, 40),
	wxSizer:add(VertSizer, wxStaticText:new(Dialog, ?wxID_ANY, "Open Project"), []),
	wxSizer:addSpacer(VertSizer, 5),
  wxSizer:add(VertSizer, wxStaticLine:new(Dialog, [{style, ?wxLI_HORIZONTAL}]), 
              [{flag, ?wxEXPAND}]),
  wxSizer:addSpacer(VertSizer, 20),
	
	%% Project list
	wxSizer:add(VertSizer, wxStaticText:new(Dialog, ?wxID_ANY, "Select a project:"), []),
	wxSizer:addSpacer(VertSizer, 5),  
  
	ListCtrl = wxListCtrl:new(Dialog,[{style, ?wxLC_REPORT}]),
  
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
  
  wxSizer:add(VertSizer, ListCtrl, [{flag, ?wxEXPAND}, {proportion, 1}]),
  wxSizer:addSpacer(VertSizer, 40),
  wxSizer:add(VertSizer, wxStaticLine:new(Dialog, [{style, ?wxLI_HORIZONTAL}]), 
              [{flag, ?wxEXPAND}]),
	wxSizer:addSpacer(VertSizer, 20),
  
  %% Buttons
  ButtonSz = wxBoxSizer:new(?wxHORIZONTAL),
	wxSizer:addStretchSpacer(ButtonSz),
  wxSizer:add(ButtonSz, wxButton:new(Dialog, ?wxID_CANCEL, [{label, "Cancel"}]), [{proportion, 0}]),
	wxSizer:addSpacer(ButtonSz, 10),  
	Open = wxButton:new(Dialog, ?wxID_OK, [{label, "Open"}]),
  wxSizer:add(ButtonSz, Open, [{proportion, 0}]),
	wxButton:disable(Open),
	wxSizer:add(VertSizer, ButtonSz, [{flag, ?wxEXPAND}, {proportion, 0}]),   
	wxSizer:addSpacer(VertSizer, 20),
  
  wxSizer:add(LRSizer, VertSizer, [{proportion, 1}, {flag, ?wxEXPAND}]),
  wxSizer:addSpacer(LRSizer, 20),
	
  wxPanel:setSizer(Dialog, LRSizer),
	wxSizer:layout(LRSizer),
	  
	State = #state{
		dialog=Dialog, 
    list_ctrl=ListCtrl
	},
  
	{Dialog, State}.


handle_event(#wx{obj=ListCtrl, event=#wxList{type=command_list_item_selected, itemIndex=Idx}}, 
             State=#state{dialog=Dialog}) ->
  wxWindow:enable(wxWindow:findWindow(Dialog, ?wxID_OK)),
  {noreply, State#state{path=get_path_from_list(ListCtrl, Idx)}};

handle_event(#wx{obj=ListCtrl, event=#wxList{type=command_list_item_activated, itemIndex=Idx}}, 
             State=#state{dialog=Dialog}) ->
  wxDialog:endModal(Dialog, ?wxID_OK),
  {noreply, State#state{path=get_path_from_list(ListCtrl, Idx)}};

handle_event(#wx{event=#wxSize{size={Width0,_}}}, State = #state{list_ctrl=ListCtrl}) ->
  Width1 = wxListCtrl:getColumnWidth(ListCtrl, 0) + wxListCtrl:getColumnWidth(ListCtrl, 1),
  wxListCtrl:setColumnWidth(ListCtrl, 2, Width0 + Width1),
  {noreply, State}.

handle_info(Msg, State) ->
  io:format( "Got Info ~p~nMsg:~p",[State, Msg]),
  {noreply,State}.

handle_call(path, _From, State=#state{path=Path}) ->
  {reply, Path, State};
handle_call(shutdown, _From, State) ->
  {stop, normal, ok, State}.


handle_cast(_, State) ->
  {noreply,State}.

code_change(_, _, State) ->
  {stop, ignore, State}.

terminate(_Reason, #state{dialog=Dialog}) ->
	wxDialog:destroy(Dialog),
	ok.
	

%% =====================================================================
%% Internal functions
%% =====================================================================

%% =====================================================================
%% @doc
	
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
  
  
%% =====================================================================
%% @doc
%% @hidden
show(This) -> wxDialog:show(This).
%% @hidden
showModal(This) -> wxDialog:showModal(This).