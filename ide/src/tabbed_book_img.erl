%% =====================================================================
%% @author
%% @copyright
%% @title
%% @version
%% @doc 
%% @end
%% =====================================================================

-module(tabbed_book_img).

-include_lib("wx/include/wx.hrl").

%% wx_object
-behaviour(wx_object).
-export([
	 init/1, terminate/2,  code_change/3,
	 handle_info/2, handle_call/3, handle_cast/2, 
	 handle_event/2, handle_sync_event/3]).
	
%% API 
-export([
	new/1,
	add_page/4,
	assign_image_list/2,
	set_selection/2
]).

%% Server state
-record(state, {tabs, 
								content,
								image_list,
								active_btn,
								pages
}).


%% =====================================================================
%% Client API
%% =====================================================================

new(Config) ->
	wx_object:start_link(?MODULE, Config, []).  
	

%% =====================================================================
%% @doc Add a page.	

add_page(This, Page, Text, Options) ->
	wx_object:cast(This, {add_page, {Page, Text, Options}}).


%% =====================================================================
%% @doc Assign an image list to the control.

assign_image_list(This, ImgList) ->
	wx_object:cast(This, {image_list, ImgList}).
	

%% =====================================================================
%% @doc Set the selected page.		

set_selection(This, Index) ->
	wx_object:cast(This, {set_selection, Index}).


%% =====================================================================
%% Callback functions
%% =====================================================================

init(Options) ->
	Parent = proplists:get_value(parent, Options),
	
	MainPanel = wxPanel:new(Parent, []),
	MainSz = wxBoxSizer:new(?wxVERTICAL),
	wxPanel:setSizer(MainPanel, MainSz),
		
	Tabs = wxPanel:new(MainPanel),
  wxWindow:setBackgroundColour(Tabs, lib_widgets:colour_shade(wxSystemSettings:getColour(?wxSYS_COLOUR_WINDOW), 0.8)),
	Sz = wxBoxSizer:new(?wxHORIZONTAL),
	wxPanel:setSizer(Tabs, Sz),
	wxSizer:add(MainSz, Tabs, [{proportion, 0}, {flag, ?wxEXPAND}]),
	
	Content = wxPanel:new(MainPanel),
	ContentSz = wxBoxSizer:new(?wxVERTICAL),
	wxPanel:setSizer(Content, ContentSz),
	wxSizer:add(MainSz, Content, [{proportion, 1}, {flag, ?wxEXPAND}]),
	
	State=#state{tabs={Tabs, Sz},
							 content={Content, ContentSz},
							 pages=[]
	},
    
  {MainPanel, State}.


handle_info(Msg, State) ->
  io:format("Got Info ~p~n",[Msg]),
  {noreply,State}.

handle_call(Msg, _From, State) ->
  io:format("Got Call ~p~n",[Msg]),
  {reply,ok,State}.

handle_cast({add_page, {Page, Text, Options}}, 
						State=#state{tabs={TabPanel, TabSz}, content={Content, ContentSz}, pages=Pages,
												 image_list=ImageList}) ->
	{Button, Label} = create_button(TabPanel, TabSz, Text, Options),
	SzFlags = wxSizerFlags:new([{proportion, 1}]),
	wxSizerFlags:expand(SzFlags),
	wxWindow:reparent(Page, Content),
	wxWindow:hide(Page),
	wxSizer:add(ContentSz, Page, SzFlags),
	UpdatedPages = [{Button, {Page, Options}} | Pages],
	wxPanel:layout(TabPanel),
	Tip = wxToolTip:new(Label),
	wxWindow:setToolTip(Button, Tip),
  {noreply,State#state{pages=UpdatedPages}};
handle_cast({set_selection, Index}, 
						State=#state{tabs={Tabs, _}, active_btn=ActiveBtn, pages=Pages, content={Cont, _}}) ->
	NewActiveBtn = try
		{Button, _} = lists:nth(Index, lists:reverse(Pages)),
		change_selection(ActiveBtn, Button, Pages, Tabs, Cont),
		Button
	catch
		_:_ -> error("Page doesn't exist"), undefined
	end,
  {noreply,State#state{active_btn=NewActiveBtn}};
handle_cast({image_list, ImgList}, State) ->
	{noreply, State#state{image_list=ImgList}};
handle_cast(Msg, State) ->
  io:format("Got cast ~p~n",[Msg]),
  {noreply,State}.


%% =====================================================================
%% Callback Sync event handling
%% =====================================================================
%% Sync events i.e. from callbacks must return ok, it can not return a new state.
%% Do the redrawing here.
handle_sync_event(#wx{obj=Btn, userData={Label, Options}, event=#wxPaint{}}, _,
		  						#state{tabs={Panel,_Sz}, active_btn=ActiveBtn, image_list=ImageList}) ->
  Bg = wxWindow:getBackgroundColour(Panel),
	case ActiveBtn of
		Btn -> draw(Btn, Label, ImageList, wxPaintDC, Bg, [{active, true} | Options]);
		% undefined -> draw(Btn, Label, wxPaintDC, ?BUTTON_ACTIVE, [{active, true}]);
		_ -> draw(Btn, Label, ImageList, wxPaintDC, Bg, Options)
	end,
	ok.
	
% handle_event(#wx{obj=Btn, userData={Label, Options}, event=#wxMouse{type=enter_window}}, 
% 						 State=#state{active_btn=ActiveBtn, image_list=ImageList, tabs={Tabs, _}}) ->
% 	trigger_tab_paint(Tabs, ActiveBtn, Btn),
% 	{noreply, State};
% handle_event(#wx{obj=Btn, userData={Label, Options}, event=#wxMouse{type=leave_window}}, 
% 						 State=#state{active_btn=ActiveBtn, image_list=ImageList, tabs={Tabs, _}}) ->
% 	trigger_tab_paint(Tabs, ActiveBtn, Btn),	
% 	{noreply, State};
handle_event(#wx{obj=Btn, userData={Label, Options}, event=#wxMouse{type=left_down}}, 
						 State=#state{pages=Pages, active_btn=ActiveBtn, tabs={Tabs, _}, content={Cont,_}, 
						 							image_list=ImageList}) ->
	change_selection(ActiveBtn, Btn, Pages, Tabs, Cont),
	{noreply, State#state{active_btn=Btn}};
handle_event(#wx{event=#wxClose{}}, State) ->
	io:format("~p Closing window ~n",[self()]),
	{stop, normal, State};
handle_event(#wx{id=Id, userData=Ud, obj=Obj}=E,State) ->
  case Id of
	  ?wxID_EXIT ->
	    {stop, normal, State};
	  _ ->
			io:format("Id: ~p~nObj: ~p~nUserData: ~p~n", [Id, Obj, Ud]),
	    {noreply, State}
  end.

code_change(_, _, State) ->
    {stop, not_yet_implemented, State}.

terminate(_Reason, _State) ->
    wx:destroy().


%% =====================================================================
%% Internal functions
%% =====================================================================		

%% =====================================================================
%% @doc Create button.
	
create_button(Parent, Sz, Label, Options) ->
	SzFlags = wxSizerFlags:new([{proportion, 0}]),
	wxSizerFlags:right(SzFlags),
  % Btn = wxWindow:new(Parent, ?wxID_ANY, [{size,{34,34}}, {style, ?wxFULL_REPAINT_ON_RESIZE}]),
	Btn = wxWindow:new(Parent, ?wxID_ANY, [{size,{34,34}}]), %% TESTING
	wxSizer:add(Sz, Btn, SzFlags),
  wxPanel:connect(Btn, paint, [callback, {userData, {Label, Options}}]),
	wxPanel:connect(Btn, left_down, [{skip, true}, {userData, {Label, Options}}]),
	% wxPanel:connect(Btn, enter_window, [{skip, true}, {userData, {Label, Options}}]),
	% wxPanel:connect(Btn, leave_window, [{skip, true}, {userData, {Label, Options}}]),
	{Btn, Label}.
	
	
%% =====================================================================
%% @doc Draw the tab graphic.		
%% Default {active, false}
draw(Btn, Label, ImageList, WxDc, Bg, Options) ->
  wxWindow:setBackgroundColour(Btn, Bg),
  {W,H} = wxWindow:getSize(Btn),

  % Following colour must match bg of tb panel
  Blend = Bg,
  FillD = {190,190,190}, %% Deselected button fill
  FillA = {170,170,170}, %% Selected
  LineD = {155,155,155}, %% Deselected buttun lines (sides)
  LineA = {100,100,100},

	%% wxDC must be created in a callback to work on windows.
	DC = WxDc:new(Btn),

	%% Draw graphics
  Draw = fun(C1, C2, Blend) ->
    wxDC:gradientFillLinear(DC, {2,6,W-5,11}, Blend, C1, [{nDirection, ?wxDOWN}]),
    wxDC:gradientFillLinear(DC, {2,17,W-5,11}, C1, Blend, [{nDirection, ?wxDOWN}]),

    wxDC:gradientFillLinear(DC, {2,4,1,13}, Blend, C2, [{nDirection, ?wxDOWN}]),
    wxDC:gradientFillLinear(DC, {2,17,1,14}, C2, Blend, [{nDirection, ?wxDOWN}]),    

    wxDC:gradientFillLinear(DC, {W-3,4,1,13}, Blend, C2, [{nDirection, ?wxDOWN}]),
    wxDC:gradientFillLinear(DC, {W-3,17,1,14}, C2, Blend, [{nDirection, ?wxDOWN}])
  end,
  
  case proplists:get_value(active, Options, false) of
    false -> 
      % Draw(FillD, LineD, Blend),
      ok;
    true ->   
      Draw(FillA, LineA, Blend)
  end,
	
	case proplists:get_value(imageId, Options) of
		undefined -> 
			ok;
		Id -> 
			Bitmap = wxImageList:getBitmap(ImageList, Id),
			wxDC:drawBitmap(DC, Bitmap, {9,9}, [{useMask, true}]),
			ok
	end,
	
	%% Nothing is drawn until wxDC is destroyed.
	WxDc:destroy(DC),
	ok.
	
	
%% =====================================================================
%% @doc Trigger a paint event for the tabs, if required.
%% @private

trigger_tab_paint(Tabs, ActiveBtn, Btn) ->
 	case ActiveBtn of
 		Btn -> ok;
 		_ -> %% Trigger a paint event
			wxWindow:refresh(Tabs), 
			wxWindow:update(Tabs)
	end.
	
	
%% =====================================================================
%% @doc Get the page associated to a button.
%% @private

get_page(Pages, Button) -> 
	{Page, _} = proplists:get_value(Button, Pages),
	Page.
	
	
%% =====================================================================
%% @doc Change the selected page.
%% Swaps contents, triggers a paint event.
%% @private
change_selection(B, B, _, _, _) -> ok;
change_selection(ActiveButton, NewButton, Pages, Tabs, Cont) ->
	case ActiveButton of
		undefined -> ok;
		_ -> wxWindow:hide(get_page(Pages, ActiveButton))
	end,
	Page = get_page(Pages, NewButton),
	wxWindow:show(Page),
	wxPanel:layout(Cont),
	wxPanel:refresh(Tabs),
	wxPanel:update(Tabs),
	ok.