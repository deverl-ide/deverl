%% editor_loader.erl
%% Simple frame that loads an editor object

-module(tabbed_book).

-export([
	init/1, terminate/2,  code_change/3,
	handle_info/2, handle_call/3, handle_cast/2, 
	handle_event/2, handle_sync_event/3]).
	 
-export([
	new/1, 
	add_page/3,
	set_selection/2]).

-include_lib("wx/include/wx.hrl").

-behaviour(wx_object).

%% The record containing the State.
-record(state, {tabs, 
								content,
								active_btn,
								pages,
								hover
}).

-define(SYS_BG, wxSystemSettings:getColour(?wxSYS_COLOUR_BACKGROUND)).


new(Config) ->
	wx_object:start_link(?MODULE, Config, []).  

init(Options) ->
	Parent = proplists:get_value(parent, Options),
	
	MainPanel = wxPanel:new(Parent, []),
	MainSz = wxBoxSizer:new(?wxHORIZONTAL),
	wxPanel:setSizer(MainPanel, MainSz),
	wxSizer:addSpacer(MainSz, 4),
	
	%% Tab area
	Tabs = wxPanel:new(MainPanel, []),
	wxPanel:connect(Tabs, paint, [callback, {userData, tab_panel}]),
	Font = wxPanel:getFont(Tabs),
	wxFont:setPointSize(Font, 11),
	wxPanel:setFont(Tabs, Font),
	wxPanel:setWindowVariant(Tabs, ?wxWINDOW_VARIANT_SMALL),
	Sz = wxBoxSizer:new(?wxVERTICAL),
	wxPanel:setSizer(Tabs, Sz),
	wxSizer:add(MainSz, Tabs, [{proportion, 0}, {flag, ?wxEXPAND}]),
	
	%% Content area
	Content = wxPanel:new(MainPanel, []),
	ContentSz = wxBoxSizer:new(?wxVERTICAL),
	wxPanel:setSizer(Content, ContentSz),
	wxSizer:add(MainSz, Content, [{proportion, 1}, {flag, ?wxEXPAND}]),
	
	State=#state{tabs={Tabs, Sz},
							 content={Content, ContentSz},
							 pages=[]
	},
    
  {MainPanel, State}.
	

%%%%% Callbacks %%%%%
handle_info(Msg, State) ->
  io:format("Got Info ~p~n",[Msg]),
  {noreply,State}.

handle_call(Msg, _From, State) ->
  io:format("Got Call ~p~n",[Msg]),
  {reply,ok,State}.

handle_cast({add_page, {Page, Text}}, 
						State=#state{tabs={TabPanel, TabSz}, content={Content, ContentSz}, pages=Pages}) ->
	{Button, Label} = create_button(TabPanel, TabSz, Text),
	SzFlags = wxSizerFlags:new([{proportion, 1}]),
	wxSizerFlags:expand(SzFlags),
	wxWindow:reparent(Page, Content),
	wxWindow:hide(Page),
	wxSizer:add(ContentSz, Page, SzFlags),
	UpdatedPages = [{Button, {Label, Page}} | Pages],
	wxPanel:layout(TabPanel),
  {noreply,State#state{pages=UpdatedPages}};
handle_cast({set_selection, Index}, 
						State=#state{tabs={Tabs, _}, content={Cont, _}, pages=Pages,
												 active_btn=ActiveButton}) ->
	NewActiveBtn = try
		{Button, _} = lists:nth(Index, lists:reverse(Pages)),
		change_selection(ActiveButton, Button, Pages, Tabs, Cont),
		Button
	catch
		_:_ -> error("Page doesn't exist"), undefined
	end,
  {noreply,State#state{active_btn=NewActiveBtn}};
handle_cast(Msg, State) ->
  io:format("Got cast ~p~n",[Msg]),
  {noreply,State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Sync events i.e. from callbacks must return ok, it can not return a new state.
%% Do the redrawing here.
handle_sync_event(#wx{obj=TabPanel, userData=tab_panel, event=#wxPaint{}},_,_State) ->
	{W,H} = wxWindow:getSize(TabPanel),
	DC = wxPaintDC:new(TabPanel),
	wxDC:gradientFillLinear(DC, {0,0,W,H}, ?SYS_BG, {199,199,199}, [{nDirection, ?wxRIGHT}]),
	wxPaintDC:destroy(DC),
	ok;
handle_sync_event(#wx{obj=Btn, userData=Label, event=#wxPaint{}},_B,
		  						#state{pages=Pages, active_btn=ActiveBtn, hover=Hover}) ->
	{FirstButton, _} = hd(lists:reverse(Pages)), %% This button is drawn slightly differently
	Options = [],
	Options2 = case ActiveBtn of
		Btn -> [{button_state, active} | Options];
		_ when Hover =:= Btn -> [{button_state, hover} | Options];
		_ -> Options %% Draw normal
	end,
	Options3 = case Btn of
		FirstButton -> [{first, true} | Options2];
		_ -> Options2
	end,
	draw(Btn, Label, wxPaintDC, Options3),
	ok.

handle_event(#wx{obj=Btn, event=#wxMouse{type=enter_window}}, 
						 State=#state{tabs={Tabs,_}, active_btn=ActiveBtn}) ->
	case ActiveBtn of
		Btn -> ok;
		_ -> %% Trigger a paint event
			wxWindow:refresh(Tabs), 
			wxWindow:update(Tabs)
	end,
	{noreply, State#state{hover=Btn}};
handle_event(#wx{obj=Btn, event=#wxMouse{type=leave_window}}, 
						 State=#state{tabs={Tabs,_}, active_btn=ActiveBtn}) ->
 	case ActiveBtn of
 		Btn -> ok;
 		_ -> %% Trigger a paint event
			wxWindow:refresh(Tabs), 
			wxWindow:update(Tabs)
	end,
	{noreply, State#state{hover=undefined}};
handle_event(#wx{obj=Btn, event=#wxMouse{type=left_down}}, 
						 State=#state{pages=Pages, tabs={Tabs,_}, active_btn=ActiveBtn, content={Cont,_}}) ->
	change_selection(ActiveBtn, Btn, Pages, Tabs, Cont),
	{noreply, State#state{active_btn=Btn}};
handle_event(#wx{event=#wxClose{}}, State) ->
	io:format("~p Closing window ~n",[self()]),
	{stop, normal, State};
handle_event(#wx{id=Id}=E,State) ->
  case Id of
	  ?wxID_EXIT ->
	    {stop, normal, State};
	  _ ->
			io:format("EVENT: ~p~n", [E]),
	    {noreply, State}
  end.

code_change(_, _, State) ->
    {stop, not_yet_implemented, State}.

terminate(_Reason, _State) ->
    wx:destroy().
	
	
%% =====================================================================
%% @doc Add a page.
		
add_page(This, Page, Text) ->
	wx_object:cast(This, {add_page, {Page, Text}}).
	
set_selection(This, Index) ->
	wx_object:cast(This, {set_selection, Index}).
	
	
%% =====================================================================
%% @doc Create a new button/tab.
%% @private

create_button(Parent, Sz, Label) ->
	SzFlags = wxSizerFlags:new([{proportion, 0}]),
	wxSizerFlags:right(SzFlags),
	Btn = wxWindow:new(Parent, ?wxID_ANY, [{size,{64,24}}, {style, ?wxFULL_REPAINT_ON_RESIZE}]),
	wxSizer:add(Sz, Btn, SzFlags),
  wxPanel:connect(Btn, paint, [callback, {userData, Label}]),
	wxPanel:connect(Btn, left_down, [{skip, true}]),
	wxPanel:connect(Btn, enter_window, [{skip, true}]),
	wxPanel:connect(Btn, leave_window, [{skip, true}]),
	{Btn, Label}.
	
	
%% =====================================================================
%% @doc Draw the tab graphic.
%% Options
%% Default {active, false}

draw(Btn, Label, WxDc, Options) ->
	{W,H} = wxWindow:getSize(Btn),

	%% wxDC must be created in a callback to work on windows.
	DC = WxDc:new(Btn),
	
	Canvas = case proplists:get_value(button_state, Options) of
		active -> draw_setup(DC, {100,100,100}, {100,100,100}, ?wxWHITE);
		hover -> draw_setup(DC, {160,160,160}, {180,180,180}, {50, 50, 50});
		_ -> draw_setup(DC, {160,160,160}, ?SYS_BG, {50, 50, 50})
	end,
	
	% Corner radius
	Radius = 4.0,

	MinY = case proplists:get_value(first, Options) of
		true -> 0;
		_ 	 -> -1
	end,
	Path = wxGraphicsContext:createPath(Canvas),
	wxGraphicsPath:moveToPoint(Path, {W-1, MinY}),
	wxGraphicsPath:addLineToPoint(Path, {Radius, MinY}),
	wxGraphicsPath:addArcToPoint(Path, 0, 0, 0, Radius, Radius),
	wxGraphicsPath:addLineToPoint(Path, {0, H-Radius}),
	wxGraphicsPath:addArcToPoint(Path, 0, H-1, Radius, H-1, Radius),
	wxGraphicsPath:addLineToPoint(Path, {W-1, H-1}),
	wxGraphicsPath:closeSubpath(Path),
	wxGraphicsContext:drawPath(Canvas, Path),

	%% Add the label at the centre
	{TW,TH,_Desc,_Lead} = wxGraphicsContext:getTextExtent(Canvas, Label),
	TextHPos = (W/2) - (TW/2),
	TextVPos = (H/2) - (TH/2) - 1,
	wxGraphicsContext:drawText(Canvas, Label, TextHPos, TextVPos),

	wxGraphicsContext:destroy(Canvas),
	%% Nothing is drawn until wxDC is destroyed.
	WxDc:destroy(DC),
	ok.
	
	
%% =====================================================================
%% @doc Prepare canvas for drawing.
%% @private
	
draw_setup(DC, StrokeColour, BrushColour, FontColour) ->
	Pen = wxPen:new(StrokeColour, [{width, 1}]),
	Brush = wxBrush:new(BrushColour),
	Font = wxSystemSettings:getFont(?wxSYS_SYSTEM_FONT),
	wxFont:setPointSize(Font, 11),
		
	Canvas = wxGraphicsContext:create(DC),
	wxGraphicsContext:setPen(Canvas, Pen),
	wxGraphicsContext:setBrush(Canvas, Brush),
	wxGraphicsContext:setFont(Canvas, Font, FontColour),
	Canvas.
	
	
%% =====================================================================
%% @doc Get the label associated to a button.
%% @private

get_label(Pages, Button) -> 
	{Label, _} = proplists:get_value(Button, Pages),
	Label.
	
	
%% =====================================================================
%% @doc Get the page associated to a button.
%% @private

get_page(Pages, Button) -> 
	{_, Page} = proplists:get_value(Button, Pages),
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