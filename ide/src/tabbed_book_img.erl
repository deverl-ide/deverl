%% editor_loader.erl
%% Simple frame that loads an editor object

-module(tabbed_book_img).

-export([start/1,
	 init/1, terminate/2,  code_change/3,
	 handle_info/2, handle_call/3, handle_cast/2, 
	 handle_event/2, handle_sync_event/3]).
	 
-export([add_page/4]).

-include_lib("wx/include/wx.hrl").

-behaviour(wx_object).

%% The record containing the State.
-record(state, {tabs, 
								content,
								active_btn,
								pages
}).

-define(BUTTON_NORMAL, {?wxWHITE, {150,150,150}}).
-define(BUTTON_HOVER, {?wxWHITE, {200,200,200}}).
-define(BUTTON_ACTIVE, {wxSystemSettings:getColour(?wxSYS_COLOUR_BACKGROUND), wxSystemSettings:getColour(?wxSYS_COLOUR_BACKGROUND)}).
-define(TAB_PANEL_BG, {100,100,100}).
-define(SYS_BG, wxSystemSettings:getColour(?wxSYS_COLOUR_BACKGROUND)).


start(Config) ->
	wx_object:start_link(?MODULE, Config, []).  

init(Options) ->
	Parent = proplists:get_value(parent, Options),
	
	MainPanel = wxPanel:new(Parent, []),
	% wxPanel:setBackgroundColour(MainPanel, {160,160,160}),
	MainSz = wxBoxSizer:new(?wxVERTICAL),
	wxPanel:setSizer(MainPanel, MainSz),
	
		% wxSizer:addSpacer(MainSz, 2),
	
	% Tabs = wxPanel:new(MainPanel, [{size, {74,-1}}]),
	Tabs = wxPanel:new(MainPanel, []),
	wxPanel:setBackgroundColour(Tabs, ?SYS_BG),
	Font = wxPanel:getFont(Tabs),
	wxFont:setPointSize(Font, 9),
	wxPanel:setFont(Tabs, Font),
	wxPanel:setWindowVariant(Tabs, ?wxWINDOW_VARIANT_SMALL),
	Sz = wxBoxSizer:new(?wxHORIZONTAL),
	wxPanel:setSizer(Tabs, Sz),
	wxSizer:addSpacer(Sz, 4),
	wxSizer:add(MainSz, Tabs, [{proportion, 0}, {flag, ?wxEXPAND}]),
	% wxSizer:addSpacer(MainSz, 1),
	
	Content = wxPanel:new(MainPanel),
	ContentSz = wxBoxSizer:new(?wxVERTICAL),
	wxPanel:setSizer(Content, ContentSz),
	wxSizer:add(MainSz, Content, [{proportion, 1}, {flag, ?wxEXPAND}]),
	
	State=#state{tabs={Tabs, Sz},
							 % active_btn=ActiveBtn,
							 content={Content, ContentSz},
							 pages=[]
	},
    
  {MainPanel, State}.
	
add_page(This, Page, Text, Options) ->
	wx_object:cast(This, {add_page, {Page, Text, Options}}),
	ok.
	
create_button(Parent, Sz, Label, Options) ->
	SzFlags = wxSizerFlags:new([{proportion, 0}]),
	wxSizerFlags:right(SzFlags),
	Btn = wxWindow:new(Parent, ?wxID_ANY, [{size,{34,34}}, {style, ?wxFULL_REPAINT_ON_RESIZE}]),
	wxSizer:add(Sz, Btn, SzFlags),
  wxPanel:connect(Btn, paint, [callback, {userData, {Label, Options}}]),
	wxPanel:connect(Btn, left_down, [{skip, true}, {userData, {Label, Options}}]),
	% wxPanel:connect(Btn, enter_window, [{skip, true}, {userData, {Label, Options}}]),
	% wxPanel:connect(Btn, leave_window, [{skip, true}, {userData, {Label, Options}}]),
	{Btn, Label}.

%%%%% Callbacks %%%%%
handle_info(Msg, State) ->
  io:format("Got Info ~p~n",[Msg]),
  {noreply,State}.

handle_call(Msg, _From, State) ->
  io:format("Got Call ~p~n",[Msg]),
  {reply,ok,State}.

handle_cast({add_page, {Page, Text, Options}}, 
						State=#state{tabs={TabPanel, TabSz}, content={Content, ContentSz}, pages=Pages}) ->
	{Button, Label} = create_button(TabPanel, TabSz, Text, Options),
	SzFlags = wxSizerFlags:new([{proportion, 1}]),
	wxSizerFlags:expand(SzFlags),
	wxWindow:reparent(Page, Content),
	wxWindow:hide(Page),
	wxSizer:add(ContentSz, Page, SzFlags),
	UpdatedPages = [{Button, Page} | Pages],
	% wxPanel:layout(Content),
	wxPanel:layout(TabPanel),
	Tip = wxToolTip:new(Label),
	wxWindow:setToolTip(Button, Tip),
  {noreply,State#state{pages=UpdatedPages}};
handle_cast(Msg, State) ->
  io:format("Got cast ~p~n",[Msg]),
  {noreply,State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Sync events i.e. from callbacks must return ok, it can not return a new state.
%% Do the redrawing here.
handle_sync_event(#wx{obj=Btn, userData={Label, Options}, event=#wxPaint{}}=A,B,
		  						#state{tabs=Panel, active_btn=ActiveBtn}) ->
	case ActiveBtn of
		{Btn,_, _} -> draw(Btn, Label, wxPaintDC, ?BUTTON_ACTIVE, [{active, true} | Options]);
		% undefined -> draw(Btn, Label, wxPaintDC, ?BUTTON_ACTIVE, [{active, true}]);
		_ -> draw(Btn, Label, wxPaintDC, ?BUTTON_NORMAL, Options)
	end,
	ok.
	
handle_event(#wx{obj=Btn, userData={Label, Options}, event=#wxMouse{type=enter_window}}, 
						 State=#state{active_btn=ActiveBtn}) ->
	case ActiveBtn of
		{Btn,_, _} -> ok;
		_ -> draw(Btn, Label, wxClientDC, ?BUTTON_HOVER, Options)
		end,
	{noreply, State};
handle_event(#wx{obj=Btn, userData={Label, Options}, event=#wxMouse{type=leave_window}}, 
						 State=#state{active_btn=ActiveBtn}) ->
 	case ActiveBtn of
 		{Btn,_, _} -> ok;
 		_ -> draw(Btn, Label, wxClientDC, ?BUTTON_NORMAL, Options)		
	end,
	{noreply, State};
handle_event(#wx{obj=Btn, userData={Label, Options}, event=#wxMouse{type=left_down}}, 
						 State=#state{pages=Pages, active_btn=ActiveBtn, content={Cont,_}}) ->
	case ActiveBtn of
		{Btn, _, _} -> ok;
		undefined -> 
			draw(Btn, Label, wxClientDC, ?BUTTON_ACTIVE, [{active, true} | Options]);
		{Ab,ActiveLabel, ActiveOptions} ->
			draw(Btn, Label, wxClientDC, ?BUTTON_ACTIVE, [{active, true} | Options]),
			%% Deselect/hide the previous button/window	
			wxWindow:hide(proplists:get_value(Ab, Pages)),
			draw(Ab, ActiveLabel, wxClientDC, ?BUTTON_NORMAL, ActiveOptions)
	end,
	Content = proplists:get_value(Btn, Pages),
	wxWindow:show(Content),
	wxPanel:layout(Cont),
	{noreply, State#state{active_btn={Btn,Label,Options}}};
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
		
-define(LINE_L, {155,155,155}).
-define(LINE_H, {100,100,100}).
-define(FILL, {190,190,190}).
		
%% Options
%% Default {active, false}
draw(Btn, Label, WxDc, {Grad1, Grad2}, Options) ->
	{W,H} = wxWindow:getSize(Btn),
	
	SysBg = wxSystemSettings:getColour(?wxSYS_COLOUR_BACKGROUND),
	wxWindow:setBackgroundColour(Btn, SysBg),
	%% wxDC must be created in a callback to work on windows.
	DC = WxDc:new(Btn),
	
	Pen = wxPen:new(SysBg, [{width, 1}]),
	wxDC:setPen(DC, Pen),
	Brush = wxBrush:new(SysBg),
	wxDC:setBrush(DC, Brush),
	wxDC:drawRectangle(DC, {0, 0}, {W, H}),

	%% Add gradients if button is selected
	Fg = case proplists:get_value(active, Options, false) of
		true -> 
			wxDC:gradientFillLinear(DC, {2,6,W-5,11}, SysBg, ?FILL, [{nDirection, ?wxDOWN}]),
			wxDC:gradientFillLinear(DC, {2,17,W-5,11}, ?FILL, SysBg, [{nDirection, ?wxDOWN}]),

			wxDC:gradientFillLinear(DC, {2,6,1,11}, SysBg, ?LINE_L, [{nDirection, ?wxDOWN}]),
			wxDC:gradientFillLinear(DC, {2,17,1,11}, ?LINE_L, SysBg, [{nDirection, ?wxDOWN}]),
	
			wxDC:gradientFillLinear(DC, {W-4,6,1,11}, SysBg, ?LINE_L, [{nDirection, ?wxDOWN}]),
			wxDC:gradientFillLinear(DC, {W-4,17,1,11}, ?LINE_L, SysBg, [{nDirection, ?wxDOWN}]);
		false -> 	
			wxDC:drawLine(DC, {W-1, 0}, {W-1, H}),
			{60,60,60}
	end,
	
	%% Add image
	% Bitmap = wxBitmap:new(wxImag           e:new("../icons/books-stack.png")),
	Bitmap = proplists:get_value(bitmap, Options),
	wxDC:drawBitmap(DC, Bitmap, {9,9}),
	
	%% Nothing is drawn until wxDC is destroyed.
	WxDc:destroy(DC),
	ok.