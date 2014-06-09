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
%% @doc This module manages several sub-windows, displaying each in a
%% separate tab with an accompanying icon.
%% @end
%% =====================================================================


-module(deverl_tabbed_win_img_wx).

-include_lib("wx/include/wx.hrl").

%% wx_object
-behaviour(wx_object).
-export([
     init/1, terminate/2,  code_change/3,
     handle_info/2, handle_call/3, handle_cast/2,
     handle_event/2, handle_sync_event/3]).

%% API
-export([
    start/1,
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

%% =====================================================================
%% @doc

start(Config) ->
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
%% @hidden
init(Options) ->
    Parent = proplists:get_value(parent, Options),

    MainPanel = wxPanel:new(Parent, []),
    MainSz = wxBoxSizer:new(?wxVERTICAL),
    wxPanel:setSizer(MainPanel, MainSz),

    Tabs = wxPanel:new(MainPanel),
  wxWindow:setBackgroundColour(Tabs, deverl_lib_widgets:colour_shade(wxSystemSettings:getColour(?wxSYS_COLOUR_WINDOW), 0.8)),
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

%% @hidden
handle_info(Msg, State) ->
  io:format("Got Info ~p~n",[Msg]),
  {noreply,State}.
%% @hidden
handle_call(Msg, _From, State) ->
  io:format("Got Call ~p~n",[Msg]),
  {reply,ok,State}.
%% @hidden
handle_cast({add_page, {Page, Text, Options}},
                        State=#state{tabs={TabPanel, TabSz}, content={Content, ContentSz}, pages=Pages}) ->
    {Button, Label} = create_button(TabPanel, TabSz, Text, Options),
    SzFlags = wxSizerFlags:new([{proportion, 1}]),
    wxSizerFlags:expand(SzFlags),
    wxWindow:reparent(Page, Content),
    wxWindow:hide(Page),
    wxSizer:add(ContentSz, Page, SzFlags),
    UpdatedPages = [{Button, {Page, Options}} | Pages],
    wxPanel:layout(TabPanel),
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
%% @hidden
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
%% @hidden
handle_event(#wx{obj=Btn, event=#wxMouse{type=left_down}},
                         State=#state{pages=Pages, active_btn=ActiveBtn, tabs={Tabs, _}, content={Cont,_}}) ->
    change_selection(ActiveBtn, Btn, Pages, Tabs, Cont),
    {noreply, State#state{active_btn=Btn}};
handle_event(#wx{event=#wxClose{}}, State) ->
    io:format("~p Closing window ~n",[self()]),
    {stop, normal, State};
handle_event(#wx{id=Id},State) ->
  case Id of
      ?wxID_EXIT ->
        {stop, normal, State};
      _ ->
        {noreply, State}
  end.
%% @hidden
code_change(_, _, State) ->
    {stop, not_yet_implemented, State}.
%% @hidden
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
    Btn = wxPanel:new(Parent, [{size,{34,34}}]), %% TESTING
  wxWindow:setToolTip(Btn, Label),
    wxSizer:add(Sz, Btn, SzFlags),
  wxPanel:connect(Btn, paint, [callback, {userData, {Label, Options}}]),
    wxPanel:connect(Btn, left_down, [{skip, true}, {userData, {Label, Options}}]),
    {Btn, Label}.


%% =====================================================================
%% @doc Draw the tab graphic.
%% Default {active, false}
draw(Btn, _Label, ImageList, WxDc, Bg, Options) ->
  wxWindow:setBackgroundColour(Btn, Bg),
  {W,_H} = wxWindow:getSize(Btn),

  %% Deselected button style
  % FillD = {190,190,190},
  % LineD = {155,155,155},
  %% Selected button style
  FillA = {170,170,170},
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
      % Draw(FillD, LineD, Bg);
      ok;
    true ->
      Draw(FillA, LineA, Bg)
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
    wxPanel:update(Tabs),
    wxPanel:refresh(Tabs),
    ok.
