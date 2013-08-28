-module(ide_status_bar).

-include_lib("wx/include/wx.hrl").
-include("ide.hrl").

-behaviour(wx_object).

%% wx_objects callbacks
-export([init/1, terminate/2,  code_change/3,
         handle_info/2, handle_cast/2, handle_call/3, handle_event/2]).

%% Client API         
-export([
	new/1, 
	set_text/3, 
	set_text_timeout/3
	% set_func_list/2
	]).

-record(state, {parent :: wxWindow:wxWindow(),
                sb :: wxWindow:wxWindow(),     %% Status bar
                % func_menu,                     %% The popup function menu
                fields :: [wxStaticText:wxStaticText()] 
                }).

-define(FG_COLOUR, {60,60,60}).
-define(FONT_SIZE, 11).
-define(PADDING, 4).
-define(TIMEOUT, 1000).

-export_type([status_bar/0]).

-type status_bar() :: wxWindow:wxWindow().

new(Config) ->
	start(Config).
  
start(Config) ->
	wx_object:start(?MODULE, Config, [{debug, [log]}]).
  
init(Config) ->
	Parent = proplists:get_value(parent, Config),
  
	Sb = wxPanel:new(Parent, []),
	SbSizer = wxBoxSizer:new(?wxHORIZONTAL),
	wxPanel:setSizer(Sb, SbSizer),
  
	Separator = wxBitmap:new(wxImage:new("../icons/separator.png")),

	add_label(Sb, ?wxID_ANY, SbSizer, "Text:"),                                    
	Line = wxStaticText:new(Sb, ?SB_ID_LINE, "1", []),
	set_style(Line),  
	wxSizer:add(SbSizer, Line, [{border, ?PADDING}, {flag, ?wxALL}]),

	add_separator(Sb, SbSizer, Separator),
 
	add_label(Sb, ?wxID_ANY, SbSizer, "Selection:"),
	Selection = wxStaticText:new(Sb, ?SB_ID_SELECTION, "-", []), 
	set_style(Selection), 
	wxSizer:add(SbSizer, Selection, [{border, ?PADDING}, {flag, ?wxALL}]),
   
	add_separator(Sb, SbSizer, Separator),

	% PopupSizer = wxBoxSizer:new(?wxHORIZONTAL),
	% L = wxStaticText:new(Sb, ?wxID_ANY, "Functions"),
	% set_style(L),
	% wxSizer:add(PopupSizer, L, [{proportion, 1},{border, ?PADDING},{flag, ?wxALL}]),
	% Icon = wxStaticBitmap:new(Sb, 345, wxBitmap:new(wxImage:new("../icons/sb_menu.png"))),
	% wxSizer:add(PopupSizer, Icon, [{border,5},{proportion,0},{flag, ?wxALIGN_CENTER_VERTICAL bor ?wxRIGHT}]),
	% FunctionPopup = create_menu(),
	% wxSizer:add(SbSizer, PopupSizer, [{proportion, 1}]),
	% wxPanel:connect(Icon, left_down),
	% wxPanel:connect(L, left_down),
	% 
	% add_separator(Sb, SbSizer, Separator),
  
	Help = wxStaticText:new(Sb, ?SB_ID_HELP, "", []),
	set_style(Help), 
	wxSizer:add(SbSizer, Help, [{proportion, 1}, {border, ?PADDING}, {flag, ?wxEXPAND bor ?wxALL bor ?wxALIGN_RIGHT}]),  
	
	% wxPanel:connect(Sb, command_menu_selected, []),
  
	wxSizer:layout(SbSizer),
	Fields = [{line, Line} | [{selection, Selection} | [{help, Help} | []]]],   
	{Sb, #state{parent=Parent, sb=Sb, fields=Fields}}.
  

%% =====================================================================
%% @doc OTP behaviour callbacks

handle_info(Msg, State) ->
	io:format("Got Info ~p~n",[Msg]),
	{noreply,State}.

handle_cast({settext, {Field,Label}}, State=#state{fields=Fields, sb=Sb}) ->
	T = proplists:get_value(Field, Fields),
	set_text(T, Label),
	wxSizer:layout(wxPanel:getSizer(Sb)),
  {noreply,State}.


handle_call(fields, _From, State) ->
  {reply, State#state.fields, State};
	
% handle_call({func_menu, List}, _From, State=#state{func_menu=Menu}) ->
% 	wxMenu:destroy(Menu),
% 	NewMenu = wxMenu:new([]),
% 	[wxMenu:append(NewMenu, ?wxID_ANY, lists:flatten(Label)) || Label <- List],
% 	{reply, ok, State#state{func_menu=NewMenu}};
% 	
% handle_call(clear_menu, _From, State=#state{func_menu=Menu}) ->
% 	wxMenu:destroy(Menu),
% 	{reply, ok, State#state{func_menu=create_menu()}};
	
handle_call(shutdown, _From, State) ->
  ok,
  {reply,{error, nyi}, State}.


% handle_event(#wx{obj=Object, event=#wxMouse{type=left_down}},
% 						State = #state{sb=Sb, func_menu=Menu}) ->
% 	%% Open the popup menu
% 	wxWindow:popupMenu(Sb, Menu),
% 	{noreply, State};  

% handle_event(O=#wx{obj=Object, event=#wxCommand{type=command_menu_selected}=E},
% 						State=#state{sb=Sb, func_menu=Menu}) ->
% 	io:format("Event: ~p~n~p~n", [O,E]),
% 	{noreply, State}; 

handle_event(Event, State) ->
	io:format("SB EVENT CA~n"),
	{noreply, State}.
  

code_change(_, _, State) ->
  {stop, not_yet_implemented, State}.

terminate(_Reason, #state{sb=Sb}) ->
	io:format("TERMINATE STATUS BAR~n"),
	wxPanel:destroy(Sb).


%% =====================================================================
%% @doc Set common status bar styles i.e font
%%
%% @private

set_style(Window) ->
	Font = wxFont:new(?FONT_SIZE, ?wxFONTFAMILY_SWISS, ?wxNORMAL, ?wxNORMAL,[]),
	wxWindow:setFont(Window, Font),
	wxWindow:setForegroundColour(Window, ?FG_COLOUR).


%% =====================================================================
%% @doc Insert a separator into the status bar
%%
%% @private

add_separator(Sb, Sizer, Bitmap) ->
	wxSizer:add(Sizer, wxStaticBitmap:new(Sb, 345, Bitmap), [{flag, ?wxALIGN_CENTER_VERTICAL}]).


%% =====================================================================
%% @doc Insert a text label into the status bar
%%
%% @private

add_label(Sb, Id, Sizer, Label) ->
	L = wxStaticText:new(Sb, ?wxID_ANY, Label),
	set_style(L),
	wxSizer:add(Sizer, L, [{border, ?PADDING}, {flag, ?wxALL}]).


%% =====================================================================
%% @doc Create the function popup menu
%%
%% @private

% create_menu() ->
% 	Menu = wxMenu:new([]),
% 	wxMenuItem:enable(wxMenu:appendRadioItem(Menu, ?wxID_ANY, 
% 					  "Document currently empty.", []), [{enable,false}]),
% 	wxMenu:connect(Menu, command_menu_selected),
% 	Menu.


%% =====================================================================
%% @doc Set the text in the specified field.
%% @see init Fields for field names.

-spec ide_status_bar:set_text(Sb, Field, Label) -> Result when
      Sb :: wxStatusBar:wxStatusBar(),
      Field :: {field, atom()},
      Label :: unicode:chardata(),
      Result :: atom().
      
set_text(Sb, {field, Field}, Label) ->
	wx_object:cast(Sb, {settext, {Field, Label}}).


%% =====================================================================
%% @doc Set the text in the specified field, for the period specified
%% in TIMEOUT, after which the field will be cleared.
    
set_text_timeout(Sb, {field,Field}, Label) ->
	% set_text(Sb, {field,Field}, Label),
	% io:format("PID: ~p~n", [self()]),
	% receive 
	% 	after ?TIMEOUT ->
	% 		set_text(Sb, {field,Field}, "")
	% end.
	ok.	


%% =====================================================================
%% @doc Set the text
%% @private

set_text(Field, Label) ->
	wxStaticText:setLabel(Field, Label).  
	
	
% set_func_list(Sb, []) ->
% 	wx_object:call(Sb, clear_menu);
% set_func_list(Sb, List) ->
% 	wx_object:call(Sb, {func_menu, List}).