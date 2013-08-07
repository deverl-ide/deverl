-module(ide_status_bar).

-include_lib("wx/include/wx.hrl").
-include("../include/ide.hrl").

-behaviour(wx_object).

%% wx_objects callbacks
-export([init/1, terminate/2,  code_change/3,
         handle_info/2, handle_cast/2, handle_call/3, handle_event/2]).

%% Client API         
-export([new/1, set_text/3, set_text_timeout/3]).

-record(state, {parent :: wxWindow:wxWindow(),
                sb :: wxWindow:wxWindow(),     %% Status bar
                func_menu,                     %% The popup function menu
                fields :: [wxStaticText:wxStaticText()] 
                }).

-define(FG_COLOUR, {60,60,60}).
-define(FONT_SIZE, 11).
-define(PADDING, 4).

% -define(SB_ID_LINE, 1).
% -define(SB_ID_SELECTION, 2).
% -define(SB_ID_FUNCTION, 3).
% -define(SB_ID_HELP, 4).

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
  
	%% Function Menu Popup %%
	% add_label(Sb, ?wxID_ANY, SbSizer, "Function:"),
	% FuncDyn = wxStaticText:new(Sb, ?SB_ID_FUNCTION, ":", []), 
	% set_style(FuncDyn), 
	% wxSizer:add(SbSizer, FuncDyn, [{proportion, 1}, {border, ?PADDING}, {flag, ?wxALL}]),
	% FunctionPopup = create_menu(),
	% wxPanel:connect(FuncDyn, left_down),

	% PopupSizer = wxBoxSizer:new(?wxHORIZONTAL),
	% add_label(Sb, ?wxID_ANY, PopupSizer, "Functions"),
	% % Icon = wxBitmap:new(wxImage:new("icons/sb_menu.png")),
	% Icon = wxStaticBitmap:new(Sb, 345, wxBitmap:new(wxImage:new("icons/sb_menu.png"))),
	% wxSizer:add(PopupSizer, 0, 0, [{proportion, 1}]),
	% % wxSizer:add(PopupSizer, wxStaticBitmap:new(Sb, 345, Icon), [{border,5},{proportion,0},{flag, ?wxALIGN_CENTER_VERTICAL bor ?wxRIGHT}]),
	% wxSizer:add(PopupSizer, Icon, [{border,5},{proportion,0},{flag, ?wxALIGN_CENTER_VERTICAL bor ?wxRIGHT}]),
	% FunctionPopup = create_menu(),
	% wxSizer:add(SbSizer, PopupSizer, [{proportion, 1}]),
	% wxPanel:connect(Icon, left_down),
  
	PopupSizer = wxBoxSizer:new(?wxHORIZONTAL),
	L = wxStaticText:new(Sb, ?wxID_ANY, "Functions"),
	set_style(L),
	wxSizer:add(PopupSizer, L, [{proportion, 1},{border, ?PADDING},{flag, ?wxALL}]),
	Icon = wxStaticBitmap:new(Sb, 345, wxBitmap:new(wxImage:new("../icons/sb_menu.png"))),
	wxSizer:add(PopupSizer, Icon, [{border,5},{proportion,0},{flag, ?wxALIGN_CENTER_VERTICAL bor ?wxRIGHT}]),
	FunctionPopup = create_menu(),
	wxSizer:add(SbSizer, PopupSizer, [{proportion, 1}]),
	wxPanel:connect(Icon, left_down),
	wxPanel:connect(L, left_down),

	add_separator(Sb, SbSizer, Separator),
  
	Help = wxStaticText:new(Sb, ?SB_ID_HELP, "", []),
	set_style(Help), 
	wxSizer:add(SbSizer, Help, [{proportion, 1}, {border, ?PADDING}, {flag, ?wxEXPAND bor ?wxALL bor ?wxALIGN_RIGHT}]),  
	
  
	wxSizer:layout(SbSizer),
	Fields = [{line, Line} | [{selection, Selection} | [{help, Help} | []]]],   
	{Sb, #state{parent=Parent, sb=Sb, func_menu=FunctionPopup, fields=Fields}}.
  

%% =====================================================================
%% @doc OTP behaviour callbacks

handle_info(Msg, State) ->
    io:format("Got Info ~p~n",[Msg]),
    {noreply,State}.

handle_cast(Msg, State) ->
    io:format("Got cast ~p~n",[Msg]),
    {noreply,State}.

handle_call(fields, _From, State) ->
    {reply, {State#state.fields, State#state.parent}, State};
handle_call(shutdown, _From, State) ->
    ok,
    {reply,{error, nyi}, State}.

handle_event(#wx{obj = Object, event = #wxMouse{type = left_down}},
			 State = #state{sb=Sb, func_menu = Menu}) ->
    %% Open the popup menu
    wxWindow:popupMenu(Sb, Menu),
    {noreply, State};  
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

create_menu() ->
	Menu = wxMenu:new([]),
	wxMenuItem:enable(wxMenu:appendRadioItem(Menu, ?wxID_UNDO, 
					  "Document currently empty.", []), [{enable,false}]),
	wxMenu:connect(Menu, command_menu_selected),
	Menu.


%% =====================================================================
%% @doc Set the text in the specified field.
%% @see init Fields for field names.

-spec ide_status_bar:set_text(Sb, Field, Label) -> Result when
      Sb :: wxStatusBar:wxStatusBar(),
      Field :: {field, atom()},
      Label :: unicode:chardata(),
      Result :: atom().
      
set_text(Sb, {field, Field}, Label) ->
	{Fields, Parent} = wx_object:call(Sb, fields),
	case Field of
		line ->
			T = proplists:get_value(line, Fields),
			set_text(T, Label);
		selection ->
			T = proplists:get_value(selection, Fields),
			set_text(T, Label);
		help ->
			T = proplists:get_value(help, Fields),
			set_text(T, Label)
	end,
    wxSizer:layout(wxPanel:getSizer(Sb)).


%% =====================================================================
%% @doc Set the text in the specified field, for the period specified
%% in TIMEOUT, after which the field will be cleared.
    
set_text_timeout(Sb, {field,Field}, Label) ->
	set_text(Sb, {field,Field}, Label),
	receive 
		after ?TIMEOUT ->
			set_text(Sb, {field,Field}, "")
	end.


%% =====================================================================
%% @doc Set the text
%%
%% @private

set_text(Field, Label) ->
	wxStaticText:setLabel(Field, Label).  
