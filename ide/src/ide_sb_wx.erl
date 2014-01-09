%% =====================================================================
%% @author
%% @copyright
%% @title
%% @version
%% @doc This module is builds and provides functions for updating the
%% status bar. It is implemented as a wxPanel to provide more 
%% flexibility than is currently offered by the built in wxStatusBar.
%% @end
%% =====================================================================

-module(ide_sb_wx).

-include_lib("wx/include/wx.hrl").
-include("ide.hrl").

%% wx_object
-behaviour(wx_object).

-export([init/1, terminate/2,  code_change/3,
         handle_info/2, handle_cast/2, handle_call/3, handle_event/2]).

%% Client API         
-export([
	start/1, 
	set_text/2
	]).

%% Macros
-define(FG_COLOUR, {60,60,60}).
-define(FONT_SIZE, 11).
-define(PADDING, 4).
-define(TIMEOUT, 1000).

%% Server state
-record(state, {parent :: wxWindow:wxWindow(),
                sb :: wxWindow:wxWindow(),     %% Status bar
                fields :: [wxStaticText:wxStaticText()] 
                }).


%% =====================================================================
%% Client API
%% =====================================================================
 
%% =====================================================================
%% @doc

start(Config) ->
	wx_object:start({local, ?MODULE}, ?MODULE, Config, [{debug, [log]}]).


%% =====================================================================
%% @doc Set the text in the specified field.

-spec ide_sb_wx:set_text(Field, Label) -> Result when
      Field :: {field, atom()},
      Label :: unicode:chardata(),
      Result :: atom().
      
set_text({field, Field}, Label) ->
	wx_object:cast(?MODULE, {settext, {Field, Label}}).


%% =====================================================================
%% Callback functions
%% =====================================================================
  
init(Config) ->
	Parent = proplists:get_value(parent, Config),
  
	Sb = wxPanel:new(Parent, []),
	SbSizer = wxBoxSizer:new(?wxHORIZONTAL),
	wxPanel:setSizer(Sb, SbSizer),
  
	Separator = wxBitmap:new(wxImage:new(ide_lib_widgets:rc_dir("separator.png"))),

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
  
	Help = wxStaticText:new(Sb, ?SB_ID_HELP, "", []),
	set_style(Help), 
	wxSizer:add(SbSizer, Help, [{proportion, 1}, {border, ?PADDING}, {flag, ?wxEXPAND bor ?wxALL bor ?wxALIGN_RIGHT}]),  
	  
	wxSizer:layout(SbSizer),
	Fields = [{line, Line} | [{selection, Selection} | [{help, Help} | []]]],   
	{Sb, #state{parent=Parent, sb=Sb, fields=Fields}}.

handle_info(Msg, State) ->
	io:format("Got Info ~p~n",[Msg]),
	{noreply,State}.

handle_cast({settext, {Field,Label}}, State=#state{fields=Fields, sb=Sb}) ->
	T = proplists:get_value(Field, Fields),
	set_label(T, Label),
	wxSizer:layout(wxPanel:getSizer(Sb)),
  {noreply,State}.

handle_call(fields, _From, State) ->
  {reply, State#state.fields, State};	
handle_call(shutdown, _From, State) ->
  ok,
  {reply,{error, nyi}, State}.

handle_event(_Event, State) ->
	{noreply, State}.
  
code_change(_, _, State) ->
  {stop, not_yet_implemented, State}.

terminate(_Reason, #state{sb=Sb}) ->
	wxPanel:destroy(Sb).


%% =====================================================================
%% Internal functions
%% =====================================================================

%% =====================================================================
%% @doc Set common status bar styles i.e font
%% @private

set_style(Window) ->
	Font = wxFont:new(?FONT_SIZE, ?wxFONTFAMILY_SWISS, ?wxNORMAL, ?wxNORMAL,[]),
	wxWindow:setFont(Window, Font),
	wxWindow:setForegroundColour(Window, ?FG_COLOUR).


%% =====================================================================
%% @doc Insert a separator into the status bar
%% @private

add_separator(Sb, Sizer, Bitmap) ->
	wxSizer:add(Sizer, wxStaticBitmap:new(Sb, 345, Bitmap), [{flag, ?wxALIGN_CENTER_VERTICAL}]).


%% =====================================================================
%% @doc Insert a text label into the status bar
%% @private

add_label(Sb, Id, Sizer, Label) ->
	L = wxStaticText:new(Sb, Id, Label),
	set_style(L),
	wxSizer:add(Sizer, L, [{border, ?PADDING}, {flag, ?wxALL}]).


%% =====================================================================
%% @doc Set the text
%% @private

set_label(Field, Label) ->
	wxStaticText:setLabel(Field, Label).  