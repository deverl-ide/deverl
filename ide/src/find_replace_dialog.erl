-module(find_replace_dialog).

-include_lib("wx/include/wx.hrl").
-include("../include/ide.hrl").

-behaviour(wx_object).
-export([init/1, terminate/2,  code_change/3,
	       handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).

-export([new/1, new/2]).

-record(state, {frame,
            	  config
            	 }).

new(Parent) ->
  new(Parent, []).

new(Parent, Data) ->
  wx_object:start(?MODULE, {Parent, Data}, []).

init(Config) ->
  wx:batch(fun() -> do_init(Config) end).

do_init(Config) ->
  {Parent, Data} = Config,
  Dialog = wxDialog:new(Parent, ?wxID_ANY, "Find and Replace"),
  
  Panel = wxWindow:new(Dialog, ?wxID_ANY),     
  MainSz = wxBoxSizer:new(?wxHORIZONTAL),
  wxWindow:setSizer(Panel, MainSz),
  wxSizer:addSpacer(MainSz, 20),
  
  FlexGridSz = wxFlexGridSizer:new(4, 2, 10, 5),
  wxSizer:add(FlexGridSz, wxStaticText:new(Panel, ?wxID_ANY, "Find:"), [{flag, ?wxALIGN_RIGHT bor ?wxALIGN_CENTRE_VERTICAL}]),
  wxSizer:add(FlexGridSz, wxTextCtrl:new(Panel, ?FIND_INPUT, []), [{flag, ?wxEXPAND}, {proportion, 1}]),
  
  wxSizer:add(FlexGridSz, wxStaticText:new(Panel, ?wxID_ANY, "Replace:"), [{flag, ?wxALIGN_RIGHT bor ?wxALIGN_CENTRE_VERTICAL}]),
  wxSizer:add(FlexGridSz, wxTextCtrl:new(Panel,?REPLACE_INPUT, []), [{flag, ?wxEXPAND}, {proportion, 1}]),
  
  wxSizer:add(FlexGridSz, wxStaticText:new(Panel, ?wxID_ANY, "Options:"), [{flag, ?wxALIGN_RIGHT bor ?wxALIGN_CENTRE_VERTICAL}]),
  OptionSz = wxGridSizer:new(2,2,10,10),
  wxSizer:add(OptionSz, wxCheckBox:new(Panel, ?IGNORE_CASE, "Ignore case")),
  wxSizer:add(OptionSz, wxCheckBox:new(Panel, ?WHOLE_WORD, "Whole word")),
  wxSizer:add(OptionSz, wxCheckBox:new(Panel, ?START_WORD, "Start of word")),
  wxSizer:add(OptionSz, wxCheckBox:new(Panel, ?REGEX, "Regex")),
  wxSizer:add(FlexGridSz, OptionSz),
  
  wxSizer:add(FlexGridSz, wxStaticText:new(Panel, ?wxID_ANY, "Search In:"), [{flag, ?wxALIGN_RIGHT bor ?wxALIGN_CENTRE_VERTICAL}]),
  Choices = ["Document", "Project", "Open Documents"],
  wxSizer:add(FlexGridSz, wxChoice:new(Panel,?FIND_LOC, [{choices, Choices}])),
  
  wxSizer:add(MainSz, FlexGridSz, [{border,20}, {flag, ?wxTOP bor ?wxBOTTOM}]),
  wxSizer:addSpacer(MainSz, 20),
  
  ButtonSz = wxBoxSizer:new(?wxVERTICAL),
  wxSizer:add(ButtonSz, wxButton:new(Panel, ?FIND_ALL, [{label,"Find All"}]), [{border,5}, {flag, ?wxEXPAND bor ?wxBOTTOM}]),
  wxSizer:add(ButtonSz, wxButton:new(Panel, ?REPLACE_ALL, [{label,"Replace All"}]), [{border,5}, {flag, ?wxEXPAND bor ?wxBOTTOM}]),
  wxSizer:addSpacer(ButtonSz, 15),
  wxSizer:add(ButtonSz, wxButton:new(Panel, ?REPLACE_FIND, [{label,"Replace && Find"}]), [{border,5}, {flag, ?wxEXPAND bor ?wxBOTTOM}]),
  wxSizer:addSpacer(ButtonSz, 15),
  wxSizer:add(ButtonSz, wxButton:new(Panel, ?FIND_PREV, [{label,"Find Previous"}]), [{border,5}, {flag, ?wxEXPAND bor ?wxBOTTOM}]),
  wxSizer:add(ButtonSz, wxButton:new(Panel, ?FIND_NEXT, [{label,"Find Next"}]), [{flag, ?wxEXPAND}]),
  
  wxSizer:add(MainSz, ButtonSz, [{border,20}, {flag, ?wxTOP bor ?wxBOTTOM}]),
  wxSizer:addSpacer(MainSz, 20),
  
  wxSizer:layout(MainSz),
	wxSizer:fit(MainSz, Dialog),
	wxSizer:setSizeHints(MainSz, Dialog),
  
  %% Init data
  case Data of
    [] -> ok;
    _ -> init_data(Panel, Data)
  end,  
    
  wxDialog:show(Dialog),
  
  wxPanel:connect(Panel, key_down, []),
  
  {Dialog, #state{frame=Dialog}}.
 
 
%% =====================================================================
%% @doc Initialise the dialog
%% @private

init_data(Parent, Data) ->
  Dl = find_replace_data:get_data(Data),
  F=fun({T,undefined}) -> ok;
       ({find_str, D}) -> 
        wxTextCtrl:setValue(get_window_as(?FIND_INPUT, Parent, wxTextCtrl), D);
       ({replace_str, D}) ->
        wxTextCtrl:setValue(get_window_as(?REPLACE_INPUT, Parent, wxTextCtrl), D);
       ({options, D}) ->
        Flags = [?IGNORE_CASE, ?WHOLE_WORD, ?START_WORD, ?REGEX],  
        Fun = fun(X) ->
        wxCheckBox:setValue(get_window_as(X, Parent, wxCheckBox), true)
        end,
        [ Fun(Flag) || Flag <- Flags, Flag band D /= 0 ];
       ({search_loc, D}) ->
        wxChoice:setSelection(get_window_as(?FIND_LOC, Parent, wxChoice), D)
  end,
  [ F(X) || X <- Dl ].
  
 
%% =====================================================================
%% @doc Get a child from a window by Id
%% This is a useful function, and therefore might be better in a seperate
%% library module, accessable by all.
%% @private

-spec get_window_as(Id, Parent, Type) -> Result when
  Id :: integer(),
  Parent :: wxWindow:wxWindow(),
  Type :: wxWindow:wxWindow(),
  Result :: wxWindow:wxWindow(). %% :: Type
 
get_window_as(Id, Parent, Type) ->
   wx:typeCast(wxWindow:findWindowById(Id, [{parent, Parent}]), Type).
  
  
%% =====================================================================
%% @doc OTP behaviour callbacks

handle_event(Ev=#wx{obj=Dialog, event=#wxKey{type=key_down, keyCode=27}}, State) ->
  {stop, shutdown, State};
handle_event(Ev = #wx{}, State) ->
  io:format("Got Event ~p~n",[Ev]),
  {noreply,State}.

handle_info(Msg, State) ->
  io:format("Got Info ~p~n",[Msg]),
  {noreply,State}.

handle_call(shutdown, _From, State=#state{frame=Dialog}) ->
  io:format("FIND SHUTDOWN~n"),
  wxWindow:destroy(Dialog),
  {stop, normal, ok, State};

handle_call(Msg, _From, State) ->
  io:format("Got Call ~p~n",[Msg]),
  {reply,ok,State}.

handle_cast(Msg, State) ->
  io:format("Got cast ~p~n",[Msg]),
  {noreply,State}.

code_change(_, _, State) ->
  {stop, ignore, State}.

terminate(_Reason, #state{frame=Dialog}) ->
  io:format("TERMINATE FIND/REPLACE~n"),
  wxDialog:destroy(Dialog).