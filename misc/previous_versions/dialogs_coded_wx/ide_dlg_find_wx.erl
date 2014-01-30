-module(ide_dlg_find_wx).

-include_lib("wx/include/wx.hrl").
-include("ide.hrl").

-behaviour(wx_object).
-export([init/1, terminate/2,  code_change/3,
	       handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).

-export([new/1, 
         new/2,
         get_ref/1,
				 show/1]).

-record(state, {frame,
            	  data,
								to_focus
            	 }).


%% =====================================================================
%% Client API
%% =====================================================================

new(Parent) ->
  new(Parent, []).

new(Parent, Data) ->
  wx_object:start({local, ?MODULE}, ?MODULE, {Parent, Data}, []).


%% =====================================================================
%% Callback functions
%% =====================================================================

init(Config) ->
  wx:batch(fun() -> do_init(Config) end).

do_init(Config) ->
  {Parent, Data} = Config,
  Dialog = wxDialog:new(Parent, ?wxID_ANY, "Find and Replace"),
  
  Panel = wxWindow:new(Dialog, ?wxID_ANY),     
  MainSz = wxBoxSizer:new(?wxHORIZONTAL),
  wxWindow:setSizer(Panel, MainSz),
  wxSizer:addSpacer(MainSz, 20),
  
  FlexGridSz = wxFlexGridSizer:new(2, [{vgap, 10}, {hgap, 5}]),
  wxSizer:add(FlexGridSz, wxStaticText:new(Panel, ?wxID_ANY, "Find:"), [{flag, ?wxALIGN_RIGHT bor ?wxALIGN_CENTRE_VERTICAL}]),
  ToFocus = wxTextCtrl:new(Panel, ?FIND_INPUT, []),
	wxSizer:add(FlexGridSz, ToFocus, [{flag, ?wxEXPAND}, {proportion, 1}]),
  
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
	DefButton = wxButton:new(Panel, ?FIND_ALL, [{label,"Find All"}]),
	wxButton:setDefault(DefButton),
  wxSizer:add(ButtonSz, DefButton, [{border,10}, {flag, ?wxEXPAND bor ?wxBOTTOM}]),
  wxSizer:add(ButtonSz, wxButton:new(Panel, ?REPLACE_ALL, [{label,"Replace All"}]), [{flag, ?wxEXPAND}]),
  wxSizer:addSpacer(ButtonSz, 15),
  wxSizer:add(ButtonSz, wxButton:new(Panel, ?REPLACE_FIND, [{label,"Replace && Find"}]), [{flag, ?wxEXPAND}]),
  wxSizer:addSpacer(ButtonSz, 15),
  wxSizer:add(ButtonSz, wxButton:new(Panel, ?FIND_PREV, [{label,"Find Previous"}]), [{border,10}, {flag, ?wxEXPAND bor ?wxBOTTOM}]),
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
	      
  wxDialog:connect(Dialog, close_window),
      
  wxPanel:connect(Panel, key_down, [{skip, true}]),
  wxPanel:connect(Panel, command_checkbox_clicked, []),
  wxPanel:connect(Panel, command_choice_selected, []),
  wxPanel:connect(Panel, command_text_updated, []),
  wxPanel:connect(Panel, command_button_clicked, [{skip, true}]), 
  % wxPanel:connect(Panel, command_button_clicked, [{callback, fun(E,O) -> io:format("E: ~p~nO:~p~n", [E,O]),wxEvent:skip(O) end}]),  
  
  {Dialog, #state{frame=Dialog, data=Data, to_focus=ToFocus}}.
 
%% =====================================================================
%% @doc Get the reference to a living dialog.

get_ref(This) ->
  wx_object:call(This, ref).

%% =====================================================================
%% @doc Initialise the dialog
%% @private

init_data(Parent, Data) ->
  Dl = ide_dlg_data_find_wx:get_data(Data),
  F=fun({_,undefined}) -> ok;
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
%% This is a useful utility function, and therefore might be better in a seperate
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
%% @doc Display the dialog

show(This) ->
	wxDialog:show(This),
	%% Set focus to the correct input
	wxWindow:setFocusFromKbd(wx_object:call(This, to_focus)).

 
%% =====================================================================
%% @doc OTP behaviour callbacks
handle_event(#wx{event=#wxClose{}}, State) ->
  {stop, normal, State};
  
handle_event(#wx{event=#wxKey{type=key_down, keyCode=27}}, State) ->
  {stop, shutdown, State};
  
handle_event(#wx{id=Cb, event=#wxCommand{type=command_checkbox_clicked, commandInt=Checked}}, 
             State=#state{data=Data}) ->
  Options = ide_dlg_data_find_wx:get_options(Data),
  NewOptions = case Checked of
    0 -> Options - Cb;
    1 -> Options + Cb
  end,
  ide_dlg_data_find_wx:set_options(Data, NewOptions),
  {noreply,State};
  
handle_event(#wx{event=#wxCommand{type=command_choice_selected, commandInt=Choice}}, 
             State=#state{data=Data}) ->
  ide_dlg_data_find_wx:set_search_location(Data, Choice),
  {noreply,State};
  
handle_event(#wx{id=Id, event=#wxCommand{type=command_text_updated, cmdString=Str}}, 
             State=#state{data=Data}) ->
  case Id of
    ?FIND_INPUT -> ide_dlg_data_find_wx:set_find_string(Data, Str);
    ?REPLACE_INPUT -> ide_dlg_data_find_wx:set_replace_string(Data, Str)
  end,
  {noreply,State};
  
handle_event(#wx{id=Id, event=#wxCommand{type=command_button_clicked}}, 
             State=#state{data=Data}) ->
  case Id of
    ?FIND_ALL ->
      {ok, {_Index,Pid}} = ide_doc_man_wx:get_active_document(),
      ide_editor_wx:find_all(Pid, ide_dlg_data_find_wx:get_find_string(Data)),
      ok;
    ?REPLACE_ALL ->
      {ok, {_Index,Pid}} = ide_doc_man_wx:get_active_document(),
      ide_editor_wx:replace_all(Pid, ide_dlg_data_find_wx:get_find_string(Data),
        ide_dlg_data_find_wx:get_replace_string(Data)),
      ok;  
    ?REPLACE_FIND ->
      ok;
    ?FIND_NEXT ->
      ok;
    ?FIND_PREV ->
      ok
  end,  
  {noreply,State};
               
handle_event(Ev, State=#state{}) ->
  io:format("Got Event (ide_dlg_find_wx) ~p~n",[Ev]),
  {noreply,State}.


handle_info(Msg, State) ->
  io:format("Got Info ~p~n",[Msg]),
  {noreply,State}.

handle_call(shutdown, _From, State=#state{frame=Dialog}) ->
  wxWindow:destroy(Dialog),
  {stop, normal, ok, State};

handle_call(ref, _From, State=#state{frame=Dialog}) ->
  {reply,Dialog,State};
	
handle_call(to_focus, _From, State=#state{to_focus=ToFocus}) ->
  {reply,ToFocus,State};

handle_call(Msg, _From, State) ->
  io:format("Got Call ~p~n",[Msg]),
  {reply,ok,State}.

handle_cast(Msg, State) ->
  io:format("Got cast ~p~n",[Msg]),
  {noreply,State}.

code_change(_, _, State) ->
  {stop, ignore, State}.

terminate(_Reason, #state{frame=Dialog}) ->
  erlang:unregister(?MODULE),
  wxDialog:destroy(Dialog).