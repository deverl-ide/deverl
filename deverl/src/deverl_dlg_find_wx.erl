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
%% @doc Displays the XRC based find/replace dialog.
%% @end
%% =====================================================================

-module(deverl_dlg_find_wx).

-include_lib("wx/include/wx.hrl").
-include("deverl.hrl").

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

-spec new(wxFrame:wxFrame()) -> wxWindow:wxWindow().

new(Parent) ->
  new(Parent, []).

-spec new(wxFrame:wxFrame(), list()) -> wxWindow:wxWindow().

new(Parent, Data) ->
  wx_object:start({local, ?MODULE}, ?MODULE, {Parent, Data}, []).


%% =====================================================================
%% @doc Get the reference to a living dialog.

-spec get_ref(wxDialog:wxDialog()) -> wxDialog:wxDialog().

get_ref(This) ->
  wx_object:call(This, ref).
  
  
%% =====================================================================
%% @doc Display the dialog

-spec show(wxDialog:wxDialog()) -> ok.

show(This) ->
	wxDialog:show(This),
	%% Set focus to the correct input
	wxWindow:setFocusFromKbd(wx_object:call(This, to_focus)).
  

%% =====================================================================
%% Callback functions
%% =====================================================================
%% @hidden
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
%% @doc Initialise the dialog
%% @private

init_data(Parent, Data) ->
  Dl = deverl_dlg_data_find_wx:get_data(Data),
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
%% @doc OTP behaviour callbacks
%% @hidden
handle_event(#wx{event=#wxClose{}}, State) ->
  {stop, normal, State};
handle_event(#wx{event=#wxKey{type=key_down, keyCode=27}}, State) ->
  {stop, shutdown, State};
  
handle_event(#wx{id=Cb, event=#wxCommand{type=command_checkbox_clicked, commandInt=Checked}}, 
             State=#state{data=Data}) ->
  Options = deverl_dlg_data_find_wx:get_options(Data),
  NewOptions = case Checked of
    0 -> Options - Cb;
    1 -> Options + Cb
  end,
  deverl_dlg_data_find_wx:set_options(Data, NewOptions),
  {noreply,State};
  
handle_event(#wx{event=#wxCommand{type=command_choice_selected, commandInt=Choice}}, 
             State=#state{data=Data}) ->
  deverl_dlg_data_find_wx:set_search_location(Data, Choice),
  {noreply,State};
  
handle_event(#wx{id=Id, event=#wxCommand{type=command_text_updated, cmdString=Str}}, 
             State=#state{data=Data}) ->
  case Id of
    ?FIND_INPUT -> deverl_dlg_data_find_wx:set_find_string(Data, Str);
    ?REPLACE_INPUT -> deverl_dlg_data_find_wx:set_replace_string(Data, Str)
  end,
  {noreply,State};
  
handle_event(#wx{id=Id, event=#wxCommand{type=command_button_clicked}}, 
             State=#state{data=Data}) ->
  case Id of
    ?FIND_ALL ->
      {ok, {_Index,Pid}} = deverl_doc_man_wx:get_active_document(),
      deverl_editor_wx:find_all(Pid, deverl_dlg_data_find_wx:get_find_string(Data)),
      ok;
    ?REPLACE_ALL ->
      {ok, {_Index,Pid}} = deverl_doc_man_wx:get_active_document(),
      deverl_editor_wx:replace_all(Pid, deverl_dlg_data_find_wx:get_find_string(Data),
        deverl_dlg_data_find_wx:get_replace_string(Data)),
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
  io:format("Got Event (deverl_dlg_find_wx) ~p~n",[Ev]),
  {noreply,State}.

%% @hidden
handle_info(Msg, State) ->
  io:format("Got Info ~p~n",[Msg]),
  {noreply,State}.
%% @hidden
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
%% @hidden
handle_cast(Msg, State) ->
  io:format("Got cast ~p~n",[Msg]),
  {noreply,State}.
%% @hidden
code_change(_, _, State) ->
  {ok, State}.
%% @hidden
terminate(_Reason, #state{frame=Dialog}) ->
  erlang:unregister(?MODULE),
  wxDialog:destroy(Dialog).


%% =====================================================================
%% Internal functions
%% =====================================================================

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
