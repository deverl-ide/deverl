-module(xrctest).
-compile(export_all).

-include_lib("wx/include/wx.hrl").
-export([start/1, init/1, terminate/2,  code_change/3,
	       handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).

-behaviour(wx_object).

-record(state, {parent,
            	  dialog
            	 }).


%% =====================================================================
%% Client API
%% =====================================================================

%% =====================================================================
%% @doc

start(Parent) ->
  wx_object:start_link(?MODULE, Parent, []).
  
destroy(This) ->
  wx_object:call(This, destroy).


%% =====================================================================
%% Callback functions
%% =====================================================================

init(Parent) ->
    io:format("In Test~n"),
    Xrc = wxXmlResource:get(),
    Dlg = wxDialog:new(),
    true = wxXmlResource:loadDialog(Xrc, Dlg, Parent, "derived_dialog"),

    %% Shows that callbacks can be used it doesn't need to though.
    OnMyButtonClicked = fun(_EvRec, _wxEvent) ->
				MD = wxMessageDialog:new(Dlg, "You clicked on My Button"),
				wxMessageDialog:showModal(MD),
				wxMessageDialog:destroy(MD)
			end,
    % wxDialog:connect(Dlg, command_button_clicked, 
    %          [{id,wxXmlResource:getXRCID("my_button")},
    %           {callback,OnMyButtonClicked}]),
    wxDialog:connect(Dlg, command_button_clicked, 
		     [{id,wxXmlResource:getXRCID("my_button")}]),
    
   Text = wxXmlResource:xrcctrl(Dlg, "my_textctrl", wxTextCtrl),
    OnMyCheckBox = fun(_EvRec, _Event) ->
			   CheckB = wxXmlResource:xrcctrl(Dlg, "my_checkbox", wxCheckBox),
			   Bool = wxCheckBox:isChecked(CheckB),
			   wxTextCtrl:enable(Text, [{enable,Bool}])
		   end,
    wxDialog:connect(Dlg,update_ui,[{id,wxXmlResource:getXRCID("my_checkbox")},
				    {callback,OnMyCheckBox}]),
    wxTextCtrl:connect(Text, command_text_updated),
    
    %% Keep updateUI event interval at 250ms
    wxUpdateUIEvent:setUpdateInterval(250),
    
    OnOk = fun(_,_) ->
		   Str = 
		       "Press Ok to close derived dialog, or Cancel to abort"
		       "Overriding base class ok button handler",
		   MD = wxMessageDialog:new(Dlg, Str, [{style, ?wxOK bor ?wxCANCEL bor ?wxCENTER}]),
		   case wxMessageDialog:showModal(MD) of
		       ?wxID_OK -> 
			   wxMessageDialog:endModal(Dlg, ?wxID_OK);
		       _R ->
			   ignore
		   end,
		   wxMessageDialog:destroy(MD)
	   end,
    wxDialog:connect(Dlg,command_button_clicked,[{id,?wxID_OK},{callback,OnOk}]),
    
    %% Dialog segfaults when closed using close button (using default handler or not)
    wxDialog:connect(Dlg, close_window, [{callback, fun(_,_) -> 
      io:format("CLOSE DIALOG~n"),
      wxDialog:endModal(Dlg, ?wxID_CANCEL) end}]),

  {Dlg, #state{parent=Parent, dialog=Dlg}}.


handle_event(Ev = #wx{}, State = #state{}) ->
  io:format("Got Event ~p~n",[Ev]),
  {noreply,State}.

handle_info(Msg, State) ->
  io:format( "Got Info ~p~n",[Msg]),
  {noreply,State}.

handle_call(destroy, _From, State=#state{dialog=P}) ->
  io:format("Terminating process, destroying the dialog.~n"),
  wxWindow:destroy(P),
  {stop, normal, ok, State};

handle_call(Msg, _From, State) ->
  io:format("Got Call ~p~n",[Msg]),
  {reply,ok,State}.

handle_cast(Msg, State) ->
  io:format("Got cast ~p~n",[Msg]),
  {noreply,State}.

code_change(_, _, State) ->
  {stop, ignore, State}.

terminate(_Reason, _) ->
  ok.