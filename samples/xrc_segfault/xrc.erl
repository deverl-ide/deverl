%% Minimal sample to reproduce a bug.
%% Will crash with a segmentation fault when the dialog is closed (try it a couple of times)
%% Only occurs when events are attached, crash occurs on the call wxEvtHandler::DoUnbind()
%% Occurs on OSX Mavericks:
%% 64bit R16B03, wx3.0 +
%% 32bit pre-built binary from erlang solutions

-module(xrc).
-compile(export_all).

-include_lib("wx/include/wx.hrl").
-export([init/1, terminate/2,  code_change/3,
	       handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).
         
%% Call this to test
-export([test/0]).

-behaviour(wx_object).

-record(state, {dialog
            	 }).


%% =====================================================================
%% Client API
%% =====================================================================

%% =====================================================================
%% @doc

test() ->
	WX = wx:new(),
	Dlg = start(WX),
	case wxDialog:showModal(Dlg) of
		?wxID_CANCEL ->
			io:format("Dialog closed: wxID_CANCEL.~n");
		?wxID_OK ->
			io:format("Dialog closed: wxID_OKL.~n")
	end,
  destroy(Dlg).

start(Parent) ->
  wx_object:start_link(?MODULE, Parent, []).
  
destroy(This) ->
  wx_object:call(This, destroy).


%% =====================================================================
%% Callback functions
%% =====================================================================

init(Parent) ->
    
  %% Load XRC
  Xrc = wxXmlResource:get(),
  wxXmlResource:initAllHandlers(Xrc),
  true = wxXmlResource:load(Xrc, "derivdlg.xrc"),

  Dlg = wxDialog:new(),
  true = wxXmlResource:loadDialog(Xrc, Dlg, Parent, "derived_dialog"),

  %% Connect an event to a ctrl
  Text = wxXmlResource:xrcctrl(Dlg, "my_textctrl", wxTextCtrl),
  Handler = fun(_E,_O) ->
    ok
  end,
  % wxTextCtrl:connect(Text, command_text_updated), %% Doesn't matter how events are attached, will crash anyway
  wxTextCtrl:connect(Text, command_text_updated, [{callback, Handler}]),

  %% Dialog segfaults when closed (close button/cancel/ok) using default handler or not
  wxDialog:connect(Dlg, close_window, [{callback, fun(_,_) -> 
    io:format("Closing dialog~n"),
    wxDialog:endModal(Dlg, ?wxID_CANCEL) 
  end}]),

  {Dlg, #state{dialog=Dlg}}.


handle_event(Ev = #wx{}, State = #state{}) ->
  io:format("Got Event ~p~n",[Ev]),
  {noreply,State}.

handle_info(Msg, State) ->
  io:format( "Got Info ~p~n",[Msg]),
  {noreply,State}.

handle_call(destroy, _From, State=#state{dialog=Dlg}) ->
  io:format("Terminating process, destroying the dialog.~n"),
  wxWindow:destroy(Dlg),
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