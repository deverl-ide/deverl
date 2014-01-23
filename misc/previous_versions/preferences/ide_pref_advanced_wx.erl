%% =====================================================================
%% @author
%% @copyright
%% @title
%% @version
%% @doc 
%% @end
%% =====================================================================

-module(ide_pref_advanced_wx).

-include_lib("wx/include/wx.hrl").
-include("ide.hrl").

-behaviour(wx_object).

-export([start/1, init/1, terminate/2,  code_change/3,
	       handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).

-record(state, {parent,
            	  config
            	 }).
                           
%% =====================================================================
%% Client API
%% =====================================================================

%% =====================================================================
%% @doc

-spec start(Config) -> wxWindow:wxWindow() when
  Config :: list().

start(Config) ->
  wx_object:start_link(?MODULE, Config, []).


%% =====================================================================
%% Callback functions
%% =====================================================================

init(Config) ->
  wx:batch(fun() -> do_init(Config) end).

do_init(Config) ->
  Parent = proplists:get_value(parent, Config),
  Xrc = wxXmlResource:get(),
  Panel0 = wxPanel:new(),
  wxXmlResource:loadPanel(Xrc, Panel0, Parent, "prefs"),
  
  %% Dialyzer 
  DlzrOptions = ide_sys_pref_gen:get_preference(dialyzer_options),
  
  DlzrPlt = wxXmlResource:xrcctrl(Panel0, "dial_plt", wxStaticText),
  DlzrPltBtn = wxXmlResource:xrcctrl(Panel0, "dial_btn_browse", wxButton),
  DlzrIncs = wxXmlResource:xrcctrl(Panel0, "dial_include", wxListCtrl),
  DlzrRmvDir = wxXmlResource:xrcctrl(Panel0, "dial_btn_remove_dir", wxButton),
  DlzrAddDir = wxXmlResource:xrcctrl(Panel0, "dial_btn_add_dir", wxButton),
  DlzrOut0 = wxXmlResource:xrcctrl(Panel0, "dial_verbose_out", wxCheckBox),
  DlzrOut1 = wxXmlResource:xrcctrl(Panel0, "dial_stats_out", wxCheckBox),
  DlzrOut2 = wxXmlResource:xrcctrl(Panel0, "dial_quiet_out", wxCheckBox),
  
  wxStaticText:setLabel(DlzrPlt, DlzrOptions#dialyzer_options.plt),
  wxCheckBox:setValue(DlzrOut0, DlzrOptions#dialyzer_options.verbose_out),
  wxCheckBox:setValue(DlzrOut1, DlzrOptions#dialyzer_options.stats_out),
  wxCheckBox:setValue(DlzrOut2, DlzrOptions#dialyzer_options.quiet_out),

  {Panel0, #state{parent=Panel0}}.


handle_event(Ev = #wx{}, State = #state{}) ->
  io:format("Got Event ~p~n",[Ev]),
  {noreply,State}.

handle_info(Msg, State) ->
  io:format( "Got Info ~p~n",[Msg]),
  {noreply,State}.

handle_call(shutdown, _From, State=#state{parent=Panel}) ->
  wxWindow:destroy(Panel),
  {stop, normal, ok, State};

handle_call(Msg, _From, State) ->
  io:format("Got Call ~p~n",[Msg]),
  {reply,ok,State}.

handle_cast(Msg, State) ->
  io:format("Got cast ~p~n",[Msg]),
  {noreply,State}.

code_change(_, _, State) ->
  {ok, State}.

terminate(_Reason, _) ->
  ok.