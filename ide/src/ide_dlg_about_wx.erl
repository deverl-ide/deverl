%% =====================================================================
%% @author
%% @copyright
%% @title
%% @version
%% @doc Displays the "About" dialog.
%% @end
%% =====================================================================

-module(ide_dlg_about_wx).

-include_lib("wx/include/wx.hrl").

-behaviour(wx_object).

%% wx_objects callbacks
-export([init/1, terminate/2, code_change/3, handle_event/2,
         handle_call/3, handle_cast/2, handle_info/2]).
%% API
-export([new/1, destroy/1]).

-record(state, {win}).


%% =====================================================================
%% Client API
%% =====================================================================

%% =====================================================================
%% @doc

-spec new(Parent) -> wxWindow:wxWindow() when
  Parent :: [wxWindow:wxWindow()].

new(Parent) ->
	wx_object:start_link(?MODULE, Parent, []).
  
  
destroy(This) ->
	wx_object:call(This, shutdown).


%% =====================================================================
%% Callback functions
%% =====================================================================


init(Parent) ->
  
  Xrc = wxXmlResource:get(),
  Dlg = wxDialog:new(),
  ide_lib_dlg_wx:win_variant(Dlg),
  wxXmlResource:loadDialog(Xrc, Dlg, Parent, "about"),
  
  Html0 = wxXmlResource:xrcctrl(Dlg, "about_html", wxHtmlWindow),
  wxHtmlWindow:setPage(Html0, about_html()),
  
  wxHtmlWindow:connect(Html0, command_html_link_clicked),

	{Dlg, State=#state{win=Dlg}}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

handle_call(shutdown, _From, State) ->
  {stop, normal, ok, State}.
  
handle_event(#wx{event=#wxHtmlLink{linkInfo=#wxHtmlLinkInfo{href=Href}}}, State) ->
  wx_misc:launchDefaultBrowser(Href),
  {noreply, State}.

code_change(_, _, State) ->
  {ok, State}.

terminate(_Reason, #state{win=Dlg}) ->
  wxDialog:destroy(Dlg).


%% =====================================================================
%% Internal functions
%% =====================================================================

%% =====================================================================
%% @doc

about_html() ->
"
<html>
  <body valign=\"center\">
        
    <div align=\"center\">
      <p><strong>Developers:</strong></p>
      <p>Tom Richmond (tr201@kent.ac.uk)<br \>
      Michael Quested (mdq3@kent.ac.uk)</p>
    </div>
    
    <div align=\"center\">
      <p><strong>Website:</strong></p>
      <p>Hosted at <a href=\"https://github.com/tomrichmond/erlangIDE\">GitHub</a></p>
    <div>
    
    <div align=\"center\">
      <p><strong>Attibutions</strong></p>
      <p>Some icons adapted from Fugue by <a href=\"http://p.yusukekamiyamane.com/\">Yusuke Kamiyamane</a>. Licensed under a Creative Commons Attribution 3.0 License.</p>
    <div>
    
    
  </body>
</html>
".