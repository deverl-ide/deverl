-module(user_prefs).

-export([new/1,
	stop/0,
	get_user_pref/1,
	set_user_pref/2]).

-export([start/1, 
	init/1, 
	terminate/2, 
	code_change/3,
	handle_call/3, 
	handle_cast/2, 
	handle_info/2]).

-include_lib("wx/include/wx.hrl").
-include("../include/ide.hrl").

-behaviour(gen_server).

-record(state, {theme,
								font,
								show_line_no,
								line_wrap,
								auto_indent,
								indent_type,
								indent_width
								}).


new(Config) ->
	start(Config).

start(Config) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

init(Config) ->
  WxEnv = proplists:get_value(wxe_server, Config),
	wx:set_env(WxEnv),
	
	%% Load the user preferences(into Mnesia maybe?)
	%% Any preferences that are not set should be set to the default value.
	
	%% Defining them here as constants for testing only.
	State = #state{font = wxFont:new(?DEFAULT_FONT_SIZE, ?wxFONTFAMILY_TELETYPE, ?wxNORMAL, ?wxNORMAL,[]),
								 theme = "Putty",
								 show_line_no = false,
								 line_wrap = true,
								 auto_indent = false,
								 indent_type = "Spaces",
								 indent_width = "7" %% String
								 },
	{ok, State}.

handle_cast(stop, _) ->
	{stop, normal, ok};
handle_cast({theme,Pref}, State) ->
	{noreply, State#state{theme=Pref}};
handle_cast({font,Pref}, State) ->
	{noreply, State#state{font=Pref}};
handle_cast({show_line_no,Pref}, State) ->
	{noreply, State#state{show_line_no=Pref}};
handle_cast({line_wrap,Pref}, State) ->
	{noreply, State#state{line_wrap=Pref}};
handle_cast({auto_indent,Pref}, State) ->
	{noreply, State#state{auto_indent=Pref}};
handle_cast({indent_type,Pref}, State) ->
	{noreply, State#state{indent_type=Pref}};
handle_cast({indent_width,Pref}, State) ->
	{noreply, State#state{indent_width=Pref}};
handle_cast(_Req, State) ->
	io:format("handle_cast: user_prefs"),
	{noreply, State}.

handle_info(_Info, State) ->
	io:format("handle_info/2: user_prefs"),
	{noreply, State}.

handle_call(shutdowm, _From, State) ->
	{stop, normal, ok, State};
handle_call(theme, _From, State=#state{theme=Theme}) ->
	{reply, Theme, State};
handle_call(font, _From, State=#state{font=Font}) ->
	{reply, Font, State};
handle_call(show_line_no, _From, State=#state{show_line_no=Bool}) ->
	{reply, Bool, State};
handle_call(line_wrap, _From, State=#state{line_wrap=Bool}) ->
	{reply, Bool, State};
handle_call(auto_indent, _From, State=#state{auto_indent=Bool}) ->
	{reply, Bool, State};
handle_call(indent_type, _From, State=#state{indent_type=Res}) ->
	{reply, Res, State};
handle_call(indent_width, _From, State=#state{indent_width=Width}) ->
	{reply, Width, State};
handle_call(_Req, _From, State) ->
	io:format("handle_call/2: user_prefs"),
	{noreply, State}.

code_change(_, _, State) ->
	{stop, not_yet_implemented, State}.

terminate(_Reason, _) ->
	io:format("TERMINATE USER_PREFS~n").
		
stop() ->
	gen_server:cast(?MODULE, stop).
		
get_user_pref({pref,Type}) ->
	gen_server:call(?MODULE, Type).
	
set_user_pref(Type, Pref) ->
	gen_server:cast(?MODULE, {Type,Pref}).