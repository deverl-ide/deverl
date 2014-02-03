%% =====================================================================
%% @author
%% @copyright
%% @title
%% @version
%% @doc A textctrl used to display the compiler output.
%% @end
%% =====================================================================

-module(ide_stdout_wx).

-include_lib("wx/include/wx.hrl").
-include("ide.hrl").

%% wx_object
-behaviour(wx_object).
-export([init/1,
		 		 terminate/2,
				 code_change/3,
				 handle_info/2,
				 handle_call/3,
				 handle_cast/2,
				 handle_event/2
         ]).

%% API
-export([new/1,
         append/1,
         append_header/1,
         clear/0,
         append_footer/0,
         append_footer/1
         ]).

%% Server state
-record(state, {win,
								output}).

-define(OUT_WIDTH, 60).

%% =====================================================================
%% Client API
%% =====================================================================

%% =====================================================================
%% @doc

-spec new(list()) -> wx_object:wx_object().

new(Config) ->
	wx_object:start_link({local, ?MODULE}, ?MODULE, Config, []).


%% =====================================================================
%% @doc

-spec append_header(string()) -> ok.

append_header(Msg) ->
  append_fixed_width(Msg).


%% =====================================================================
%% @doc

-spec append_footer() -> ok.

append_footer() ->
  append_footer("Complete").
  
-spec append_footer(string()) -> ok.

append_footer(Msg) ->
  append_fixed_width(Msg).
 
 
%% =====================================================================
%% @doc Append text of fixed width to output. Width is defined
%% in the OUT_WIDTH macro.

-spec append_fixed_width(string()) -> ok.
 
append_fixed_width(Msg) ->
  Op = round(?OUT_WIDTH/2) + round(length(Msg)/2),
  Msg1 = io_lib:format([126] ++ integer_to_list(?OUT_WIDTH)
                            ++ "." ++ integer_to_list(Op)
                            ++ ".=s~n", [Msg]),
  append(Msg1).
  
  
%% =====================================================================
%% @doc

-spec append(string()) -> ok.

append(Msg) ->
  wx_object:cast(?MODULE, Msg).
  

%% =====================================================================
%% @doc

-spec clear() -> ok.

clear() ->
  wx_object:cast(?MODULE, clear).
  

%% =====================================================================
%% Callback functions
%% =====================================================================

init(Config) ->
	Parent = proplists:get_value(parent, Config),
	Panel = wxPanel:new(Parent),
	MainSizer = wxBoxSizer:new(?wxVERTICAL),
	wxWindow:setSizer(Panel, MainSizer),

  Font0 = ide_sys_pref_gen:get_font(console_font),
  Font1 = wxFont:new(wxFont:getPointSize(Font0), ?wxFONTFAMILY_TELETYPE, ?wxNORMAL, ?wxNORMAL),
  Style = wxTextAttr:new(?wxBLACK, [{font, Font1}]),

	Output = wxTextCtrl:new(Panel, ?WINDOW_OUTPUT, [{style, ?wxBORDER_NONE bor ?wxTE_DONTWRAP bor ?wxTE_RICH2 bor ?wxTE_READONLY bor ?wxTE_MULTILINE}]),
  wxTextCtrl:setDefaultStyle(Output, Style),

	wxSizer:add(MainSizer, Output, [{flag, ?wxEXPAND}, {proportion, 1}]),

	State=#state{win=Panel,
				       output=Output},

  %% Note this stops the small square artifact from appearing in top left corner.
  wxSizer:layout(MainSizer),

  {Panel, State}.

handle_info(Msg, State) ->
  io:format("Got cast ~p~n",[Msg]),
  {noreply, State}.

handle_cast(clear, State=#state{output=Output}) ->
  wxTextCtrl:setValue(Output, ""), %% keeps the default style
  {noreply, State};
handle_cast(Msg, State=#state{output=Output}) ->
  wxTextCtrl:appendText(Output, Msg),
  {noreply, State}.

handle_call(Msg, _From, State) ->
  io:format("Got Call ~p~n",[Msg]),
  {reply,ok, State}.

handle_event(#wx{}, State) ->
	{noreply, State}.

code_change(_, _, State) ->
	{stop, not_yet_implemented, State}.

terminate(_Reason, #state{win=Frame}) ->
	wxPanel:destroy(Frame).