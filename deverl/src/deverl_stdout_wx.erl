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
%% @doc A textctrl used to display the compiler/dialyzer output.
%% This is used to replicate standard out.
%% @end
%% =====================================================================

-module(deverl_stdout_wx).

-include_lib("wx/include/wx.hrl").
-include("deverl.hrl").

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
%% @hidden
init(Config) ->
	Parent = proplists:get_value(parent, Config),
	Panel = wxPanel:new(Parent),
	MainSizer = wxBoxSizer:new(?wxVERTICAL),
	wxWindow:setSizer(Panel, MainSizer),

  Font0 = deverl_sys_pref_gen:get_font(console_font),
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
%% @hidden
handle_info(Msg, State) ->
  io:format("Got cast ~p~n",[Msg]),
  {noreply, State}.
%% @hidden
handle_cast(clear, State=#state{output=Output}) ->
  wxTextCtrl:setValue(Output, ""), %% keeps the default style
  {noreply, State};
handle_cast(Msg, State=#state{output=Output}) ->
  wxTextCtrl:appendText(Output, Msg),
  {noreply, State}.
%% @hidden
handle_call(Msg, _From, State) ->
  io:format("Got Call ~p~n",[Msg]),
  {reply,ok, State}.
%% @hidden
handle_event(#wx{}, State) ->
	{noreply, State}.
%% @hidden
code_change(_, _, State) ->
	{stop, not_yet_implemented, State}.
%% @hidden
terminate(_Reason, #state{win=Frame}) ->
	wxPanel:destroy(Frame).