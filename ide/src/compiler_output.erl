%% =====================================================================
%% @author
%% @copyright
%% @title
%% @version
%% @doc A textctrl used to display the compiler output.
%% @end
%% =====================================================================

-module(compiler_output).

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
				 handle_event/2]).

%% API     
-export([new/1,
         append/1]).

%% Server state
-record(state, {win, 
								output}).


%% =====================================================================
%% Client API
%% =====================================================================	

%% =====================================================================
%% @doc

new(Config) ->
	wx_object:start_link({local, ?MODULE}, ?MODULE, Config, []).
	

%% =====================================================================
%% @doc

append(Msg) ->
  wx_object:cast(?MODULE, Msg).


%% =====================================================================
%% Callback functions
%% =====================================================================

init(Config) ->
	Parent = proplists:get_value(parent, Config),
	Panel = wxPanel:new(Parent),
	MainSizer = wxBoxSizer:new(?wxVERTICAL),
	wxWindow:setSizer(Panel, MainSizer),

	Output = wxTextCtrl:new(Panel, ?wxID_ANY, [{style, ?wxBORDER_NONE bor ?wxTE_DONTWRAP bor ?wxTE_READONLY bor ?wxTE_MULTILINE}]),
  wxTextCtrl:setFont(Output, wxFont:new(11, ?wxFONTFAMILY_TELETYPE, ?wxNORMAL, ?wxNORMAL,[])),
  wxTextCtrl:setForegroundColour(Output, ?wxBLACK),
  wxTextCtrl:setBackgroundColour(Output, ?wxWHITE),
                                                	
	wxSizer:add(MainSizer, Output, [{flag, ?wxEXPAND}, {proportion, 1}]),
                                        
	State=#state{win=Panel, 
				       output=Output},
               
  %% Note this stops the samll square artifact from appearing in top left corner.
  wxSizer:layout(MainSizer),
  
  {Panel, State}.

handle_info(Msg, State) ->
  io:format("Got cast ~p~n",[Msg]),
  {noreply, State}.

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
	io:format("TERMINATE LOG~n"),
	wxPanel:destroy(Frame).

	
%% =====================================================================
%% Internal functions
%% =====================================================================