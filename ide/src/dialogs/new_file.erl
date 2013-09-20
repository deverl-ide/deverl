-module(new_file).
  
-include_lib("wx/include/wx.hrl").
  
-behaviour(wx_object).
  
%% wx_objects callbacks
-export([start/1, init/1, terminate/2, code_change/3, handle_event/2, 
         handle_call/3, handle_cast/2, handle_info/2]).
%% API
-export([new/1]).
  
-record(state, {win}).

  
new(Frame) ->
	start([Frame]).
  
start(Config) ->
	wx_object:start_link(?MODULE, Config, []).
  
init(Args) ->
	Parent = proplists:get_value(parent, Args),
	Frame = wxFrame:new(Parent, ?wxID_ANY, "New File", [{size,{500, 500}}]),
	Panel = wxPanel:new(Frame),
	MainSizer   = wxBoxSizer:new(?wxVERTICAL),
	wxPanel:setSizer(Panel, MainSizer),
  
  
	%%%%%%%%
  
	
  
	wxFrame:centerOnParent(Frame),
	wxFrame:show(Frame),
	wxFrame:raise(Frame),
  
	State = #state{win = Frame},
	{Frame, State}.
  
handle_cast(_Msg, State) ->
	io:format("handle_cast/2: NEW FILE DIALOG"),
	{noreply, State}.
  
handle_info(_Info, State) ->
	io:format("handle_info/2: NEW FILE DIALOG"),
	{noreply, State}.
  
handle_call(shutdown, _From, State=#state{win=Frame}) ->
    wxFrame:destroy(Frame),
    {stop, normal, ok, State}.
  
handle_event(#wx{event = #wxClose{}}, State) ->
	{stop, normal, State};
handle_event(#wx{id = ?wxID_EXIT, event = #wxCommand{type = command_button_clicked}}, State=#state{win=Frame}) ->
	wxFrame:destroy(Frame),
	{stop, normal, State}.
  
code_change(_, _, State) ->
  {stop, not_yet_implemented, State}.
  
terminate(_Reason, #state{win=Frame}) ->
  wxFrame:destroy(Frame).
 







