%% =====================================================================
%% @author Tom Richmond
%% @copyright
%% @title
%% @version
%% @doc 
%% This module was written to test the suitability of using XRC for
%% dialogs. The XRC is contained within rc/dlgs.xrc.
%% Currently there is a bug which causes a seg fault on OSX (see
%% terminate/2), which will probably prevent us from using this method.
%% This module is a exact replacement of ide_dlg_new_proj_wx.erl.
%% Uses default close_window/wxID_CANCEL handlers.
%% @end
%% =====================================================================

-module(dlgxrc).

-include_lib("wx/include/wx.hrl").

-compile(export_all).

%% Server state
-record(state, {frame,
                dlg,
                name_input,
                path_input,
                name_tc,
                path_tc
            	 }).

%% Macros							 
-define(ID_BROWSE, 200).

%% TESTING segfaults
test_get_dlg(This) ->
  wx_object:call(This, dlg).

%% =====================================================================
%% Client API
%% =====================================================================

new(Config) ->
  % wx_object:start({local, ?MODULE},?MODULE, Config, []).
  wx_object:start_link(?MODULE, Config, []).

start(Config) ->
  wx_object:start({local, ?MODULE},?MODULE, Config, []).

destroy(This) -> %% was stop/1
  wx_object:call(This, shutdown).
  
%% =====================================================================
%% Callback functions
%% =====================================================================

init(Config) ->
  % wx:batch(fun() -> do_init(Config) end),
  do_init(Config).

do_init(Config) ->
  Parent = Config,
  
  Xrc = wxXmlResource:get(),
  % Dlg = wxDialog:new(),
  % dlg_ld:win_var(Dlg),

  %% Load XRC (Assumes all XRC handlers init previously)
  Dlg = wxXmlResource:loadDialog(Xrc, Parent, "new_project"),
  
  Path = ide_sys_pref_gen:get_preference(project_directory),
  PathTc = wxXmlResource:xrcctrl(Dlg, "path_tc", wxTextCtrl),
  NameTc = wxXmlResource:xrcctrl(Dlg, "name_tc", wxTextCtrl),
  io:format("NAMETC: ~p~n", [NameTc]),
    
  Dir  = wxTextCtrl:new(Dlg, ?wxID_ANY, []),
  wxTextCtrl:connect(Dir, command_text_updated),
  
  wxTextCtrl:setValue(PathTc, Path), %% Callbacks still get seg fault, see terminate/2
  Fn = fun(_E,_O) ->
    io:format("RAAA!")
  end,
  % wxTextCtrl:connect(NameTc, command_text_updated, [{callback,Fn}]),
  % wxTextCtrl:connect(NameTc, command_text_updated), % segfault
  % wxTextCtrl:connect(NameTc, command_text_enter), % segfault
  wxTextCtrl:connect(NameTc, set_focus), % segfault

  %% checkbox callback
  OnMyCheckBox = fun(_EvRec, _Event) ->
    CheckB = wxXmlResource:xrcctrl(Dlg, "checkbox", wxCheckBox),
    Bool = wxCheckBox:isChecked(CheckB),
    wxTextCtrl:enable(PathTc, [{enable, not Bool}])
  end,
  wxDialog:connect(Dlg,update_ui,[{id,wxXmlResource:getXRCID("checkbox")},
            {callback,OnMyCheckBox}]),
              
  %% Keep updateUI event interval at 250ms
  wxUpdateUIEvent:setUpdateInterval(250),
  
  % browse for directory
  Browse = fun(_EvRec, _Event) ->
    case ide_lib_dlg_wx:get_dir(Parent) of
      cancelled -> ok;
      Path1 -> wxTextCtrl:setValue(PathTc, Path1)
    end
  end,
  wxDialog:connect(Dlg, command_button_clicked, 
        [{id,wxXmlResource:getXRCID("browse_btn")}]),
        
  State=#state{frame=Parent,
               dlg=Dlg,
               name_tc=NameTc,
               path_tc=PathTc
               },

  {Dlg, State}.

handle_event(#wx{event=#wxCommand{type=command_button_clicked}}, State) ->
  io:format("command_button_clicked~n"),
  {noreply, State};
handle_event(#wx{event=#wxCommand{type=command_text_updated, cmdString=Wanted}}, State) ->
  io:format("command_text_updated~n"),
  {noreply, State};
handle_event(#wx{event=#wxFocus{}}, State) ->
 io:format("set_focus~n"),
 {noreply, State}.
    
% handle_event(#wx{id=?wxID_OK, event=#wxCommand{}}, 
%              State=#state{dlg=Dlg, name_tc=Tc0, path_tc=Tc1}) ->
%    N = wxTextCtrl:getValue(Tc0), 
%   P = wxTextCtrl:getValue(Tc1), 
%   wxDialog:endModal(Dlg, ?wxID_OK),
%   {noreply, State#state{name_input=N, path_input=P}};
% handle_event(#wx{event=#wxCommand{}, userData=name_tc}, State) ->
%   io:format( "Evt name_tc~n"),
%   {noreply, State};
% handle_event(#wx{event=#wxCommand{type=command_text_updated, cmdString=_Str}, userData=path_tc}, 
% % handle_event(#wx{event=#wxCommand{}, userData=path_tc}, 
%              State=#state{dlg=_Dlg, name_tc=_Tc0, path_tc=_Tc1}) ->
%   io:format( "Evt path_tc~n"),
%   {noreply, State}.
  
handle_info(_Msg, State) ->
  {noreply,State}.

%% TEST
handle_call(dlg, _From, State) ->
  {reply, State#state.dlg, State};
   
handle_call(shutdown, _From, State) ->
  {stop, normal, ok, State}.

handle_cast(_Msg, State) ->
  {noreply,State}.

code_change(_, _, State) ->
  {stop, ignore, State}.

terminate(_Reason, State) ->

  %% NOTE!!!! On OSX the call to destroy cause a seg fault when using
  %% XRC. (when an event is connected to an xrc textctrl object.
  %% This means we can't safely destroy the object (memory leak).
  
  %% This is  a bummer. It only occurs in this instance when the 
  %% command_text_updated event handler is attached. So its possible
  %% an event is received after it being destroyed. Tried to debug
  %% in GDB.
  wxDialog:destroy(State#state.dlg),
  ok.
