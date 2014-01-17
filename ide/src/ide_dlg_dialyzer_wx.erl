%% =====================================================================
%% @author
%% @copyright
%% @title
%% @version
%% @doc
%% @end
%% =====================================================================

-module(ide_dlg_dialyzer_wx).

-include_lib("wx/include/wx.hrl").

%% wx_object
-behaviour(wx_object).
-export([init/1, terminate/2, code_change/3,
	       handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).

%% API
-export([
  new/2,
  set_focus/1,
  get_files/1,
  destroy/1
  ]).

%% Server state
-record(state, {dlg,
                files %% Selected src files
            	 }).


%% =====================================================================
%% Client API
%% =====================================================================

new(Parent, Config) ->
  wx_object:start(?MODULE, {Parent, Config}, []).

set_focus(This) ->
	wx_object:cast(This, setfocus).

-spec get_files(wxDialog:wxDialog()) -> [file:filename()].

get_files(This) ->
	wx_object:call(This, files).

destroy(This) ->
	wx_object:call(This, shutdown).


%% =====================================================================
%% Callback functions
%% =====================================================================

init({Parent, Config}) ->
  Projects = proplists:get_value(projects, Config),
  Stdln = proplists:get_value(standalone, Config),

  Xrc = wxXmlResource:get(),
  Dlg = wxDialog:new(),
  ide_lib_dlg_wx:win_var(Dlg),
  wxXmlResource:loadDialog(Xrc, Dlg, Parent, "dialyzer"),

  Choice = wxXmlResource:xrcctrl(Dlg, "source", wxChoice),
  Add =  fun(Id) ->
    wxChoice:append(Choice, ide_proj_man:get_name(Id), Id)
  end,
  lists:map(Add, Projects),

  case Stdln of
    [] -> ok;
    _ -> wxChoice:append(Choice, "Standalone Files")
  end,

  case Projects of
    [] when Stdln =:= [] -> ok;
    _ -> wxChoice:append(Choice, "All")
  end,
  wxChoice:connect(Choice, command_choice_selected),

  Listbox0 = wxXmlResource:xrcctrl(Dlg, "listbox0", wxListBox),
  Listbox1 = wxXmlResource:xrcctrl(Dlg, "listbox1", wxListBox),
  Btn0 = wxXmlResource:xrcctrl(Dlg, "move_right", wxButton),
  Btn1 = wxXmlResource:xrcctrl(Dlg, "move_left", wxButton),

  %% TESTING
  try
    Fls = ide_proj_man:get_project_src_files(hd(Projects)),
    Insert = fun(Str) ->
      wxListBox:append(Listbox0, filename:basename(Str), Str)
    end,
    lists:map(Insert, Fls)
  catch
    _:_ -> ok
  end,

  wxButton:connect(Btn0, command_button_clicked, [{userData, right}]),
  wxButton:connect(Btn1, command_button_clicked, [{userData, left}]),
  wxListBox:connect(Listbox0, command_listbox_selected, [{userData, 0}]),
  wxListBox:connect(Listbox1, command_listbox_selected, [{userData, 1}]),
  wxDialog:connect(Dlg, command_button_clicked, [{id, ?wxID_OK}]), %% overide default handler

	State = #state{
		dlg=Dlg
	},
	{Dlg, State}.


handle_event(#wx{id=?wxID_OK=Id, event=#wxCommand{type=command_button_clicked}}, State) ->
  io:format("thing"),
  Listbox1 = wxXmlResource:xrcctrl(State#state.dlg, "listbox1", wxListBox),
  GetData = fun(E) ->
    wxListBox:getClientData(Listbox1, E)
  end,
  Files = lists:map(GetData, lists:seq(0, wxListBox:getCount(Listbox1) - 1)),
  wxDialog:endModal(State#state.dlg, Id),
  {noreply, State#state{files=Files}};

handle_event(#wx{event=#wxCommand{type=command_choice_selected}}, State) ->
  io:format("CHOICE EVENT~n"),
  {noreply, State};

handle_event(#wx{event=#wxCommand{type=command_button_clicked}, userData=UD}, State) ->
  Listbox0 = wxXmlResource:xrcctrl(State#state.dlg, "listbox0", wxListBox),
  Listbox1 = wxXmlResource:xrcctrl(State#state.dlg, "listbox1", wxListBox),
  case UD of
    left ->
      move(Listbox1, Listbox0),
      deselect_all(Listbox1);
    right ->
      move(Listbox0, Listbox1),
      deselect_all(Listbox0),
      wxWindow:enable(wxXmlResource:xrcctrl(State#state.dlg, "wxID_OK", wxButton))
  end,
  {noreply, State};

handle_event(#wx{event=#wxFocus{type=kill_focus}, userData=0}, State) ->
  wxWindow:disable(wxXmlResource:xrcctrl(State#state.dlg, "move_right", wxButton)),
  {noreply, State};

handle_event(#wx{event=#wxFocus{type=kill_focus}, userData=1}, State) ->
  wxWindow:disable(wxXmlResource:xrcctrl(State#state.dlg, "move_left", wxButton)),
  {noreply, State};

handle_event(#wx{event=#wxFocus{type=set_focus}, userData=0}, State) ->
  deselect_all(wxXmlResource:xrcctrl(State#state.dlg, "listbox1", wxListBox)),
  {noreply, State};

handle_event(#wx{event=#wxFocus{type=set_focus}, userData=1}, State) ->
  deselect_all(wxXmlResource:xrcctrl(State#state.dlg, "listbox0", wxListBox)),
  {noreply, State};

handle_event(#wx{event=#wxCommand{type=command_listbox_selected}, userData=0}, State) ->
  wxWindow:enable(wxXmlResource:xrcctrl(State#state.dlg, "move_right", wxButton)),
  {noreply, State};

handle_event(#wx{event=#wxCommand{type=command_listbox_selected}, userData=1}, State) ->
  wxWindow:enable(wxXmlResource:xrcctrl(State#state.dlg, "move_left", wxButton)),
  {noreply, State}.

handle_info(_Msg, State) ->
  {noreply,State}.

handle_call(files, _From, State) ->
  {reply, State#state.files, State};
handle_call(shutdown, _From, State) ->
  {stop, normal, ok, State}.

handle_cast(_, State) ->
  {noreply,State}.

code_change(_, _, State) ->
  {stop, ignore, State}.

terminate(_Reason, _State) ->
  % wxDialog:destroy(State#state.dlg),  %% segfault OSX wx3.0 erlR16B03 (see xrc sample directory)
	ok.


%% =====================================================================
%% Internal functions
%% =====================================================================

%% =====================================================================
%% @doc

-spec deselect_all(wxListBox:wxListBox()) -> ok.

deselect_all(Listbox) ->
  Deselect = fun(Idx) ->
    wxListBox:deselect(Listbox, Idx)
  end,
  {_,Slcd} = wxListBox:getSelections(Listbox),
  lists:map(Deselect, Slcd),
  ok.


%% =====================================================================
%% @doc

-spec move(From, To) -> ok when
  From :: wxListBox:wxListBox(),
  To :: wxListBox:wxListBox().

move(From, To) ->
  {_Total, Idxs} = wxListBox:getSelections(From),
  Move = fun(Idx, Acc) ->
    Cd = wxListBox:getClientData(From, Idx-Acc),
    wxListBox:append(To, wxListBox:getString(From, Idx-Acc), Cd),
    wxListBox:delete(From, Idx-Acc),
    Acc + 1
  end,
  lists:foldl(Move, 0, Idxs),
  ok.
