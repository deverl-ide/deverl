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
%% @doc Displays the XRC based <em>Import Project</em> dialog.
%% @end
%% =====================================================================

-module(deverl_dlg_import_proj_wx).

-include_lib("wx/include/wx.hrl").

%% wx_object
-behaviour(wx_object).
-export([init/1, terminate/2, code_change/3,
	       handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).
				 
%% API
-export([
  new/1,
  get_path/1,
  copy_dir/1,
  set_focus/1,
  destroy/1
  ]).
	
%% Server state			 
-record(state, {dialog,
                input,
                copy_dir :: boolean()
            	 }).



%% =====================================================================
%% Client API
%% =====================================================================

new(Parent) ->
  wx_object:start({local, ?MODULE}, ?MODULE, Parent, []).
  
get_path(This) ->
  wx_object:call(This, path).

copy_dir(This) ->
  wx_object:call(This, copy).
  
set_focus(This) ->
	wx_object:cast(This, setfocus).
	
destroy(This) ->
	wx_object:call(This, shutdown).
	
	
%% =====================================================================
%% Callback functions
%% =====================================================================
%% @hidden
init(Parent) ->
  wx:batch(fun() -> do_init(Parent) end).

do_init(Parent) ->
  Xrc = wxXmlResource:get(),
  Dlg = wxDialog:new(),
  deverl_lib_dlg_wx:win_variant(Dlg),
  wxXmlResource:loadDialog(Xrc, Dlg, Parent, "import_proj"),

  Input = wxXmlResource:xrcctrl(Dlg, "import_input_tc", wxTextCtrl),
  wxTextCtrl:connect(Input, command_text_updated),
  
  Browse = wxXmlResource:xrcctrl(Dlg, "import_browse_btn", wxButton),
  DirPicker = fun(_E, _O) ->
    case deverl_lib_dlg_wx:get_dir(Parent) of
      cancelled -> ok;
      Path -> 
        wxTextCtrl:setValue(Input, Path),
        wxTextCtrl:setInsertionPointEnd(Input)
    end
  end,
  wxButton:connect(Browse, command_button_clicked, [{callback, DirPicker}]),
  
  wxDialog:connect(Dlg, command_button_clicked, [{id, ?wxID_OK}]),
    
	State = #state{
		dialog=Dlg
	},
  
	{Dlg, State}.
      
%% =====================================================================
%% Event callbacks
%% =====================================================================
%% @hidden
handle_event(#wx{event=#wxCommand{type=command_text_updated, cmdString=Str}}, State) ->
  case validate(Str) of
    false ->
      %% write notice to description 
      wxWindow:disable(wxWindow:findWindowById(?wxID_OK));
    true ->
      wxWindow:enable(wxWindow:findWindowById(?wxID_OK))
  end,
  {noreply, State#state{input=Str}};

handle_event(#wx{id=?wxID_OK, event=#wxCommand{type=command_button_clicked}}, 
             State=#state{dialog=Dlg}) ->
  Cb = wxXmlResource:xrcctrl(Dlg, "import_copy_cb", wxCheckBox),
  wxDialog:endModal(Dlg, ?wxID_OK),
  {noreply, State#state{copy_dir=wxCheckBox:isChecked(Cb)}}.
%% @hidden
handle_info(Msg, State) ->
  {noreply,State}.
%% @hidden
handle_call(path, _From, State) ->
  {reply, State#state.input, State};
handle_call(copy, _From, State) ->
  {reply, State#state.copy_dir, State};
handle_call(shutdown, _From, State) ->
  {stop, normal, ok, State}.
%% @hidden
handle_cast(setfocus, State) ->
  % wxWindow:setFocus(Tc),
  {noreply,State}.
%% @hidden
code_change(_, _, State) ->
  {stop, ignore, State}.
%% @hidden
terminate(_Reason, #state{dialog=Dialog}) ->
	wxDialog:destroy(Dialog),
  ok.
	

%% =====================================================================
%% Internal functions
%% =====================================================================

%% @doc Validate this is a directory

validate([]) -> false;
validate(Str) ->
  filelib:is_dir(Str).
  
	
%% =====================================================================
%% @doc Display information to the user within the 'Description' box.	

insert_desc(Description, Msg) ->
	insert_desc(Description, Msg, []).

insert_desc(Description, Msg, Options) ->
	SzFlags = wxSizerFlags:new([{proportion, 0}]),
	wxSizerFlags:expand(wxSizerFlags:border(SzFlags, ?wxTOP, 10)),
	wxWindow:freeze(Description),
	wxPanel:destroyChildren(Description),
	Sz = wxBoxSizer:new(?wxHORIZONTAL),
	wxPanel:setSizer(Description, Sz),
	wxSizer:addSpacer(Sz, 10),
	case proplists:get_value(bitmap, Options) of
		undefined -> ok;
		Bitmap -> 
			wxSizer:add(Sz, wxStaticBitmap:new(Description, ?wxID_ANY, Bitmap), [{border, 5}, {flag, ?wxTOP bor ?wxRIGHT}])
	end,
	wxSizer:add(Sz, wxStaticText:new(Description, ?wxID_ANY, Msg), SzFlags),
	wxPanel:layout(Description),	
	wxWindow:thaw(Description),
	ok.