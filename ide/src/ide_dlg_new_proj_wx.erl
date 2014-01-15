%% =====================================================================
%% @author Tom Richmond
%% @copyright
%% @title
%% @version
%% @doc Display the New Project dialog.
%% Uses default CANCEL/close_window handlers.
%% @end
%% =====================================================================

-module(ide_dlg_new_proj_wx).

-include_lib("wx/include/wx.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_event/2,
         handle_info/2, code_change/3, terminate/2
]).
         
-export([
  new/1,
  destroy/1,
  get_path/1
]).

%% Server state
-record(state, {frame,
                dlg,
                name_input,
                path_input,
                name_tc,
                path_tc
            	 }).
               

%% =====================================================================
%% Client API
%% =====================================================================

new(Config) ->
  % wx_object:start({local, ?MODULE},?MODULE, Config, []).
  wx_object:start_link(?MODULE, Config, []).

destroy(This) ->
  wx_object:call(This, shutdown).
  
get_path(This) ->
  wx_object:call(This, path).
  
  
%% =====================================================================
%% Callback functions
%% =====================================================================

init(Parent) ->
  Xrc = wxXmlResource:get(),
  Dlg = wxDialog:new(),
  ide_lib_dlg_wx:win_var(Dlg),

  %% Load XRC (Assumes all XRC handlers init previously)
  wxXmlResource:loadDialog(Xrc, Dlg, Parent, "new_project"),
  
  Path = ide_sys_pref_gen:get_preference(project_directory),
  PathTc = wxXmlResource:xrcctrl(Dlg, "path_tc", wxTextCtrl),
  NameTc = wxXmlResource:xrcctrl(Dlg, "name_tc", wxTextCtrl),
  wxTextCtrl:setValue(PathTc, Path),
  wxTextCtrl:connect(NameTc, command_text_updated, [{userData, name}]),
  wxTextCtrl:connect(PathTc, command_text_updated, [{userData, path}]),
  Cb = wxXmlResource:xrcctrl(Dlg, "checkbox", wxCheckBox),
  wxCheckBox:connect(Cb, command_checkbox_clicked),
  
  % browse for directory
  Browse = fun(_EvRec, _Event) ->
    case ide_lib_dlg_wx:get_dir(Parent) of
      cancelled -> ok;
      Path1 -> wxTextCtrl:setValue(PathTc, Path1)
    end
  end,
  Btn = wxXmlResource:xrcctrl(Dlg, "browse_btn", wxButton),
  wxButton:connect(Btn, command_button_clicked,[{callback,Browse}]),
  wxDialog:connect(Dlg, command_button_clicked,[{id,?wxID_OK}]), %% Overide OK handker
        
  State=#state{frame=Parent,
               dlg=Dlg,
               name_tc=NameTc,
               path_tc=PathTc
               },

  {Dlg, State}.
    
handle_event(#wx{id=?wxID_OK, event=#wxCommand{}}, 
             State=#state{dlg=Dlg, name_tc=Tc0, path_tc=Tc1}) ->
  N = wxTextCtrl:getValue(Tc0), 
  P = wxTextCtrl:getValue(Tc1), 
  wxDialog:endModal(Dlg, ?wxID_OK),
  {noreply, State#state{name_input=N, path_input=P}};
  
handle_event(#wx{obj=Cb, event=#wxCommand{type=command_checkbox_clicked}}, 
             State=#state{dlg=Dlg, path_tc=Tc0, name_tc=Tc1, path_input=Input}) ->
  case wxCheckBox:isChecked(Cb) of
    true ->
      Input1 = wxTextCtrl:getValue(Tc0),
    	wxTextCtrl:setValue(Tc0, ide_sys_pref_gen:get_preference(project_directory)), %% Generates command_text_updated event for ?ID_PROJ_PATH
      Name = wxTextCtrl:getValue(Tc1),
    	case length(Name) of
    		0 -> ok;
    		_ -> wxTextCtrl:appendText(Tc0, filename:nativename("/") ++ Name)
    	end,
      wxTextCtrl:disable(Tc0),
      wxWindow:disable(wxXmlResource:xrcctrl(Dlg, "browse_btn", wxButton)),
      {noreply, State#state{path_input=Input1}};
    false ->
    	case Input of %% Generates command_text_updated event for ?ID_PROJ_PATH
    		undefined -> wxTextCtrl:clear(Tc0);
    		Str -> wxTextCtrl:setValue(Tc0, Str)
    	end,
      wxTextCtrl:enable(Tc0, [{enable, true}]),
      wxWindow:enable(wxXmlResource:xrcctrl(Dlg, "browse_btn", wxButton)),
      {noreply, State}
  end;

handle_event(#wx{event=#wxCommand{cmdString=Str}, userData=path}, 
             State=#state{dlg=Dlg, name_tc=Tc0, path_tc=Tc1}) ->  
	Name = wxTextCtrl:getValue(Tc0),
	Path = wxTextCtrl:getValue(Tc1),
	case wxCheckBox:isChecked(wxXmlResource:xrcctrl(Dlg, "checkbox", wxCheckBox)) of
		false when length(Path) =:= 0, length(Name) =:= 0 ->
      % insert_desc(Desc, get_message(Info, both), [{bitmap, wxImageList:getBitmap(ImageList, ?ID_BITMAP_INFO)}]);
      ok;
		false when length(Path) =:= 0 -> %%
      % insert_desc(Desc, get_message(Info, location), [{bitmap, wxImageList:getBitmap(ImageList, ?ID_BITMAP_INFO)}]),
			wxButton:disable(wxXmlResource:xrcctrl(Dlg, "wxID_OK", wxButton));
		_ when length(Name) =:= 0 ->
      % insert_desc(Desc, get_message(Info, name), [{bitmap, wxImageList:getBitmap(ImageList, ?ID_BITMAP_INFO)}]);
      ok;
		_ ->
      % EnableFinish = validate_path(Path, Desc),
      % ValidName = validate_name(InputName, Desc),
			Bool = validate_path(Path, desc) and validate_name(Name, desc),
      % display_message(Desc, Bool),
			wxButton:enable(wxXmlResource:xrcctrl(Dlg, "wxID_OK", wxButton), [{enable, Bool}])
  end,
  {noreply, State};
  
handle_event(#wx{event=#wxCommand{cmdString=Str}, userData=name}, 
             State=#state{dlg=Dlg, name_tc=Tc0, path_tc=Tc1}) ->
  % io:format( "Evt path_tc~n"),
	Name = wxTextCtrl:getValue(Tc0),
	Path = wxTextCtrl:getValue(Tc1),
  Cb = wxXmlResource:xrcctrl(Dlg, "checkbox", wxCheckBox),
	Valid = case wxCheckBox:isChecked(Cb) of
		true when length(Name) =:= 0 ->
      % insert_desc(Desc, get_message(Info, name), [{bitmap, wxImageList:getBitmap(ImageList, ?ID_BITMAP_INFO)}]),
			StartPos = length(ide_sys_pref_gen:get_preference(project_directory)),
			wxTextCtrl:replace(Tc1, StartPos, -1, Str),
			false;
		true ->
			StartPos = length(ide_sys_pref_gen:get_preference(project_directory)),
			wxTextCtrl:replace(Tc1, StartPos, -1, filename:nativename("/") ++ Str),
      validate_name(Str, desc);
		false when length(Name) =:= 0, length(Path) =:= 0 ->
      % insert_desc(Desc, get_message(Info, both), [{bitmap, wxImageList:getBitmap(ImageList, ?ID_BITMAP_INFO)}]),
			false;
		false when length(Name) =:= 0 ->
      % insert_desc(Desc, get_message(Info, name), [{bitmap, wxImageList:getBitmap(ImageList, ?ID_BITMAP_INFO)}]),
			false;	
		false when length(Path) =:= 0 ->
      % insert_desc(Desc, get_message(Info, location), [{bitmap, wxImageList:getBitmap(ImageList, ?ID_BITMAP_INFO)}]),  
			false;
		false -> 
      validate_name(Str, desc)
	end,
	wxButton:enable(wxXmlResource:xrcctrl(Dlg, "wxID_OK", wxButton), [{enable, Valid}]),
  % display_message(Desc, Valid),
  {noreply, State}.
  
handle_info(_Msg, State) ->
  {noreply,State}.

handle_call(path, _From, State) ->
  {reply, State#state.path_input, State};   
handle_call(shutdown, _From, State) ->
  {stop, normal, ok, State}.

handle_cast(_Msg, State) ->
  {noreply,State}.

code_change(_, _, State) ->
  {stop, ignore, State}.

terminate(_Reason, _State) ->
  io:format("Terminating dialog~n"),
  % wxDialog:destroy(State#state.dlg), %% segfault OSX wx3.0 erlR16B03
  ok.

%% =====================================================================
%% Internal functions
%% ====================================================================
  
validate_name([], _) -> false;
validate_name(Str, Desc) ->
	case validate_name(Str) of
		nomatch -> 
			true;
		{match, [{Pos,_}]} -> 
      % Bitmap = wxBitmap:new(wxImage:new("../icons/prohibition.png")),
      % insert_desc(Desc, "Illegal character \"" ++ [lists:nth(Pos + 1, Str)] ++ "\" in filename.", [{bitmap, Bitmap}]),
			false
	end.
validate_name(Str) ->
	re:run(Str, "[/\]").
  
validate_path(Path, desc) -> true.