%% =====================================================================
%% @author
%% @copyright
%% @title
%% @version
%% @doc This module contains library functions to display common dialogs.
%% @end
%% =====================================================================

-module(deverl_lib_dlg_wx).

-include_lib("wx/include/wx.hrl").

%% API
-export([
	get_dir/1,
	get_existing_dir/1,
	message_quick/3,
  win_variant/1,
  message/2
  ]).
	
-define(ERROR_CAPTION, "The operation could not continue.").
-define(ID_DISCARD_CHANGES, 12345).

%% =====================================================================
%% Client API
%% =====================================================================

%% =====================================================================
%% @doc Get a directory from the user.

get_dir(Parent) ->
	dir_dialog(Parent, []).


%% =====================================================================
%% @doc Get an existing directory from the user.

get_existing_dir(Parent) ->
	dir_dialog(Parent, [{style, ?wxDD_DIR_MUST_EXIST}]).


%% =====================================================================
%% @doc Set the window variant on OSX. 
	
win_variant(Dlg) ->
  %% Smaller dialogs etc look better on OSX
  case os:type() of
    {_, darwin} ->
      wxDialog:setWindowVariant(Dlg, ?wxWINDOW_VARIANT_SMALL);
     _ -> ok
  end.
  

%% =====================================================================
%% @doc Display an error message to the user, with a single OK button.

message_quick(Parent, Caption, Msg) ->
  Dlg = message(Parent, [{caption, Caption}, {text1, Msg}, {buttons, [?wxID_OK]}]),
  wxDialog:showModal(Dlg),
  wxDialog:destroy(Dlg).
  

%% =====================================================================
%% @doc Display a customised (standard application formatting) 
%% message dialog.
%% If you end up with buttons in strange positions. You are
%% probably using an incorrect cobination of button id's.
%% Throws an error if an invalid button id is given.
%% showModal() will return the Id of the clicked button.

-spec message(Parent, [Option]) -> Result when
  Parent :: wxWindow:wxWindow() | wx:null(),
  Option :: {button, [Id]} |
            {caption, string()} |
            {text1, string() | {list, [string()]}} |
            {text2, string() | {list, [string()]}},
  Result :: wxDialog:wxDialog(),
  Id :: ?wxID_OK | ?wxID_YES | ?wxID_SAVE | ?wxID_APPLY | ?wxID_NO | ?wxID_CANCEL | ?wxID_HELP | ?wxID_CONTEXT_HELP.
  
message(Parent, Options) ->
      
  %% fold over the buttons, ensure they're valid
  Vld = fun(?wxID_OK, Acc) -> [?wxID_OK | Acc ];
           (?wxID_YES, Acc) -> [?wxID_YES | Acc ];
           (?wxID_SAVE, Acc) -> [?wxID_SAVE | Acc ];
           (?wxID_APPLY, Acc) -> [?wxID_APPLY | Acc ];
           (?wxID_NO, Acc) -> [?wxID_NO | Acc ];
           (?wxID_CANCEL, Acc) -> [?wxID_CANCEL | Acc ];
           (?wxID_HELP, Acc) -> [?wxID_HELP | Acc ];
           (?wxID_CONTEXT_HELP, Acc) -> [?wxID_CONTEXT_HELP | Acc ];
           (B, _Acc) -> error({invalid_button, B}) end,
           
  Buttons = lists:foldl(Vld, [], proplists:get_value(buttons, Options)),
  
  Xrc = wxXmlResource:get(),
  Dlg = wxDialog:new(),
  deverl_lib_dlg_wx:win_variant(Dlg),
  wxXmlResource:loadDialog(Xrc, Dlg, wx:null(), "message"),
  
  Panel = wxXmlResource:xrcctrl(Dlg, "panel", wxPanel),
  Sz = wxPanel:getSizer(Panel),
  
  %% The width at width to wrap the static text
  Wrap = 350,
  
  SetText = fun(StatTxt, Str) ->
    wxStaticText:setLabel(StatTxt, Str),
    wxStaticText:wrap(StatTxt, Wrap),
    wxStaticText:show(StatTxt)
  end,
  
  %% caption
  case proplists:get_value(caption, Options) of
    undefined -> ok;
    Caption -> 
      CapSt = wxXmlResource:xrcctrl(Dlg, "caption", wxStaticText),
      SetText(CapSt, Caption),
      wxStaticText:show(CapSt)
  end,

  AddText = fun(Name) ->
    NameStr = atom_to_list(Name),
    case proplists:get_value(Name, Options) of
      undefined -> ok;
      {list, Items} ->
        Text1 = wxXmlResource:xrcctrl(Dlg, NameStr, wxStaticText),
        Flat = string:strip(lists:flatten([Item ++ io_lib:nl() || Item <- Items]), both, $\n),
        SetText(Text1, Flat);
      Str -> 
        Text1 = wxXmlResource:xrcctrl(Dlg, NameStr, wxStaticText),
        SetText(Text1, Str)
    end
  end,

  %% text 1
  AddText(text1),
  
  %% text 2
  AddText(text2),

  %% add buttons
  BtnSz = wxStdDialogButtonSizer:new(),
  AddBtn = fun(ID) ->
    Btn = wxButton:new(Panel, ID),
    wxStdDialogButtonSizer:addButton(BtnSz, Btn),
    wxButton:connect(Btn, command_button_clicked, 
      [{callback, fun(_E,_O) -> wxDialog:endModal(Dlg, ID) end}])
  end,
  [AddBtn(Btn) || Btn <- Buttons],

  wxStdDialogButtonSizer:realize(BtnSz),
  wxSizer:add(Sz, BtnSz, [{flag, ?wxEXPAND bor ?wxTOP}, {border, 15}]),
  wxDialog:fit(Dlg),
  wxDialog:centre(Dlg),
  Dlg.


%% =====================================================================
%% Internal functions
%% =====================================================================

%% =====================================================================
%% @doc Display a choose directory dialog; gets a directory from the
%% user.
%% @private

dir_dialog(Parent, Args) ->
	Dialog = wxDirDialog:new(Parent, Args),
	case wxDirDialog:showModal(Dialog) of
		?wxID_OK ->
			wxDirDialog:getPath(Dialog);
		?wxID_CANCEL ->
      cancelled
	end.