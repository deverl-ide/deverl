-module(lib_widgets).

-include_lib("wx/include/wx.hrl").
-include("ide.hrl").

-export([placeholder/2]).


%% =====================================================================
%% @doc Get a placeholder panel that displays some text within the parent
%% when the parent is otherwise empty.

placeholder(Parent, Str) ->
	%% Mike's Linux needs an additional horizontal sizer (seems like it
	%% ignores {style, ?wxALIGN_CENTRE}).
	Panel = wxPanel:new(Parent),
	wxPanel:setBackgroundColour(Panel, ?PANEL_BG),
	Sz = wxBoxSizer:new(?wxVERTICAL),
	wxPanel:setSizer(Panel, Sz),
	
	HSz = wxBoxSizer:new(?wxHORIZONTAL),
	Text = wxStaticText:new(Panel, ?wxID_ANY, Str, [{style, ?wxALIGN_CENTRE}]),
	wxStaticText:setForegroundColour(Text, ?PANEL_FG),
	wxSizer:addStretchSpacer(HSz),
	wxSizer:add(HSz, Text, [{proportion, 1}, {flag, ?wxEXPAND}]),
	wxSizer:addStretchSpacer(HSz),	
	
	wxSizer:addStretchSpacer(Sz),
	wxSizer:add(Sz, HSz, [{proportion, 1}, {flag, ?wxEXPAND}]),
	wxSizer:addStretchSpacer(Sz),
	Panel.