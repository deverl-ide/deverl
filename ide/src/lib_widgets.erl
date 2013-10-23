-module(lib_widgets).

-include_lib("wx/include/wx.hrl").
-include("ide.hrl").

-export([
	placeholder/2,
	placeholder/3,
	colour_shade/2
]).


%% =====================================================================
%% @doc Get a placeholder panel that displays some text within the parent
%% when the parent is otherwise empty.

placeholder(Parent, Str) ->
	placeholder(Parent, Str, []).
	
placeholder(Parent, Str, Options) ->
	%% Linux needs an additional horizontal sizer (seems like it
	%% ignores {style, ?wxALIGN_CENTRE}).
	
	Panel = wxPanel:new(Parent),
	wxPanel:setBackgroundColour(Panel, ?PANEL_BG),
	Sz = wxBoxSizer:new(?wxVERTICAL),
	wxPanel:setSizer(Panel, Sz),
	
	HSz = wxBoxSizer:new(?wxHORIZONTAL),
	case os:type() of
		{linux, _} ->
			wxSizer:addStretchSpacer(HSz);
			_ -> ok
	end,
	Text = wxStaticText:new(Panel, ?wxID_ANY, Str, [{style, ?wxALIGN_CENTRE}]),
	case os:type() of
		{linux, _} ->
			wxSizer:addStretchSpacer(HSz);
			_ -> ok
	end,
	
	Fg = case proplists:get_value(fgColour, Options) of
		undefined -> ?PANEL_FG;
		C -> C
	end,
	wxStaticText:setForegroundColour(Text, Fg),
	wxSizer:add(HSz, Text, [{proportion, 1}, {flag, ?wxEXPAND}]),
	
	wxSizer:addStretchSpacer(Sz),
	wxSizer:add(Sz, HSz, [{proportion, 1}, {flag, ?wxEXPAND}]),
	wxSizer:addStretchSpacer(Sz),
	Panel.
	

%% =====================================================================
%% @doc Get a new shade of Colour.

colour_shade({R,G,B}, Scalar) ->
	{get_shade(R, Scalar), get_shade(G, Scalar), get_shade(B, Scalar)};
colour_shade({R,G,B,A}, Scalar) ->
	{get_shade(R, Scalar), get_shade(G, Scalar), get_shade(B, Scalar), A}.

%% @private
%% @hidden	
get_shade(V, Scalar) ->
	Shade = round(V * Scalar),
	case Shade of
		N when N > 255 ->
			255;
		N when N < 0 ->
			0;
		N -> N
	end.