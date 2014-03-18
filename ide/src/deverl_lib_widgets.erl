%% =====================================================================
%% @author
%% @copyright
%% @title
%% @version
%% @doc This module groups together common functions for wx.
%% @end
%% =====================================================================

-module(deverl_lib_widgets).

-include_lib("wx/include/wx.hrl").
-include("deverl.hrl").

-export([
  rc_dir/1,
	placeholder/2,
	placeholder/3,
	colour_shade/2,
  datetime_to_string/1,
  set_list_item_background/2
]).

-define(PANEL_FG, {160,160,160}).
-define(PANEL_BG, {252,252,252}).

%% =====================================================================
%% Client API
%% =====================================================================

%% =====================================================================
%% @doc Get the absolute path to the rsc (resource) directory.

rc_dir(File) ->
  Dir = filename:dirname(code:which(?MODULE)),
  filename:join([Dir,"../rc",File]).
  

%% =====================================================================
%% @doc Get a placeholder panel that displays some text within the parent
%% when the parent is otherwise empty.

-spec placeholder(wxWindow:wxWindow(), string()) -> wxPanel:wxPanel().

placeholder(Parent, Str) ->
	placeholder(Parent, Str, []).

-spec placeholder(wxWindow:wxWindow(), string(), list()) -> wxPanel:wxPanel().

placeholder(Parent, Str, Options) ->
	%% Linux (Mint, at least!) needs an additional horizontal sizer (seems like it
	%% ignores {style, ?wxALIGN_CENTRE}).

	Panel = wxPanel:new(Parent),
	wxPanel:setBackgroundColour(Panel, ?PANEL_BG),
	Sz = wxBoxSizer:new(?wxVERTICAL),
	wxPanel:setSizer(Panel, Sz),

	HSz = wxBoxSizer:new(?wxHORIZONTAL),
	case os:type() of
		{_, linux} ->
			wxSizer:addStretchSpacer(HSz);
			_ -> ok
	end,

	Text = wxStaticText:new(Panel, ?wxID_ANY, Str, [{style, ?wxALIGN_CENTRE}]),
  wxSizer:add(HSz, Text, [{proportion, 1}, {flag, ?wxEXPAND}]),
	case os:type() of
		{_, linux} ->
			wxSizer:addStretchSpacer(HSz);
			_ -> ok
	end,

	Fg = case proplists:get_value(fgColour, Options) of
		undefined -> ?PANEL_FG;
		C -> C
	end,
	wxStaticText:setForegroundColour(Text, Fg),
	%wxSizer:add(HSz, Text, [{proportion, 1}, {flag, ?wxEXPAND}]),

	wxSizer:addStretchSpacer(Sz),
	wxSizer:add(Sz, HSz, [{proportion, 1}, {flag, ?wxEXPAND}]),
	wxSizer:addStretchSpacer(Sz),
	Panel.


%% =====================================================================
%% @doc Get a new shade of Colour.

-spec colour_shade(RGB | RGBA, float()) -> RGB | RGBA when
  RGB  :: {integer(), integer(), integer()},
  RGBA :: {integer(), integer(), integer(), integer()}.


colour_shade({R,G,B}, Scalar) ->
	{get_shade(R, Scalar), get_shade(G, Scalar), get_shade(B, Scalar)};
colour_shade({R,G,B,A}, Scalar) ->
	{get_shade(R, Scalar), get_shade(G, Scalar), get_shade(B, Scalar), A}.



%% =====================================================================
%% @doc Convert calender:datetime() to a string of the form
%% YYYY-MM-DD HH:MM:SS

-spec datetime_to_string(calender:datetime()) -> string().

datetime_to_string({{Y, M, D}, {H, Mi, S}}) ->
  Args = [Y, M, D, H, Mi, S],
  Str = io_lib:format("~B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B", Args),
  lists:flatten(Str).


%% =====================================================================
%% @doc Set the background colour for a single list item.

-spec set_list_item_background(wxListCtrl:wxListCtrl(), integer()) -> ok.

set_list_item_background(ListCtrl, Item) ->
	case Item rem 2 of
	  0 ->
			wxListCtrl:setItemBackgroundColour(ListCtrl, Item, ?ROW_BG_EVEN);
	  _ ->
	 		wxListCtrl:setItemBackgroundColour(ListCtrl, Item, ?ROW_BG_ODD)
	end.


%% =====================================================================
%% Internal functions
%% =====================================================================

%% =====================================================================
%% @doc
%% @private

-spec get_shade(integer(), float()) -> integer().

get_shade(V, Scalar) ->
	Shade = round(V * Scalar),
	case Shade of
		N when N > 255 ->
			255;
		N when N < 0 ->
			0;
		N -> N
	end.