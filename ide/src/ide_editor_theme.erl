-module(ide_editor_theme).

-export([get_theme_names/0,
				 load_theme/1,
				 hexstr_to_rgb/1]).

-include_lib("xmerl/include/xmerl.hrl").
-include("ide.hrl").


%% =====================================================================
%% @doc Find all themes in directory D.
%% @private

get_themes(D) ->
	filelib:fold_files(D, ".+\.theme$", true, fun(F, L) -> [F|L] end, []).


%% =====================================================================
%% @doc Returns a list containing the names of all themes in the themes
%% directory.

get_theme_names() ->
	parse_theme_names(get_theme_dir()).


%% =====================================================================
%% @doc Construct a list of theme names.
%% @private

parse_theme_names(D) ->
	%% Should really only load these if they are valid
	Fns = get_themes(D),
	lists:map(fun(F)->parse_name(F)end, Fns).


%% =====================================================================
%% @doc Parse a single theme file for its name.
%% @private

parse_name(File) ->
	{ok,Binary} = file:read_file(File),
	{Xml,_} = xmerl_scan:string(binary_to_list(Binary)),
	[#xmlAttribute{value=Name}] = xmerl_xpath:string("/Theme/@name", Xml),
	Name.


%% =====================================================================
%% @doc Read the XML file located at Path into memory.
%% @private

read_xml(Path) ->
	{ok,Binary} = file:read_file(Path),
	{Xml,_} = xmerl_scan:string(binary_to_list(Binary)),
	Xml.


%% =====================================================================
%% @doc Return a theme in its parsed format.

load_theme(Name) ->
	try read_xml(filename:join(get_theme_dir(), Name ++ ".theme")) of
		Result ->
			parse_all(Result)
	catch
		_:_ -> %% ! This needs to be expanded
			{error, load_theme}
	end.


%% =====================================================================
%% @doc Parse the XML theme into the format expected by the theme loader.
%% @private

parse_all(Xml) ->
	Styles = [Attributes || #xmlElement{attributes=Attributes} <-
		xmerl_xpath:string("//Style", Xml)],
	[Default | Lex] = [[ {Name, Value} || #xmlAttribute{name=Name, value=Value} <-
		Style, Value /= [] ] || Style <- Styles],
	[Default, extract_style(fgColour, Lex, true), extract_style(bgColour, Lex, true),
		extract_style(fontStyle, Lex, false), extract_style(fontSize, Lex, false)].


%% =====================================================================
%% @doc Extract individual style types from Data.
%% @private

extract_style(Type, Data, true) ->
	R = [[ {list_to_integer(Id), hexstr_to_rgb(Val)} || {id, Id} <- P, {S,Val} <- P, S =:= Type ] || P <- Data ],
	{Type, lists:flatten(R)};
extract_style(Type, Data, _) ->
	R = [[ {list_to_integer(Id), Val} || {id, Id} <- P, {S,Val} <- P, S =:= Type ] || P <- Data ],
	{Type, lists:flatten(R)}.


%% =====================================================================
%% @doc Convert the hex string from the theme file into {R,G,B}.

hexstr_to_rgb([_|Rgb]) ->
	hexstr_to_rgb(Rgb, []).
hexstr_to_rgb([], Acc) ->
	list_to_tuple(lists:reverse(Acc));
hexstr_to_rgb([A,B | T], Acc) ->
	hexstr_to_rgb(T, [list_to_integer([A,B], 16) | Acc]).


%% =====================================================================
%% @doc Get the absolute path to the theme directory

get_theme_dir() ->
  Dir = filename:dirname(code:which(?MODULE)),
	filename:join([Dir,"../priv","themes"]).
  
  
%% =====================================================================
%% @doc Alternative SAX parser. Marvelous documentation on this.
%% It may well be faster than the current DOM implemenation, however.
%% @private
% sax_print() ->  
%   xmerl_sax_console_parser:file("../priv/themes/Text.theme", 
%   [{event_fun, fun(Event, _Location, _State) ->
%                  io:format("~p~n", [Event])
%                end}]).
% 
% sax_parse() ->
%   xmerl_sax_console_parser:file("../priv/themes/Text.theme", 
%     [{event_fun, fun event/3},
%      {event_state, [{default,[]},{fgColour,[]},{bgColour,[]},{style,[]},{size,[]}] 
%      }]).
% 
% event(E, Loc, State) ->
%   io:format("E: ~p~n", [E]),  
%   io:format("Loc: ~p~n", [Loc]),  
%   io:format("State: ~p~n", [State]),
%   State.