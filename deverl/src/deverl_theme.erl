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
%% @author Tom Richmond <me@tomrichmond.co.uk>
%% @copyright Tom Richmond 2014
%%
%% @doc Theme parser/importer/transformer. 
%% @end
%% =====================================================================
-module(deverl_theme).

-export([get_theme_names/0,
         parse/2,
         import/1]).

-include_lib("xmerl/include/xmerl.hrl").
-include("deverl.hrl").

%% =====================================================================
%% Client API
%% =====================================================================
%% =====================================================================
%% @doc Returns a list containing the names of all themes in the themes
%% directory without the extension.
get_theme_names() ->
  % parse_theme_names(get_theme_dir()).
  L = filelib:wildcard(filename:join([get_theme_dir(), "*.xml"])),
  lists:map(fun(E)->filename:basename(E, ".xml")end, L).
  
%% =====================================================================
%% @doc Parse the theme file.
%% Uses Notepad++ themes. If the LexerType for language Pref doesn't
%% exist then it uses default WidgetStyles styles.
%% @see extract/2 for return type/format.
parse(Filename, Pref) ->
  Fn = filename:join([filename:dirname(code:which(?MODULE)), "../priv/themes", 
    filename:basename(Filename, ".xml")]) ++ ".xml",
  try xmerl_scan:file(Fn) of
      {error, _} -> error;
      {Root, _Rest} ->
         extract(Root, string:to_lower(Pref), [])
  catch
    exit:_ -> error; % xmerl doesn't fail nicely on invalid XML
    error:_ -> error
  end.
  
%% =====================================================================
%% @doc Entry function for transforming/importing an notepad++ theme.
%% returns ok | {error, string()} 
import(Filename) ->
  try
    transform(Filename)
  catch
    throw:Reason -> {error, Reason}
  end.

%% =====================================================================
%% @doc Get the absolute path to the theme directory
get_theme_dir() ->
  filename:join([filename:dirname(code:which(?MODULE)),"../priv/themes"]).

%% =====================================================================
%% Internal functions
%% ===================================================================== 
%% =====================================================================
%% @doc Parse the XML.
%% returns a list of the form [{widget_styles, [#stye{}]} [, {word_styles, [#style]}]]
extract(R=#xmlElement{name='NotepadPlus'}, Pref, Acc) ->
  extract(R#xmlElement.content, Pref, Acc);
extract([R=#xmlElement{name='LexerStyles'}|T], Pref, Acc0) ->
  Acc1 = extract(R#xmlElement.content, Pref, Acc0),
  extract(T, Pref, Acc1); % continue to the <WidgetStyle> sibling 
extract([R=#xmlElement{name='LexerType'}|T], Pref, Acc) ->
  case extract(R#xmlElement.attributes, Pref, Acc) of
    [] -> extract(T, Pref, Acc); % try the next LexerType
    _ -> [{word_styles, extract(R#xmlElement.content, Pref, Acc)}] % found, grab the attr's
  end;
extract([R=#xmlElement{name='WordsStyle'}|T], Pref, Acc0) ->
  Acc1 = extract(R#xmlElement.attributes, Pref, #style{}),
  extract(T, Pref, [Acc1 | Acc0]);
extract([R=#xmlElement{name='GlobalStyles'}|_], Pref, Acc0) ->
  [{widget_styles, extract(R#xmlElement.content, Pref, [])} | Acc0];
extract([R=#xmlElement{name='WidgetStyle'}|T], Pref, Acc0) ->
  Acc1 = extract(R#xmlElement.attributes, Pref, #style{}),
  extract(T, Pref, [Acc1 | Acc0]);
extract([R=#xmlAttribute{parents=[{WordsStyle,_Pos}|_]}|T], Pref, Acc0) when (WordsStyle == 'WordsStyle') or (WordsStyle == 'WidgetStyle')  -> % built a list of #styles for each style
  Acc1 = case R#xmlAttribute.name of
    styleID -> Acc0#style{id=R#xmlAttribute.value};
    fgColor -> Acc0#style{fg_colour=R#xmlAttribute.value};
    bgColor -> Acc0#style{bg_colour=R#xmlAttribute.value};
    fontName -> Acc0#style{font_name=R#xmlAttribute.value};
    fontStyle -> Acc0#style{font_style=R#xmlAttribute.value};
    fontSize -> Acc0#style{font_size=R#xmlAttribute.value};
    _ -> Acc0
  end,
  extract(T, Pref, Acc1);
extract([#xmlAttribute{value=Pref}|_], Pref, _Acc) -> %% found lang
  [Pref];
extract([#xmlAttribute{}|T], Pref, Acc) ->
  extract(T, Pref, Acc);
extract([_H|T], Pref, Acc) ->
  extract(T, Pref, Acc);
extract([], _Pref, Acc) ->
  Acc.
  

%% Below functoins used for importing notepad++ thenes.
%% =====================================================================
%% @doc Transform a notepad++ theme for erlang support.
%% Currently xmerl will fail on the encoding used in these themes, so remove
%% the encoding attribute in the xml dec.
transform(Filename) ->
  % grab any comments before the root element (want to keep any notices from the authors, and xmerl will trash them)
  Data = case file:read_file(Filename) of
    {error, _} -> throw("Error reading file: " ++ Filename);
    {ok, B}-> B
  end,
    Ack0 = case re:run(Data, "<!--/{0,2}(.*?)/{0,2}-->.*<NotepadPlus>", [dotall,{capture,all_but_first,list}]) of
    {match, D} -> D;
    nomatch -> []
  end,
  % append our messge to the ack
  Ack1 = "<!--" ++ Ack0 ++ "\nImported by Deverl - modified to provide Erlang support.\n\n-->",

  % notepad++ themes usually use windows-1252 encoding (superset of iso-8859-1). 
  % xmerl doesn't support this encoding so rather than manage errors, just remove the xml dec.
  % this also solves the other issue of failure when the xml dec. is not the very first stmt (no whitespace)
  Stripped = re:replace(Data, "(<\\?[^?]+\\?>)", "", [{return,list}]),

  try xmerl_scan:string(Stripped) of
      {error, _} -> error; % XML not valid
      {XML, _} -> 
        case xmerl_xpath:string("//NotepadPlus/LexerStyles/LexerType[@name=\"erlang\"]", XML) of
          [] -> run(XML, Ack1, Filename);
          _ -> export(XML, [], Filename) % already contains Erlang LexerType
        end
  catch
    exit:{fatal, {error_scanning_entity_ref,_,{line,Ln},{col,Col}}} -> 
      throw(lists:flatten(io_lib:format("Invalid entity in XML document ~s, Line: ~p, Col: ~p", [Filename, Ln, Col])));
    exit:_Reason ->
      throw("Error parsing document as valid XML: " ++ Filename);
    error:_ -> error
  end.
  
%% =====================================================================
%% @doc Create the new XML doc.
run(XML, Ack, Filename) ->
  % create the new LexerType element for Erlang
  Node = {'LexerType',
          [{name,"erlang"}, {desc,"Erlang"}, {ext,""}],
          []},
           
  % list of all style names for the Erlang lexer
  % when an element with the same name is not found in the imported theme, and no alternative is provided (2nd tuple element) "DEFAULT" is assumed
  % if there is no "DEFAULT", then this WordStyle (Desc) is just left out
  Descs = ["DEFAULT", "COMMENT", "VARIABLE", "NUMBER", "KEYWORD", "STRING", "OPERATOR", "ATOM", "FUNCTION", "CHARACTER", "MACRO", "RECORD", "SEPARATOR", "NODE NAME", "COMMENT FUNCTION", "COMMENT MODULE", "COMMENT DOC", "COMMENT MACRO", "ATOM QUOTED", "MACRO QUOTED", "RECORD QUOTED", "NODE NAME QUOTED", "BIFS", "MODULES", "MODULE ATTR"],
  
  Zipped = lists:zip(Descs, lists:seq(0, length(Descs) - 1)),  
  % query the XML for a style matching the name in Descs, and modify any attribute values
  Wts = lists:foldl(fun(E, Acc) -> query(E, XML, Acc) end, Node, Zipped),
  
  [WordStyles] = xmerl_xpath:string("//NotepadPlus/LexerStyles", XML),
  #xmlElement{content=Content} = WordStyles,
  NewContent=Content++lists:flatten([Wts]), % append new LexerType node to LexerStyles
  New0=WordStyles#xmlElement{content=NewContent},
  GlobalStyles = xmerl_xpath:string("//NotepadPlus/GlobalStyles", XML),
  New1=XML#xmlElement{content=[New0]++GlobalStyles},
  export(New1, Ack, Filename).
  
%% =====================================================================
%% @doc Add new WordStyle elements for Erlang by querying the original xml theme.
query({{_Name, Rep}, _Id}=T, XML, {LexerType,Attr,WordStys}) ->
  XPATH = "//NotepadPlus/LexerStyles/LexerType/WordsStyle[@name=\"" ++ Rep ++ "\"][1]",
  WsElmt = xmerl_xpath:string(XPATH, XML),
  WsElmtTran = transform(T, XML, WsElmt),
  {LexerType,Attr,[{WsElmtTran}, #xmlText{value="\n\t\t\t"} | WordStys]};
query({Name, Id}=T, XML, {LexerType,Attr,WordStys}=Node) ->
  XPATH = "//NotepadPlus/LexerStyles/LexerType/WordsStyle[@name=\"" ++ Name ++ "\"][1]",
  case xmerl_xpath:string(XPATH, XML) of 
    [] -> query({{Name, "DEFAULT"}, Id}, XML, Node);
    WsElmt ->
      WsElmtTran = transform(T, XML, WsElmt),
      {LexerType,Attr,[{WsElmtTran}, #xmlText{value="\n\t\t\t"} | WordStys]}
  end.
  
%% =====================================================================
%% @doc Modify attribute values. Specifically name and styleID, removing
%% keywordClass. A query is based on the name - so when we get a match
%% it's unlikely that these params will be the same.
transform(_Prms, _XML, []) -> [];
transform(Prms, XML, [#xmlElement{}=Elmt]) ->
  Trans=transform(Prms, XML, Elmt#xmlElement.attributes, []), % update attrs
  [Elmt#xmlElement{attributes=lists:reverse(Trans)}];
transform(_Prms, _XML, Other) ->
  Other.    
  
transform(_Prms, _XML, [], Acc) -> Acc; % 
transform({{Name, Rep}, Id}, XML, [#xmlAttribute{name=name, value=Rep}=Attr | T], Acc) ->
  transform({Name, Id}, XML, T, [Attr#xmlAttribute{value=Name} | Acc]);
transform({Tu1, Id}, XML, [#xmlAttribute{name=styleID}=Attr | T], Acc) ->
  transform({Tu1, Id}, XML, T, [Attr#xmlAttribute{value=Id} | Acc]);
transform(Prms, XML, [#xmlAttribute{name=keywordClass} | T], Acc) ->
  transform(Prms, XML, T, Acc);
transform(Prms, XML, [Attr | T], Acc) ->
  transform(Prms, XML, T, [Attr | Acc]).  

%% =====================================================================
%% @doc Export the theme to the theme folder.
export(XML, Ack0, Filename) ->
  case file:open(filename:join(get_theme_dir(), filename:basename(Filename)),[write]) of
    {error, _Reason} -> throw("Error writing the theme.");
    {ok, IOF} ->
      [XMLDec | Export]=xmerl:export_simple([XML],xmerl_xml), %% export trashes all comments, so they're reinserted after the xml dec.
      Ack1 = XMLDec ++ io_lib:nl() ++ Ack0 ++ io_lib:nl() ++ Export,
      io:format(IOF,"~s~n",[lists:flatten(Ack1)])
  end.