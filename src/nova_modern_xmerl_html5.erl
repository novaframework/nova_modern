%% @private
-module(nova_modern_xmerl_html5).
-export([
	'#xml-inheritance#'/0
]).

-export([
	'#root#'/4,
	'#element#'/5,
	'#text#'/1,
	p/4
]).

-include_lib("xmerl/include/xmerl.hrl").

'#xml-inheritance#'() ->
	[].

'#text#'([{binary, _, Contains}|Tl]) ->
    ["&#60;&#60;", '#text#'(Contains), "&#62;&#62;"|'#text#'(Tl)];
'#text#'([{code, Attrs, Code}|Tl]) ->
    TitleAttr =
        case proplists:get_value(title, Attrs) of
            undefined -> [];
            Value -> [#xmlAttribute{name = title, value = Value}]
        end,
    [closing_element(pre, TitleAttr, closing_element(code, [#xmlAttribute{name = class, value = "language-erlang"}], '#text#'(Code)))|'#text#'(Tl)];
'#text#'([{options, Attrs, Options}|Tl]) ->
    [closing_element(b, [#xmlAttribute{name = class, value="options-title"}], "Options")|'#text#'(Options)];
'#text#'([{option, Attrs, Text}|Tl]) ->
    ForAttr = proplists:get_value(for, Attrs),
    [closing_element(p, [], [closing_element(span, [#xmlAttribute{name = class, value="inline-code"}], ForAttr)|'#text#'(Text)])|'#text#'(Tl)];
'#text#'([{icode, _Attrs, Code}|Tl]) ->
    [closing_element(span, [#xmlAttribute{name = class, value="inline-code"}], '#text#'(Code))|'#text#'(Tl)];
'#text#'([{Type, Attrs, Content}|Tl]) ->
    AttrsNorm = [ #xmlAttribute{name = Name, value = Value} || {Name, Value} <- Attrs ],
    [closing_element(Type, AttrsNorm, '#text#'(Content))|'#text#'(Tl)];
'#text#'([Text|Tl]) when is_list(Text) ->
    [xmerl_lib:export_text(Text)|'#text#'(Tl)];
'#text#'(Text) ->
    xmerl_lib:export_text(Text).

'#root#'(Data, _Attrs, [], _E) ->
	Data.

'#element#'(Tag, Data, Attrs, _Parents, _E) ->
	case is_void_element(Tag) of
		false -> closing_element(Tag, Attrs, Data);
		true -> void_element(Tag, Attrs, Data)
	end.

p(Data, Attrs, _Parents, _E) ->
	case xmerl_lib:is_empty_data(Data) of
		true ->
			%% Paragraph elements should never be completely empty.
			closing_element(p, Attrs, "\s");
		false ->
			closing_element(p, Attrs, Data)
	end.


-spec is_void_element(atom()) -> boolean().
is_void_element(area) -> true;
is_void_element(base) -> true;
is_void_element(br) -> true;
is_void_element(col) -> true;
is_void_element(embed) -> true;
is_void_element(hr) -> true;
is_void_element(img) -> true;
is_void_element(input) -> true;
is_void_element(keygen) -> true;
is_void_element(link) -> true;
is_void_element(meta) -> true;
is_void_element(param) -> true;
is_void_element(source) -> true;
is_void_element(track) -> true;
is_void_element(wbr) -> true;
is_void_element(_) -> false.

closing_element(Tag, Attrs, Data) ->
	[xmerl_lib:start_tag(Tag, Attrs), Data, xmerl_lib:end_tag(Tag)].

void_element(Tag, Attrs, Data) ->
	[xmerl_lib:start_tag(Tag, Attrs), Data].
