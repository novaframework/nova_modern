%% @private
-module(nova_modern_doc_html).
-export([
         module/1
        ]).

-include("./nova_modern_doc.hrl").

-spec module(#module{}) -> [tuple()].
module(#module{name = Name, description = Description, types = Types, functions = Functions, todo = Todo}) ->
    [{article, [{class, "module"}], [
                                     {h1, [Name]},
                                     layout_module_doc(Description),
                                     layout_todo(Todo),
                                     layout_module_types(Types),
                                     layout_module_functions(Functions)
                                    ]}].

%% @private
layout_module_doc(undefined) ->
    [];
layout_module_doc(#description{brief = Brief, full = Full}) ->
    {section,
     [{class, "description"}],
     case Full of
         _ when is_list(Full) -> [{p, [Full]}];
         _ -> [{p, [Brief]}]
     end
    }.

%% @private
layout_module_types([]) ->
    [];
layout_module_types(Types) ->
    {section, [{id, "types"}, {class, "types details-list"}], [
                                                               {h1, [{class, "section-heading"}], [anchor("#types", "Types")]},
                                                               {'div', [{class, "types-list"}], [
                                                                                                 layout_module_type(Type) || Type <- Types
                                                                                                ]}
                                                              ]}.

%% @private
layout_module_type(#typedecl{label = Label, name = Name, args = Args, description = Description}) ->
    {'div', [{id, Label}, {class, "type-detail"}], [
                                                    {pre, [{code, [{class, "language-erlang"}], [type_spec(Name, Args)]}]},
                                                    layout_description("typespec-doc", Description)
                                                   ]}.

%% @private
layout_module_functions([]) ->
    [];
layout_module_functions(Functions) ->
    {section, [{id, "functions"}, {class, "functions details-list"}], [
                                                                       {h1, [{class, "section-heading"}], [anchor("#functions", "Functions")]},
                                                                       {'div', [{class, "functions-list"}], [
                                                                                                             layout_module_function(Function) || Function <- Functions
                                                                                                            ]}
                                                                      ]}.

%% @private
layout_module_function(#function{label = Label, name = Name, args = Args, returns = Returns, types = Types, description = Description, todo = Todo}) ->
    {'div', [{id, Label}, {class, "detail"}],
     [{'div', [{class, "detail-header"}],
       [{span, [{class, "signature"}], function_name(Name, Args, Returns)},
        {ul, [{class, "type-info"}], function_spec(Args, Types)}
       ]},
      layout_description("docstring", Description),
      layout_todo(Todo)
     ]}.

%% @private
layout_description(_, undefined) ->
    [];
layout_description(Class, Description) ->
    {'div', [{class, Class}], [Description#description.full]}.


layout_todo(undefined) -> [];
layout_todo(Todo) ->
    {'p', [{class, "todo"}], [
                                Todo
                               ]}.

anchor(Href, Title) ->
    {a, [{href, Href}], [Title]}.

type_spec(Name, Args) ->
    Name ++ "/" ++ integer_to_list(length(Args)).

function_name(Name, Args, Type) ->
    case Type of
        undefined ->
            [Name ++ "(" ++ string:join([Arg#argument.name || Arg <- Args], ", ") ++ ")"];
        #type{} ->
            [Name, "("] ++ lists:join(", ", [Arg#argument.name || Arg <- Args]) ++ [") -> "]
    end.

function_spec(Args, Types) ->
    [ {li, [], argument_spec(Arg, Types)} || Arg <- Args ].

argument_spec([], _) -> [];
argument_spec(#argument{name = Arg}, Types) ->
    case lists:keysearch(Arg, 2, Types) of
        false -> [];
        {value, #type{type = Type}} ->
            [Arg, " = ", type_spec(Type)]
    end.


type_spec(#type{name = undefined, type = Type}) ->
    type_spec(Type);
type_spec(#type{name = Name, type = Type}) ->
    {span, [{class, "argument"}], [Name, " :: ", type_spec(Type)]};


type_spec(#type_abstract{module = Module, href = undefined, name = Name}) ->
    case Module of
        undefined -> lists:concat([Name, "()"]);
        _ -> lists:concat([Module, ":", Name, "()"])
    end;
type_spec(#type_abstract{module = Module, href = Href, name = Name}) ->
    ModName =
        case Module of
            undefined -> Name ++ "()";
            _ -> Module ++ ":" ++ Name ++ "()"
        end,
    anchor(Href, ModName);
    %%"<a href=\"" ++ Href ++ "\">" ++ Module ++ ":" ++ Name ++ "()</a>";
type_spec(#type_map{}) -> "map()";
type_spec(#type_tuple{types = Types}) -> lists:join(", ", [ type_spec(X) || X <- Types ]);
type_spec(_) ->
    "_".
