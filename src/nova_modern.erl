-module(nova_modern).
-export([
	run/2
]).

-include_lib("edoc/include/edoc_doclet.hrl").
-include("./nova_modern_doc.hrl").


%% @doc Main doclet entry point.
-spec run(#doclet_gen{} | #doclet_toc{}, #context{}) -> ok.
run(#doclet_gen{sources = Sources, app = App}, #context{dir = OutputDir, env = Env, opts = Options}) ->
	Docs = [#module{name = index} | [nova_modern_doc:from_source(filename:join(SourceDir, SourceFile), Env, Options) ||
		{_Module, SourceFile, SourceDir} <- Sources]],
    gen(OutputDir, App, [Doc || Doc <- Docs, not Doc#module.private, not Doc#module.hidden]);
run(#doclet_toc{paths = Paths}, Context) ->
	io:format(user, "TOC~npaths=~p~ncontext=~p~n", [Paths, Context]).
	%toc(Paths, Context).

%% @private
gen(OutputDir, App, Docs) ->
	lists:foreach(fun (#module{name = ModuleName} = Modulnova) ->
	    case ModuleName of
			index ->
				Navigation = navigation(ModuleName, Docs),
				write_html(OutputDir, "index.html", layout("", Navigation, ""));
			_ ->      
				Title = title(App, ModuleName),
				Navigation = navigation(ModuleName, Docs),
				Content = content(Modulnova),
				OutputName = ModuleName ++ ".html",
				write_html(OutputDir, OutputName, layout(Title, Navigation, Content))
		end
	end, Docs),
	copy_assets(filename:join(OutputDir, "assets")).

%% @private
title(?NO_APP, Module) ->
	io_lib:fwrite("~s", [Module]);
title(App, Module) ->
	io_lib:fwrite("~s – ~s", [Module, App]).

%% @private
navigation(_ModuleName, ModuleDocs) ->
	[
         {input, [{type, "input"}, {placeholder, "Search Nova docs"}, {class, "searchbox"}], []},
         {ul, lists:map(fun (Doc) ->
		Href = Doc#module.name ++ ".html",
		{li, [
			{a, [{href, Href}], [Doc#module.name]},
			{ul, lists:map (fun (Function) ->
				{li, [
					{a, [{href, Href ++ "#" ++ Function#function.label}], [Function#function.name, "/", Function#function.arity]}
				]}
			end, Doc#module.functions)}
		]}
	end, ModuleDocs)}].

%% @private
content(ModuleDocs) ->
	nova_modern_doc:to_html(ModuleDocs).

%% @private
layout(Title, Navigation, Content) ->
	[{html, [
		{head, [
			meta([{charset, "utf-8"}]),
			meta([{'http-equiv', "x-ua-compatible"}, {content, "ie=edge"}]),
			meta([{name, "viewport"}, {content, "width=device-width, initial-scale=1.0"}]),
			meta([{name, "generator"}, {content, "EDoc"}]),
			{title, [Title]},
			{link, [{rel, "stylesheet"}, {href, "assets/app.css"}], []},
			{link, [{rel, "stylesheet"}, {href, "assets/github.css"}], []},
                        {script, [{src, "assets/highlight.min.js"}], []},
                        {script, [{src, "assets/jquery-3.5.0.min.js"}], []},
                        {script, [{src, "assets/nova_modern.js"}], []},
                        {script, [], ["hljs.initHighlightingOnLoad();"]}
		]},
                 {body, [
                         {'div', [{id, app}], [
                                               {'div', [{id, 'app-navigation'}], Navigation},
                                               {main, [{id, 'app-content'}], Content}
                                              ]}
                        ]}
                ]}].

%% @private
meta(Attributes) ->
	{meta, Attributes, []}.

%% @private
write_html(OutputDir, OutputName, Xml) ->
    Html = xmerl:export_simple(Xml, nova_modern_xmerl_html5, []),
    edoc_lib:write_file(Html, OutputDir, OutputName, [{encoding, utf8}]).

%% @private
copy_assets(ToDir) ->
    FromDir = code:priv_dir("nova_modern"),
    ok = copy_asset(FromDir, ToDir, "app.css"),
    ok = copy_asset(FromDir, ToDir, "github.css"),
    ok = copy_asset(FromDir, ToDir, "highlight.min.js"),
    ok = copy_asset(FromDir, ToDir, "jquery-3.5.0.min.js"),
    ok = copy_asset(FromDir, ToDir, "nova_modern.js"),
    ok = copy_asset(FromDir, ToDir, "todo-icon.png").

%% @private
copy_asset(FromDir, ToDir, Name) ->
	From = filename:join(FromDir, Name),
	To = filename:join(ToDir, Name),
	ok = filelib:ensure_dir(To),
	edoc_lib:copy_file(From, To).
