%% Copyright 2010 Steve Davis <steve@simulacity.com>
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
% 
% http://www.apache.org/licenses/LICENSE-2.0
% 
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.

-module(esp).

-include("esp.hrl").
-include("ewok.hrl").
-include("ewok_system.hrl").

-compile(export_all).
%% API
-export([render/1, render/2, render/4]).
-export([render_page/4, parse_template/1]).
-export([add_dir/1, refresh/0, get_template/1]).
-export([validate/1, validate/2]).

-define(ESP_REGEX, "(<%|%>)").
-define(ESP_DECOMMENT, "<%--.*--%>\n?"). %% NOTE! use dotall option for re:

%% internal use for Cache lookup
-record(template, {path, markup}).

%%
validate(Params) ->
	esp_validator:validate(Params, [not_null]).
%%
validate(Params, Predicates) ->
	esp_validator:validate(Params, Predicates).

%% 
render(Page) ->
	render(ok, Page).
	
render(Status, Page = #page{}) ->
	Spec = esp_html:page(Page#page.title, Page#page.head, Page#page.body),
	try begin
		Elements = render_elements([Spec], []),
		Markup = list_to_binary(Elements),
		Headers = [
			{content_type, ewok_http:mimetype(Page#page.doctype)},
			{content_length, size(Markup)}
		], 
		{Status, Headers, Markup}
	end catch
	_Error:Reason ->
		{internal_server_error, [], Reason}
	end.
%% 
render(Template, Module, Request, Session) when ?is_string(Template) ->
	case get_template(Template) of
	undefined -> 
		{error, no_file};
	{error, Reason} -> 
		{error, Reason};
	_ -> 
		render_page(Template#template.markup, Module, Request, Session)
	end.

%%
render_page(Spec, Module, Request, Session)  ->
	render_page(Spec, Module, Request, Session, true).
%
render_page(Spec, Module, Request, Session, AllowInclude)  ->
%	?TTY({?MODULE, spec, Spec}),
	F = fun (X) ->
		try
			eval(X, Module, Request, Session, AllowInclude)
		catch
		_:_ -> [<<"<tt>&lt;!ESP PARSE ERROR ">>,
			esp_html:text(X), <<" &gt;</tt>">> ]
		end
	end,
	case list_to_binary([F(X) || X <- Spec]) of
	Markup when is_binary(Markup) ->
		{ok, Markup};
	{error, Reason} -> 
		{error, Reason}
	end.

eval(Bin, _, _, _, _) when is_binary(Bin) -> 
	Bin;
eval([H|_] = String, _, _, _, _) when ?is_string(H) ->
	list_to_binary(String);
eval(Records, _, _, _, _) when is_list(Records) ->
	render_elements(Records, []);
eval({esp, 'include', Filename}, _, _, _, AllowInclude) ->
	true = AllowInclude, % intentionally throw parse error
	%have to load the file now...
	?TTY({"ESP include", Filename});
	%render_page(Spec, Module, Request, Session, false);
eval({page, Function, []}, Module, Request, Session, _) -> 
	Result = Module:Function(Request, Session),
	%?TTY("ESP -> ~p ~p~n", [{Module, Function}, Result]),
	render_elements(Result, []);
eval({request, Function, []}, _, Request, _, _) ->
	esp_html:text(Request:Function());
eval({session, ip, []}, _, _, Session, _) ->
	Session:ip();
eval({session, started, []}, _, _, Session, _) ->
		esp_html:text(Session:started());
eval({session, user, []}, _, _, Session, _) ->
	case Session:user() of
	#ewok_user{} = User -> 
		User#ewok_user.name;
	_ -> 
		<<"undefined">>
	end;
eval({session, data, []}, _, _, Session, _) ->
	esp_html:text(Session:data());
eval({M, F, []}, _, _, _, _) ->
	case M:F() of
	Value when is_binary(Value) -> 
		Value;
	Value ->
		esp_html:text(Value)
	end.


% render_elements
render_elements([H|T], Acc) when is_binary(H) ->
	render_elements(T, [H|Acc]);
%% GRRR STRINGS!
render_elements([H|T], Acc) when ?is_string(H) ->
	render_elements(T, [list_to_binary(H)|Acc]);
render_elements([H|T], Acc) when is_list(H) ->
	render_elements(T, [render_elements(H, [])|Acc]);
render_elements([H|T], Acc) when is_tuple(H) ->
	render_elements(T, [render_element(H)|Acc]);
render_elements([], Acc) ->
	lists:reverse(Acc).

% render_element/1
render_element(E) when is_tuple(E), size(E) > 1 ->
%	?TTY(E),
	T = transform_custom_element(E),
	{[Type, Fields], Values} = lists:split(2, tuple_to_list(T)),
	F = fun (X, Y) ->
		Value =
			if 
			X =:= body -> [];
			Y =:= undefined -> [];
			is_atom(Y) -> atom_to_list(Y);
			is_integer(Y) -> integer_to_list(Y);
			is_list(Y) -> Y;
			is_binary(Y) -> binary_to_list(Y); %% this shouldn't happen :)
			true -> []
			end,
		case Value of
		[] -> [];
		_ -> [<<$ >>, atom_to_binary(X, utf8), <<$=,$">>, list_to_binary(Value), <<$">>]
		end
	end,
	Tag = atom_to_binary(Type, utf8), %% is latin1 "safer"?
	Attrs = lists:zipwith(F, Fields, Values),
	Body = 
		%% this case is ENTIRELY to improve markup formatting and should be removed
		%% when html_prettyprint is done. In production mode, markup will be delivered
		%% as a single line, in development we'll prettyprint to make life bearable
		case esp_html:element_type(Type) of
		block ->
			case Fields of 
			[] -> [<<$/, $>, $\n>>];
			_ ->
				case lists:last(Fields) of 
				body -> [<<$>, $\n>>, render_elements(lists:last(Values), []), <<$<, $/>>, Tag, <<$>, $\n>>];
				_ -> [<<$/, $>, $\n>>]
				end
			end;
		inline ->
			case Fields of 
			[] -> [<<$/, $>>>];
			_ ->
				case lists:last(Fields) of 
				body -> [<<$>>>, render_elements(lists:last(Values), []), <<$<, $/>>, Tag, <<$>>>];
				_ -> [<<$/, $>>>]
				end
			end;
		normal ->
			case Fields of 
			[] -> [<<$/, $>, $\n>>];
			_ ->
				case lists:last(Fields) of 
				body -> [<<$>>>, render_elements(lists:last(Values), []), <<$<, $/>>, Tag, <<$>, $\n>>];
				_ -> [<<$/, $>, $\n>>]
				end
			end
		end,
%	?TTY(" ~p~n", [Tag]),
	list_to_binary([<<$<>>, Tag, Attrs, Body]). 

%% first stab at this... 
%% fprof appears to be saying that is_record guards eat up processing, so...
transform_custom_element(E = #inplace{}) ->
	esp_html:inplace(E);
transform_custom_element(E = #css{}) ->
	esp_html:stylesheet(E#css.src, E#css.type, E#css.media);
transform_custom_element(E = #grid{}) ->
	esp_html:grid(E);
transform_custom_element(E) ->
	E.

%%
add_dir(Path) ->
% wrong!% wrong!% wrong!% wrong!
	ewok_cache:add(template, Path).

%%
refresh() ->
	ewok_cache:clear(template).

%%
get_template(Path) ->
	case ewok_cache:lookup(template, Path) of
	#template{} = T -> 
		T;
	undefined ->
		TemplateRoot = ewok_config:get_value({ewok, http, template_root}),
		Markup = load_template(TemplateRoot, Path),
		T = #template{path=Path, markup=Markup},
		case ewok_config:get_value({ewok, runmode}, production) of
		true -> ok = ewok_cache:add(T);
		_ -> ok
		end,
		T
	end.

load_template(undefined, Path) -> 
	{error, {undefined, Path}};
load_template(Dir, Path) ->
	File = ewok_file:path([code:lib_dir(ewok), Dir, Path]),
	case ewok_file:load(File) of
	undefined -> 
		{error, File};
	Bin -> 
		parse_template(Bin)
	end.
	
parse_template(Bin) ->
	Bin2 = re:split(Bin, ?ESP_DECOMMENT, [dotall]),
	parse_template(ewok_text:split(Bin2, ?ESP_REGEX), []).
	
parse_template([<<"<%">>, Expr, <<"%>">>|T], Acc) ->
	parse_template(T, [parse_expr(Expr)|Acc]);
parse_template([H|T], Acc) ->
	parse_template(T, [H|Acc]);
parse_template([], Acc) ->
	lists:reverse(Acc).

parse_expr(Expr) ->
	[Ms, ":", Fs, "(", Args, ")"] = re:split(ewok_text:trim(Expr), "([\:\(\)])", [{return, list}, trim]),
	[Mb, Fb, Argb] = [ewok_text:trim(X) || X <- [Ms, Fs, Args]],
	{list_to_atom(Mb), list_to_atom(Fb), Argb}.
