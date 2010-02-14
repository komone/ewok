%%
-module(exercise6).
-vsn("1.0").

-include_lib("ewok/include/ewok.hrl").
-include_lib("ewok/include/esp.hrl").

-behavior(ewok_http_resource).
-export([filter/1, resource_info/0]).
-export(['GET'/2]).
%% Resource Callbacks
%
resource_info() -> [
	{name, "Exercise 6"}
].

%%
filter(_Request) -> ok.

%%
'GET'(Request, _Session) ->
	Name = 
		case Request:parameter(<<"module">>) of
		undefined -> <<"ewok">>;
		Value -> Value
		end,
	Page = #page{
		title=[list_to_binary([Name, <<" API">>])],
		head=[#css{src="/tutorial.css"}],
		body=[
			#h1{body=[<<"Module: ">>, Name]},
			get_api(Name)
		]
	},
	esp:render(Page).
%%
get_api(ModuleName) ->
	Module = erlang:binary_to_atom(ModuleName, utf8),
	case code:ensure_loaded(Module) of
	{'module', Module} ->
		F = fun(Name, Arity) -> [list_to_binary([esp_html:text(Name), <<"/">>, esp_html:text(Arity)])] end,
		Body = [F(X, Y) || {X, Y} <- lists:sort(Module:module_info(exports)), X =/= module_info],
		esp_html:table([ModuleName, <<" API">>], [<<"Function">>], Body);
	_ ->
		#p{body=[<<"Can't find module: ">>, ModuleName]}
	end.
