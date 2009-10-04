%%
%%
-module(ewok_installer).
-vsn({1,0,0}).
-author('steve@simulacity.com').

-include("../include/ewok.hrl").
-include("../include/esp.hrl").
%%
-export([validate/0]).

-behaviour(ewok_web_application).
-export([application_info/0]).

-behaviour(ewok_http_resource).
-export([filter/1, resource_info/0]).
-export(['GET'/2]).

%%
%% Resource Callbacks
%%
application_info() -> [{name, "Ewok Installer"}].
resource_info() -> [{name, "Ewok Installer"}].

%%
validate() ->	
	%false.
	true.

%%
filter(_Request) ->  ok.

%%
'GET'(_Request, _Session) -> 
	get_install_props(),
	not_found.



%%
get_install_props() ->
	Node = node(),
	IsRemote = (Node =/= 'nonode@nohost'),
	UsesDb = ([Node] =:= [N || DbNode = N <- mnesia:system_info(db_nodes), DbNode =:= Node]),
	HasDir = mnesia:system_info(directory),
	UsesDir = mnesia:system_info(use_dir),
	[{node, Node}, {remote, IsRemote}, {db_node, UsesDb}, {db_dir, HasDir}, {db_used, UsesDir}].
