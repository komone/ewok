-module(ewok_security).
-include("ewok.hrl").
-include("ewok_system.hrl").

-export([check_access/2]).

%% TODO: make pluggable

check_access(User, #ewok_route{realm = Realm, roles = Roles}) ->
	validate_access(User, Realm, Roles).
	
validate_access(_, _, any) -> 
	ok;
validate_access(undefined, _, _) -> 
	{error, unauthorized};
validate_access(#ewok_user{roles = UserRoles}, Realm, ResourceRoles) ->
	Auth = lists:map(fun (R = {_, _}) -> R; (R) -> {Realm, R} end, ResourceRoles),
	case [X || X <- UserRoles, Y <- Auth, X =:= Y] of
	[] -> 
		{error, unauthorized};
	_ -> 
		ok
	end.
