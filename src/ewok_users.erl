%%
-module(ewok_users).
-vsn("1.0").
-include("ewok.hrl").

-compile(export_all).

%%
init_db() ->
	mnesia:start(),
	ewok_db:create_tables([
		{role, record_info(fields, role)},
		{user, record_info(fields, user)},
		{auth, record_info(fields, auth)}
	]),
    ok = mnesia:wait_for_tables([user, role, auth], 10000),
	
	%% TODO: Later convert these to use ewok identity server
	ewok_identity:seed(),
	AdminRole = {ewok, admin},
	UserID = ewok_identity:id(),
	Activation = ewok_identity:key(),
	%%
	ok = ewok_db:create(#role{id=AdminRole}),
	ok = ewok_db:create(#user{id=UserID, name={ewok, "ewok"}, roles=[AdminRole]}), 
	ok = ewok_db:create(#auth{id=UserID, activation=Activation}),
	{admin, {activation, Activation}}.

%%
login(Domain, Username, Password) ->
	try begin
		Domain1 = list_to_existing_atom(Domain),
		case ewok_db:select(user, {name, {Domain1, Username}}) of
		{ok, [User]} when is_record(User, user) ->
			Auth = ewok_db:read(auth, User#user.id),
			%?TTY("GOT USER ~p~n", [{User, Auth}]),
			case Auth#auth.password of
			Digest when is_binary(Digest) ->
				case crypto:sha(Password) =:= Digest of
				true -> 
					{ok, User};
				false -> 
					{error, invalid_password}
				end;
			undefined ->
				{error, not_activated}
			end;
		{ok, []} ->
			{error, no_user}
		end
	end catch
		error:badarg -> 
			{error, no_domain}
	end.

%%
exists(Realm, Username) ->
	try begin
		Realm1 = list_to_existing_atom(Realm),
		case ewok_db:select(user, {name, {Realm1, Username}}) of
		{ok, [User]} when is_record(User, user) ->
			true;
		_ -> false
		end
	end catch
		error:badarg -> 
			{error, no_domain}
	end.

%%
activate(Realm, Username, Activation, Password) when is_list(Realm) ->
	activate(list_to_existing_atom(Realm), Username, Activation, Password);
activate(Realm, Username, Activation, Password) ->
	try begin
		case ewok_db:select(user, {name, {Realm, Username}}) of
		{ok, [User]} when is_record(User, user) ->
			case ewok_db:read(auth, User#user.id) of
			Auth when is_record(Auth, auth) ->
				case Auth#auth.activation of
				Activation ->
					Digest = crypto:sha(Password),
					ewok_db:update(Auth#auth{password=Digest}),
					{ok, User};
				_ -> 
					{error, invalid_activation}
				end;
			_ ->
				{error, no_user}
			end;
		_ ->
			{error, no_auth}
		end
	end catch
		error:badarg -> 
			{error, no_domain}
	end.

%%
create_user(Realm, Username, Password) when is_list(Realm) ->
	create_user(list_to_existing_atom(Realm), Username, Password);
create_user(Realm, Username, Password) ->
	try begin
		case ewok_db:select(user, {name, {Realm, Username}}) of
		{ok, []} ->
			ID = ewok_identity:id(),
			ewok_db:create(#user{id=ID, name={Realm, Username}}),
			Digest = crypto:sha(Password),
			Activation = ewok_identity:key(),
			ewok_db:create(#auth{id=ID, password=Digest, activation=Activation}),
			{ok, Activation};
		{ok, _} ->
			{error, exists}
		end
	end catch
		error:badarg -> 
			{error, no_domain}
	end.	
%%
create_user(Domain, Username) when is_list(Domain) ->
	create_user(list_to_existing_atom(Domain), Username);
create_user(Domain, Username) ->
	try begin
		case ewok_db:select(user, {Domain, Username}) of
		{ok, []} ->
			ID = ewok_identity:id(),
			ewok_db:create(#user{id=ID, name={Domain, Username}}),
			Activation = ewok_identity:key(),
			ewok_db:create(#auth{id=ID, activation=Activation}),
			{ok, Activation};
		{ok, _} ->
			{error, exists}
		end
	end catch
		error:badarg -> 
			{error, no_domain}
	end.
%%
get_user(ID) ->
	{ok, User} = ewok_db:read(user, ID),
	User.
%%
get_activation(ID) ->
	{ok, Auth} = ewok_db:read(auth, ID),
	Auth#auth.activation.

