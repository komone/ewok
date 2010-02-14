%% Copyright 2009 Steve Davis <steve@simulacity.com>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%% http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(ewok_identity_srv).
-name("Ewok Identity Service").

-include("ewok.hrl").
-include("ewok_system.hrl").

%% TODO: Thoroughly review the RFC and this implementation.
%% RFC-4122 <http://www.ietf.org/rfc/rfc4122.txt>

-behaviour(ewok_service).
-export([start_link/1, stop/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-compile(export_all).

%% TODO: Replace this with an id that's dynamically generated at build time?
-define(IVEC, <<213,53,164,93,158,212,70,56,134,80,224,220,249,214,82,76>>).

-record(state, {node, clock_seq, secret, keystore}).

%% TODO: These are all the same!?!
-define(UUID_DNS_NAMESPACE, <<107,167,184,16,157,173,17,209,128,180,0,192,79,212,48,200>>).
-define(UUID_URL_NAMESPACE, <<107,167,184,17,157,173,17,209,128,180,0,192,79,212,48,200>>).
-define(UUID_OID_NAMESPACE, <<107,167,184,18,157,173,17,209,128,180,0,192,79,212,48,200>>).
-define(UUID_X500_NAMESPACE, <<107,167,184,20,157,173,17,209,128,180,0,192,79,212,48,200>>).

%%
start_link(Args) ->
	crypto:start(),
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).
	
stop() ->
    gen_server:cast(?SERVER, stop).

%%
%% uuid gen_server for generating timestamps with saved state
%%
init(Options) ->
	Secret = crypto:md5(proplists:get_value(password, Options, <<>>)),
    {A1,A2,A3} = proplists:get_value(seed, Options, seed()),
    random:seed(A1, A2, A3),
    State = #state{
        node = proplists:get_value(node, Options, <<0:48>>),
        clock_seq = random:uniform(65536),
		secret = Secret,
		keystore = load_keystore(ewok, Secret)
    },
	?TTY({keystore, State#state.keystore}),
    {ok, State}.

handle_call({new, default}, _From, State) ->
	ID = random(),
	{reply, ID, State};
handle_call({new, timestamp}, _From, State) ->
    Reply = timestamp(State#state.node, State#state.clock_seq),
    {reply, Reply, State};
handle_call({new, sha}, _From, State) ->
    Reply = sha(dns, <<"ewok">>),
    {reply, Reply, State};
handle_call({new, md5}, _From, State) ->
    Reply = md5(dns, <<"ewok">>),
    {reply, Reply, State};
handle_call({password, Password}, _From, State) ->
	Secret = crypto:md5(Password),
	save_keystore(ewok, Secret, State#state.keystore),
    {reply, ok, State#state{secret = Secret}};
handle_call({keystore}, _From, State) ->
	Keys = proplists:get_keys(State#state.keystore),
	{reply, Keys, State};
handle_call({keystore, Key}, _From, State) when is_atom(Key) ->
	Value = proplists:get_value(Key, State#state.keystore),
    {reply, Value, State};
handle_call({keystore, Key, Value}, _From, State) when is_atom(Key) ->
	Keystore = lists:keystore(Key, 1, State#state.keystore, {Key, Value}),
	save_keystore(ewok, State#state.secret, Keystore),
    {reply, ok, State#state{keystore = Keystore}};
handle_call(_, _From, State) ->
	{reply, undefined, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ewok_log:info("uuid server stopped"),
    ok.

%% FROM YAWS
%% pretty good seed, but non portable
seed() ->
	try begin
		URandom = os:cmd("dd if=/dev/urandom ibs=12 count=1 2>/dev/null"),
		<<X:32, Y:32, Z:32>> = list_to_binary(URandom),
		{X, Y, Z}
	end catch 
		_:_ -> now()
	end.
	
%% Generates a random UUID 
random() ->
    U = <<
        (random:uniform(4294967296) - 1):32,
        (random:uniform(4294967296) - 1):32,
        (random:uniform(4294967296) - 1):32,
        (random:uniform(4294967296) - 1):32
    >>,
    format_uuid(U, 4).
	
%% Generates a UUID based on a crypto:sha() hash
sha(Namespace, Name) ->
    Context = crypto:sha_update(crypto:sha_update(crypto:sha_init(), namespace(Namespace)), Name),
    U = crypto:sha_final(Context),
    format_uuid(U, 5).

%% Generates a UUID based on a crypto:md5() hash
md5(Namespace, Name) ->
    Context = crypto:md5_update(crypto:md5_update(crypto:md5_init(), namespace(Namespace)), Name),
    U = crypto:md5_final(Context),
    format_uuid(U, 3).

%% @spec timestamp(Node, CS) -> uuid()
%% where
%%      Node = binary()
%%      CS = int()
%% @doc
%% Generates a UUID based on timestamp
%%
timestamp(Node, CS) ->
    {MegaSecs, Secs, MicroSecs} = erlang:now(),
    T = (((((MegaSecs * 1000000) + Secs) * 1000000) + MicroSecs) * 10) + 16#01b21dd213814000,
    format_uuid(T band 16#ffffffff, (T bsr 32) band 16#ffff, (T bsr 48) band 16#ffff, (CS bsr 8) band 16#ff, CS band 16#ff, Node, 1).

%%
%% Internal API
%%
namespace(dns) -> ?UUID_DNS_NAMESPACE;
namespace(url) -> ?UUID_URL_NAMESPACE;
namespace(oid) -> ?UUID_OID_NAMESPACE;
namespace(x500) -> ?UUID_X500_NAMESPACE;
namespace(UUID) when is_binary(UUID) -> UUID;
namespace(_) -> error.

format_uuid(TL, TM, THV, CSR, CSL, <<N:48>>, V) ->
    format_uuid(<<TL:32, TM:16, THV:16, CSR:8, CSL:8, N:48>>, V);

format_uuid(TL, TM, THV, CSR, CSL, N, V) ->
    format_uuid(<<TL:32, TM:16, THV:16, CSR:8, CSL:8, N:48>>, V).

format_uuid(<<TL:32, TM:16, THV:16, CSR:8, CSL:8, N:48, _Rest/binary>>, V) ->
    <<TL:32, TM:16, ((THV band 16#0fff) bor (V bsl 12)):16, ((CSR band 16#3f) bor 16#80):8, CSL:8, N:48>>.

%% TODO: Add encryption - make aes work for Erlang Binary Term Format
keystore_path(App) ->
	ewok_file:path([ewok_util:appdir(App), ewok_util:get_env(data_dir, ?DATA_DIR), ?KEYSTORE_FILE]).
%%
save_keystore(App, SecretKey, Keystore) ->
	Path = keystore_path(App),
	Bin = base64:encode(term_to_binary(Keystore)),
	Cipher = ewok_crypto:encrypt(aes, SecretKey, Bin),
	ok = ewok_file:save(Path, Cipher).
%%
load_keystore(App, SecretKey) ->
	Path = keystore_path(App),
	case ewok_file:load(Path) of
	Bin when is_binary(Bin) ->
		Base64 = ewok_crypto:decrypt(aes, SecretKey, Bin),
		binary_to_term(base64:decode(Base64));
	undefined ->
		Keystore = [],
		Bin = base64:encode(term_to_binary(Keystore)),
		Cipher = ewok_crypto:encrypt(aes, SecretKey, Bin),
		ewok_file:save(Path, Cipher),
		Keystore
	end.
