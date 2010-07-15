-module(ewok_pop3).
-include("ewok.hrl").
-include("email.hrl").

-behaviour(ewok_codec).
-export([encode/1, decode/1]).

% @ref http://tools.ietf.org/html/rfc1939
-compile(export_all).

test() ->
	?TTY(decode(<<"USER steved\r\n">>)),
	?TTY(decode(<<"STAT\r\n">>)),
	?TTY(decode(<<"LIST 1\r\n">>)),
	?TTY(decode(<<"RETR 1\r\n">>)),
	?TTY(decode(<<"QUIT\r\n">>)),
	
	?TTY(encode({ok, <<"Message">>})),
	?TTY(encode({ok, [<<"Message">>, <<"0">>]})),
	?TTY(encode({error, <<"Message">>})),
	?TTY(encode({error, [<<"Message">>]})).
	
encode({Status, Message}) when is_binary(Message) ->
	{ok, list_to_binary([status_code(Status), ?SP, Message, ?CRLF])};
encode({Status, Messages}) ->
	{ok, list_to_binary([status_code(Status), ?SP, [[X, ?CRLF] || X <- Messages]])}.

decode(Bin) ->
	[Command|Args] = ewok_text:split(Bin, <<"[ \r\n]+">>),
	{ok, {command(Command), Args}}.


status_code(ok)    -> <<"+OK">>;
status_code(error) -> <<"-ERR">>.

% Minimal
command(<<"USER">>) -> 'USER'; % name
command(<<"PASS">>) -> 'PASS'; % string
command(<<"STAT">>) -> 'STAT';
command(<<"LIST">>) -> 'LIST'; % [msg]
command(<<"RETR">>) -> 'RETR'; % msg
command(<<"DELE">>) -> 'DELE'; % msg
command(<<"NOOP">>) -> 'NOOP';
command(<<"RSET">>) -> 'RSET';
command(<<"QUIT">>) -> 'QUIT';
% Optional
command(<<"APOP">>) -> 'APOP'; % name, digest
command(<<"TOP">>)  -> 'TOP';  % msg n
command(<<"UIDL">>) -> 'UIDL'; % [msg]
% @ref http://tools.ietf.org/html/rfc1734
command(<<"AUTH">>) -> 'AUTH'; % name
command(<<"CAPA">>) -> 'CAPA'; % name
command(Bin) -> Bin. %% temporary
