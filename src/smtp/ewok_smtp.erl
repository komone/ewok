%% Copyright 2009-2010 Steve Davis <steve@simulacity.com>
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
-module(ewok_smtp).

-include("ewok.hrl").
-include("ewok_system.hrl").
-include("email.hrl").

-behaviour(ewok_codec).
-export([encode/1, decode/1]).

%% @ref RFC5321

%% ewok_codec callbacks
%%
encode(Term) ->
	Code = code(Term),
	{ok, list_to_binary([ewok_text:encode(Code), ?SP, status_message(Code), ?CRLF])}.
%%	
decode(Bin) ->
	[Verb|Args] = ewok_text:split(Bin, <<"[ \r\n]+">>, 2),
	case command(ewok_text:to_upper(Verb)) of
	undefined -> 
		case ewok_text:split(Bin, <<"\r\n\r\n">>) of
		[Body] ->
			{ok, {data, [], decode_body([Body])}};
		[Headers|Body] ->
			{ok, {data, decode_headers(Headers), decode_body(Body)}}
		end;
	Command ->
		{ok, {Command, ewok_text:split(Args, <<"\r\n">>)}}
	end.

%%
decode_headers(Headers) ->
	decode_headers(ewok_text:split(Headers, <<"\r\n">>), []).
decode_headers([H|T], Acc) ->
	[Name, Value] = ewok_text:split(H, <<":">>, 2),
	decode_headers(T, [{ewok_text:trim(Name), ewok_text:trim(Value)} | Acc]);
decode_headers([], Acc) ->
	lists:reverse(Acc).
	
%%
decode_body(Lines) ->
	decode_body(Lines, []).
decode_body([H|T], Acc) ->
	Lines = ewok_text:split(H, <<"\r\n">>),
	decode_body(T, lists:append(Acc, Lines));
decode_body([], Acc) ->
	Acc.

command(<<"DATA">>) -> 'DATA';
command(<<"EHLO">>) -> 'EHLO';
command(<<"EXPN">>) -> 'EXPN';
command(<<"HELO">>) -> 'HELO';
command(<<"HELP">>) -> 'HELP';
command(<<"MAIL">>) -> 'MAIL';
command(<<"NOOP">>) -> 'NOOP';
command(<<"QUIT">>) -> 'QUIT';
command(<<"RCPT">>) -> 'RCPT';
command(<<"RSET">>) -> 'RSET';
command(<<"VRFY">>) -> 'VRFY';
command(_) -> undefined.

code(system_status) ->   211;
code(help_message) ->    214;
code(service_ready) ->   220;
code(closing_channel) -> 221;

code(ok_completed) ->    250;
code(ok_forwarding) ->   251;
code(ok_unverified) ->   252;
code(ok_started) ->      253;

code(start_mail_input) ->   354;
code(transaction_offset) -> 355;

code(service_not_available) -> 421;
code(mailbox_full) ->          422;
code(password_required) ->     432; %% could be MS Exchange error

code(mailbox_unavailable) ->   450;
code(action_aborted) ->        451;
code(insufficient_storage) ->  452;
code(no_mail) ->               453;
code(alt_encryption_required) ->   454; %% incorrect?

%458	Unable to queue messages for node node.
%459	Node node not allowed: reason.

code(command_not_recognized) ->    500;
code(syntax_error) ->              501;
code(command_not_implemented) ->   502;
code(bad_command_sequence) ->      503;
code(parameter_not_implemented) -> 504;

%521	Machine does not accept mail.
%530	Must issue a STARTTLS command first. Encryption required for requested authentication mechanism.
%534	Authentication mechanism is too weak.
%538	Encryption required for requested authentication mechanism.
%550	Requested action not taken: mailbox unavailable.
%551	User not local; please try forwardpath.
%552	Requested mail action aborted: exceeded storage allocation.
%553	Requested action not taken: mailbox name not allowed.
%554	Transaction failed.
code(mail_not_accepted) -> 521;
code(alt2_encryption_required) -> 530;
code(authentication_too_weak) -> 534;
code(encryption_required) -> 538;

code(mailbox_unavailable2) -> 550;
code(user_unknown_try_forwardpath) -> 551;
code(mailbox_exceeded_storage_allocation) -> 552;
code(mailbox_name_not_allowed) -> 553;
code(transaction_failed) -> 554.

status_message(211) -> <<"System Status">>; % Need more info
status_message(214) -> <<"http://www.ietf.org/rfc/rfc5321.txt">>;
status_message(220) -> <<"OK">>;
status_message(221) -> <<"SMTP server closing transmission channel">>;
status_message(250) -> <<"Requested mail action okay, completed">>;
status_message(251) -> <<"User not local">>;
status_message(252) -> <<"Cannot VRFY user, but will accept message and attempt deliver">>;
status_message(354) -> <<"Mail input accepted">>;
status_message(421) -> <<"Service not avaiable, closing transmission channel">>;
status_message(450) -> <<"Requestion action not taken: mailbox unavailable">>;
status_message(451) -> <<"Requestion action aborted: local error in processing">>;
status_message(452) -> <<"Requestion action not taken: insufficient system storage">>;
status_message(500) -> <<"Syntax error, command unrecognized">>;
status_message(501) -> <<"Syntax error in parameters or arguments">>;
status_message(502) -> <<"Command not implemented">>;
status_message(503) -> <<"Bad sequence of commands">>;
status_message(504) -> <<"Command parameter not implemented">>;
status_message(511) -> <<"No mailbox here by that name">>;
status_message(550) -> <<"Reqested action not taken: mailbox unavailable">>;
status_message(551) -> <<"User not local">>;
status_message(552) -> <<"Requestion action not taken: insufficient system storage">>;
status_message(553) -> <<"Requestion action not taken: mailbox name not allowed">>;
status_message(554) -> <<"Transaction Failed">>.
