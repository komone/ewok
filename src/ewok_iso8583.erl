
-module(ewok_iso8583).

-behaviour(ewok_codec).
-export([encode/1, decode/1]).

-record(message, {version, class, function, origin}).

encode(_Term) ->
	ok.
	
decode(<<Version:8, Class:8, Function:8, Origin:8, _Primary:32, _Rest/binary>>) ->
	#message{
		version = version(Version),
		class = class(Class),
		function = function(Function),
		origin = origin(Origin)
	}.

version(0) -> 1987;
version(1) -> 1993;
version(2) -> 2003;
version(9) -> private_usage.

class(1) -> authorization;
class(2) -> financial;
class(3) -> file_actions;
class(4) -> reversal;
class(5) -> reconciliation;
class(6) -> administrative;
class(7) -> fee_collection;
class(8) -> network_management;
class(9) -> reserved.

function(0) -> request;
function(1) -> request_response;
function(2) -> advice;
function(3) -> advice_response;
function(4) -> notification;
function(8) -> ack;
function(9) -> nak.

origin(0) -> acquirer;
origin(1) -> acquirer_repeat;
origin(2) -> issuer;
origin(3) -> issuer_repeat;
origin(4) -> other;
origin(5) -> other_repeat.
