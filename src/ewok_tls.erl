-module(ewok_tls).
-compile(export_all).

-define(TLS_VERSION, {3, 3}).

% ContentType
-define(CHANGE_CIPHER_SPEC, 20).
-define(ALERT, 21).
-define(HANDSHAKE, 22).
-define(APPLICATION_DATA, 23).

% AlertLevel
-define(WARNING, 1).
-define(FATAL, 2).

%% AlertDescription
-define(CLOSE_NOTIFY, 0).
-define(UNEXPECTED_MESSAGE, 10).
-define(BAD_RECORD_MAC, 20).
-define(DECRYPTION_FAILED_RESERVED, 21).
-define(RECORD_OVERFLOW, 22).
-define(DECOMPRESSION_FAILURE, 30).
-define(HANDSHAKE_FAILURE, 40).
-define(NO_CERTIFICATE_RESERVED, 41).
-define(BAD_CERTIFICATE, 42).
-define(UNSUPPORTED_CERTIFICATE, 43).
-define(CERTIFICATE_REVOKED, 44).
-define(CERTIFICATE_EXPIRED, 45).
-define(CERTIFICATE_UNKNOWN, 46).
-define(ILLEGAL_PARAMETER, 47).
-define(UNKNOWN_CA, 48).
-define(ACCESS_DENIED, 49).
-define(DECODE_ERROR, 50).
-define(DECRYPT_ERROR, 51).
-define(EXPORT_RESTRICTION_RESERVED, 60).
-define(PROTOCOL_VERSION, 70).
-define(INSUFFICIENT_SECURITY, 71).
-define(INTERNAL_ERROR, 80).
-define(USER_CANCELED, 90).
-define(NO_RENEGOTIATION, 100).
-define(UNSUPPORTED_EXTENSION, 110).

-record(security_parameters, {
	entity, % server, client
	prf_algorithm = tls_prf_sha256,
	bulk_cipher_algorithm = null, % null, rc4, 3des, aes
	cypher_type, % stream, block, aead
	enc_key_length, 
	block_length,
	fixed_iv_length,
	record_iv_length,
	mac_algorithm = null, % null, hmac_md5, hmac_sha1, hmac_sha256, hmac_sha384, hmac_sha512
	mac_length,
	mac_key_length,
	compression_algorithm = 0, % in other rfc
	master_secret, % 48 bytes
	client_random, % 32 bytes
	server_random % 32 bytes
}).

-record(protocol_version, {major, minor}).

-record(tls_plaintext,  {content_type, protocol_version, length, fragment}).
-record(tls_compressed, {content_type, protocol_version, length, fragment}).
-record(tls_ciphertext, {content_type, protocol_version, length, fragment}).


tls_plaintext(Fragment) ->
	tls_plaintext(Fragment, ?TLS_VERSION).
	
tls_plaintext(Fragment, {Major, Minor}) ->
	Length = size(Fragment),
	<<?APPLICATION_DATA, Major, Minor, Length:16, Fragment/binary>>. 

