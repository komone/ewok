%% dns.hrl


%% http://tools.ietf.org/html/rfc1035

-define(DNS_PORT, 53).

%-record(dns_message, {header, question, answer, authority, additional}).

-record(dns_query, {id = 0, questions = [], type=standard, recurse=true, authenticate=false}).

-record(dns_response, {id, type, code, questions = [], answers = [], authority = [], additional = []}).

-record(dns_rr, {class, type, name, ttl, data}).

