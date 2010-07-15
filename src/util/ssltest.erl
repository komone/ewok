-module(ssltest).

-export([run/1, run/2]).

-define(HOST, "www.google.com").
-define(PORT, 443).
-define(REQUEST, 
	<<"GET / HTTP/1.1\r\n", 
	"Connection: close\r\n",
	"User-Agent: Mozilla/5.0 (Windows NT 5.1; en-US) Test/1.0.0\r\n",
	"Accept: application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5\r\n",
	"Accept-Language: en-US,en;q=0.8\r\n",
	"Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.3\r\n",
	"\r\n">>).

run(Impl) ->
	run(?HOST, Impl).
run(Host, Impl) when Impl =:= old; Impl =:= new ->
	Opts = [
		binary,
		{active, false}, 
		{verify, 0},
		{depth, 1},
		{keyfile, "./key.pem"},
		{certfile, "./cert.pem"},
		{ssl_imp, Impl}
	], 
	ssl:start(),
	{ok, Socket} = ssl:connect(Host, ?PORT, Opts),
	ok = ssl:send(Socket, ?REQUEST),
	catch ssl:recv(Socket, 0, 10000).
		
