-module(xhr).

%% ready states
-define(UNSENT, 0).
-define(OPENED, 1).
-define(HEADERS_RECEIVED, 2).
-define(LOADING, 3).
-define(DONE, 4).

-record(xhr, {
		  ready_state = ?UNSENT,
		  status = 0,
		  method = get,
		  url,
		  headers = [],
		  body,
		  http_options = [],
		  options = [{sync, false}, 
					 {stream, self}],
		  request_id
		 }).
-define(is_xhr(X), is_record(X, xhr)).

-export([new/0,
		 open/3,
		 send/1]).


new() ->
	#xhr{}.


open(XHR=#xhr{ready_state=?UNSENT}, Method, URL) 
  when is_list(Method) andalso
	   is_list(URL) ->
	{ok, XHR#xhr{ready_state=?OPENED,
				 method=method_string_to_atom(Method),
				 url=URL}}.


send(XHR=#xhr{ready_state=?OPENED, method=Method, http_options=HTTPOptions, options=Options}) ->
	case http:request(Method, request(XHR), HTTPOptions, Options) of
		{ok, RequestId} ->
			{ok, XHR#xhr{request_id=RequestId}};
		{error, Reason} ->
			{error, Reason}
	end.


request(#xhr{method=M, url=URL, headers=Headers}) 
  when (M == get) orelse
	   (M == options) orelse
	   (M == head) orelse
	   (M == trace) orelse
	   (M == delete) ->
	{URL, Headers};
request(#xhr{method=M, url=URL, headers=Headers, body=Body})
  when (M == put) orelse
	   (M == post) ->
	{URL, Headers, content_type(Headers), Body}.


content_type(Headers) ->
	proplists:get_value(Headers, "content-type", "text/plain").


method_string_to_atom(Method) ->
	method_ustring_to_atom(string:to_upper(Method)).

method_ustring_to_atom("GET") ->
	get;
method_ustring_to_atom("PUT") ->
	put;
method_ustring_to_atom("POST") ->
	post;
method_ustring_to_atom("HEAD") ->
	head;
method_ustring_to_atom("DELETE") ->
	delete;
method_ustring_to_atom("TRACE") ->
	trace;
method_ustring_to_atom("OPTIONS") ->
	options.
