%%
-module(ewok_response).

-include("ewok.hrl").
%-include_lib("kernel/include/file.hrl").

-compile(export_all).

-export([reply/2, continue/1]).

% RIPE FOR REMOVAL after ranges are done.
-export([ok/1]).

-export([write_chunk/2]). %% find where this is used...

%-define(SAVE_FORCE_CLOSE, ewok_request_force_close).
-define(READ_SIZE, 8192).

% maybe move from ewok.hrl to here...
%-record(response, {status, headers=[], content=[], close=false}).

%% Placeholder!! 100-continue
continue(_Request) ->
	ok.

% Plaintext response for now... soon use esp errorpage template?
reply(Request, Response) when is_record(Response, response) ->
	ContentLength = content_length(Response#response.content),
	Headers = 
		case proplists:get_value(content_length, Response#response.headers) of
		undefined -> 
			case Response#response.content of 
			chunked -> 
				[{transfer_encoding, chunked}|Response#response.headers];
			_ -> 
				case Response#response.status =/= 304 of
				true -> [{content_length, ContentLength}|Response#response.headers];
				false -> Response#response.headers
				end
			end;
		Value when is_integer(Value) -> 
			Response#response.headers
		end,
	HttpResponse = get_http_response(Response#response{headers=Headers}),
	Socket = Request:socket(),
	ok = send(Socket, HttpResponse),
	case is_integer(ContentLength) andalso ContentLength > 0 of
	true -> ok = send(Socket, Response#response.content); %% file or iolist
	false -> ok
	end,
	{ok, HttpResponse, ContentLength}.

%% TODO: for deletion
content_length({file, Path}) ->
	filelib:file_size(Path);
content_length(Content) when is_list(Content); is_binary(Content) ->
	iolist_size(Content);
content_length(chunked) ->
	0.

%% 
get_http_response(Response) ->
	Code = ewok_http:status_code(Response#response.status),
	StatusLine = [
		<<"HTTP/1.1 ">>, list_to_binary(integer_to_list(Code)), 
		<<" ">>, ewok_http:status_message(Code), <<"\r\n">>
	],	
    Headers = lists:append(
		[{server, ?SERVER_ID},
		{date, ewok_http:date()}],
		Response#response.headers),
    F = fun ({K, V}, Acc) ->
			[ewok_http:header(K), <<": ">>, make_io(V), <<"\r\n">> | Acc]
		end,
    HeaderLines = lists:foldl(F, [<<"\r\n">>], Headers),
	iolist_to_binary([StatusLine|HeaderLines]).

%%
send(Socket, Bin) when is_binary(Bin) ->
	ok = send_data(Socket, Bin);
send(_, chunked) -> %% placeholder
	ok;
send(Socket, L) when is_list(L) ->
	ok = send_data(Socket, L);
send(Socket, {file, F}) ->
	{ok, Fd} = file:open(F, [raw, binary]),
	ok = send_file(Socket, Fd).

%%
send_file(Socket, Fd) ->
    case file:read(Fd, ?READ_SIZE) of
	{ok, Data} ->
		ok = send_data(Socket, Data),
		send_file(Socket, Fd);
	eof ->
		file:close(Fd),
		ok
    end.
%%
send_data(Socket, Data) ->
    case gen_tcp:send(Socket, Data) of
	ok -> ok;
	_ -> exit(normal) %% leaving a file open?
    end.

%%
write_chunk(Socket, Data) ->
	Length = iolist_size(Data),
	send_data(Socket, io_lib:format("~.16b\r\n", [Length])),
	send_data(Socket, [Data, <<"\r\n">>]).


%% Mochiweb stuff for reference only...

%% @spec ok({value(), iodata()} | {value(), ioheaders(), iodata() | {file, IoDevice}}) ->
%%           response()
%% @doc respond({200, [{"Content-Type", ContentType} | Headers], Body}).
ok({ContentType, Body}) ->
    ok({ContentType, [], Body});
ok({ContentType, Headers, Body}) ->

%% Just to allow compilation for now until range stuff done
    case see_line_below of
%	case Request:get_range() of
	X when X =:= undefined; X =:= fail ->
		% respond({200, Headers, Body});
		error;
	Ranges ->
		{PartList, Size} = range_parts(Body, Ranges),
		case PartList of
		[] -> %% no valid ranges
			Headers1 = [<<"Content-Type">>, list_to_binary(ContentType)|Headers],
			%% could be 416, for now we'll just return 200
			{error, Headers1}; % respond({200, Headers1, Body});
		PartList ->
			{RangeHeaders, RangeBody} =
				parts_to_body(PartList, ContentType, Size),
			HResponse1 = mochiweb_headers:enter_from_list(
				[{"Accept-Ranges", "bytes"} | RangeHeaders], Headers),
			{error, HResponse1, RangeBody} % respond({206, HResponse1, RangeBody})
		end
    end.
	
multipart_body([], _ContentType, Boundary, _Size) ->
    ["--", Boundary, "--\r\n"];
multipart_body([{Start, End, Body} | BodyList], ContentType, Boundary, Size) ->
    ["--", Boundary, "\r\n",
     "Content-Type: ", ContentType, "\r\n",
     "Content-Range: ",
         "bytes ", make_io(Start), "-", make_io(End),
             "/", make_io(Size), "\r\n\r\n",
     Body, "\r\n"
     | multipart_body(BodyList, ContentType, Boundary, Size)].



parts_to_body([{Start, End, Body}], ContentType, Size) ->
    %% return body for a range reponse with a single body
    HeaderList = [{"Content-Type", ContentType},
                  {"Content-Range",
                   ["bytes ",
                    make_io(Start), "-", make_io(End),
                    "/", make_io(Size)]}],
    {HeaderList, Body};
parts_to_body(BodyList, ContentType, Size) when is_list(BodyList) ->
    %% return
    %% header Content-Type: multipart/byteranges; boundary=441934886133bdee4
    %% and multipart body
    Boundary = mochihex:to_hex(crypto:rand_bytes(8)),
    HeaderList = [{"Content-Type",
                   ["multipart/byteranges; ",
                    "boundary=", Boundary]}],
    MultiPartBody = multipart_body(BodyList, ContentType, Boundary, Size),

    {HeaderList, MultiPartBody}.


range_parts({file, IoDevice}, Ranges) ->
    Size = content_length(IoDevice),
    F = fun (Spec, Acc) ->
			case range_skip_length(Spec, Size) of
			invalid_range -> Acc;
			V -> [V | Acc]
			end
        end,
    LocNums = lists:foldr(F, [], Ranges),
    {ok, Data} = file:pread(IoDevice, LocNums),
    Bodies = lists:zipwith(fun ({Skip, Length}, PartialBody) ->
                                   {Skip, Skip + Length - 1, PartialBody}
                           end,
                           LocNums, Data),
    {Bodies, Size};

range_parts(Body0, Ranges) ->
    Body = iolist_to_binary(Body0),
    Size = size(Body),
    F = fun(Spec, Acc) ->
			case range_skip_length(Spec, Size) of
			invalid_range ->
				Acc;
			{Skip, Length} ->
				<<_:Skip/binary, PartialBody:Length/binary, _/binary>> = Body,
				[{Skip, Skip + Length - 1, PartialBody} | Acc]
			end
        end,
    {lists:foldr(F, [], Ranges), Size}.

range_skip_length(Spec, Size) ->
    case Spec of
	{none, R} when R =< Size, R >= 0 ->
		{Size - R, R};
	{none, _OutOfRange} ->
		{0, Size};
	{R, none} when R >= 0, R < Size ->
		{R, Size - R};
	{_OutOfRange, none} ->
		invalid_range;
	{Start, End} when 0 =< Start, Start =< End, End < Size ->
		{Start, End - Start + 1};
	{_OutOfRange, _End} ->
		invalid_range
    end.

%%
make_io(Atom) when is_atom(Atom) -> list_to_binary(atom_to_list(Atom));
make_io(Integer) when is_integer(Integer) -> list_to_binary(integer_to_list(Integer));
make_io(List) when is_list(List) -> list_to_binary(List); 
make_io(Bin) when is_binary(Bin) -> Bin.
