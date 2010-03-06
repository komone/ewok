%% Copyright 2010 Steve Davis <steve@simulacity.com>
% 
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
% 
% http://www.apache.org/licenses/LICENSE-2.0
% 
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.

-module(ewok_geoip).

-include("ewok.hrl").
-include("ewok_system.hrl").
-include("ewok_geoip.hrl").

-behaviour(ewok_service).
-export([start_link/1, stop/0]).

%% API
-export([lookup/1, update/0, info/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2,
	handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
	type = ?GEOIP_COUNTRY_EDITION,
	updated,
	file,
	record_length = ?STANDARD_RECORD_LENGTH,
	segments = 0,
	data
}).

%%
start_link([]) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
%%
stop() ->
    gen_server:cast(?MODULE, stop).

%%
lookup(Address) when is_binary(Address) ->
    gen_server:call(?MODULE, {lookup, Address});
lookup(Address) when is_list(Address) ->
	lookup(list_to_binary(Address)).
%%	
update() ->
	gen_server:call(?SERVER, update).

%%
info() ->
    gen_server:call(?SERVER, info).

%% gen_server callbacks

%%
init(_Opts) ->
	load_data().
%%
handle_call({lookup, Address}, _From, State) ->
    Result = lookup(Address, State),
	{reply, Result, State};
%%
handle_call(update, _From, State) ->
	Result =
		case load_data() of
		{ok, NewState} ->
			ok;
		{error, _} ->
			NewState = State
		end,
    {reply, Result, NewState};
%%
handle_call(info, _From, State) ->
	Result = [
		{type, State#state.type},
		{file, State#state.file},
		{updated, ewok_util:timestamp(State#state.updated)},
		{segments, State#state.segments}
	],
		
    {reply, Result, State}. 
%%
handle_cast(stop, State) ->
    {stop, normal, State}.
%%
handle_info(Info, State) ->
    ewok_log:info({?MODULE, Info}),
    {noreply, State}.
%%
terminate(_Reason, _State) ->
    ok.
%%
code_change(_OldVsn, State, _Extra) ->
    State.

%% Internal functions

%%
load_data() ->
	File = ewok_config:get_value({ewok, geoip, data_file}, ?GEOIP_DATA_FILE),
	Path = ewok_file:path([ewok_util:appdir(), File]),
	case ewok_file:is_regular(Path) of
	true ->
		Bin = ewok_file:load(Path),
	    Data = zlib:gunzip(Bin),
		{ok, State} = parse_data(size(Data) - ?STANDARD_RECORD_LENGTH, ?STRUCTURE_INFO_MAX_SIZE, Data),
		ok = validate(State),
		{ok, State#state{updated = ewok_file:modified(Path), file = Path}};
	false ->
		ewok_log:error({db_not_found, Path}),
		{error, db_not_found}
	end.

%%
parse_data(_, 0, _Data) ->
    {error, invalid_data};
parse_data(Cursor, N, Data) ->
	case Data of
	<<_:Cursor/binary, 255, 255, 255, Tail/binary>> ->
		Edition = 
			case Tail of
			<<X, _/binary>> when X >= 106 ->
				X - 105;
			<<X, _/binary>> ->
				X
			end,
		{Start, Length} = edition_offsets(Edition),
%		?TTY({edition, Edition, Start, Length}),
		Segments = 
			case Start of 
			calculated -> 
				parse_segments(Edition, Cursor + 4, Data);
			_ ->
				Start
			end,
	    State = #state{
				type = Edition,
				segments = Segments,
				record_length = Length,
				data = Data
			},
%		?TTY({edition, State#state.type, segments, State#state.segments}),
	    {ok, State};
	_ ->
	    parse_data(Cursor - 1, N - 1, Data)
    end.

%%
edition_offsets(?GEOIP_REGION_EDITION_REV0) ->
	{?GEOIP_STATE_BEGIN_REV0, ?STANDARD_RECORD_LENGTH};
edition_offsets(?GEOIP_REGION_EDITION_REV1) ->
	{?GEOIP_STATE_BEGIN_REV1, ?STANDARD_RECORD_LENGTH};
edition_offsets(?GEOIP_COUNTRY_EDITION) ->
	{?GEOIP_COUNTRY_BEGIN, ?STANDARD_RECORD_LENGTH};
edition_offsets(?GEOIP_PROXY_EDITION) ->
	{?GEOIP_COUNTRY_BEGIN, ?STANDARD_RECORD_LENGTH};
edition_offsets(?GEOIP_NETSPEED_EDITION) ->
	{?GEOIP_COUNTRY_BEGIN, ?STANDARD_RECORD_LENGTH};	
edition_offsets(?GEOIP_ORG_EDITION) ->
	{calculated, ?ORG_RECORD_LENGTH};	
edition_offsets(?GEOIP_ISP_EDITION) ->
	{calculated, ?ORG_RECORD_LENGTH};
edition_offsets(?GEOIP_ASNUM_EDITION) ->
	{calculated, ?STANDARD_RECORD_LENGTH};
edition_offsets(?GEOIP_CITY_EDITION_REV0) ->
	{calculated, ?STANDARD_RECORD_LENGTH};
edition_offsets(?GEOIP_CITY_EDITION_REV1) ->
	{calculated, ?STANDARD_RECORD_LENGTH}.

parse_segments(Edition, Cursor, Data) when 
		Edition == ?GEOIP_CITY_EDITION_REV0;
		Edition == ?GEOIP_CITY_EDITION_REV1;
		Edition == ?GEOIP_ORG_EDITION;
		Edition == ?GEOIP_ISP_EDITION;
		Edition == ?GEOIP_ASNUM_EDITION ->
    <<_:Cursor/binary, Segments:?SEGMENT_RECORD_LENGTH/little-unit:8, _/binary>> = Data,
    Segments.

%%
% IP as integer
lookup(IP, Data) when is_integer(IP) ->
    case find_country(IP, 0, 31, Data) of
	{ok, Cursor} ->
	    parse_record(Cursor, Data);
	Error ->
	    Error
    end;
% IPv4 tuple
lookup({X3, X2, X1, X0}, Data) ->
	<<X:32>> = <<X3, X2, X1, X0>>,
	lookup(X, Data);
% IPv6 tuple
lookup({X7, X6, X5, X4, X3, X2, X1, X0}, Data) ->
	<<X:64>> = <<X7, X6, X5, X4, X3, X2, X1, X0>>,
	lookup(X, Data);
% Dot-delimited text format
lookup(Text, Data) ->
	Strings = ewok_text:split(Text, <<"\\.">>, [{return, list}]),
	IntList = [list_to_integer(X) || X <- Strings],
	lookup(list_to_tuple(IntList), Data).

%%
find_country(_Ip, _Offset, -1, _Data) ->
    {error, depth_exceeded};
find_country(Ip, Cursor, Depth, Data) ->
    RecordLength = Data#state.record_length,
	Offset = 2 * RecordLength * Cursor,
    <<_:Offset/binary, X0:RecordLength/little-unit:8, X1:RecordLength/little-unit:8, _/binary>> = Data#state.data,
    X = case (Ip band (1 bsl Depth)) of
		0 -> 
			X0;
		_ -> 
			X1
		end,
    case (X >= Data#state.segments) of
	true ->
	    {ok, X};
	false ->
	    find_country(Ip, X, Depth - 1, Data)
    end.
	
%%
parse_record(Cursor, Db) ->
    Data = Db#state.data,
	Cursor1 = Cursor + (2 * Db#state.record_length - 1) * Db#state.segments,
    <<_:Cursor1/binary, CountryNum, _/binary>> = Data,
	{Country, Country3, CountryName} = country_data(CountryNum),
    {Region, Cursor2} = get_string(Cursor1 + 1, 0, Data),
    {City, Cursor3} = get_string(Cursor2, 0, Data),
    {Postal, Cursor4} = get_string(Cursor3, 0, Data),
    <<_:Cursor4/binary, RawLat:24/little, RawLon:24/little, _/binary>> = Data,
    Lat = (RawLat / 10000) - 180,
    Lon = (RawLon / 10000) - 180,
    {DmaCode, AreaCode} = get_record_extra(Db#state.type, Country, Cursor4 + 6, Data),
	#geoip{
		country_code = Country,
		country_code3 = Country3,
		country_name = CountryName,
		region = Region,
		city = City,
		postal_code = Postal,
		latitude = Lat,
		longitude = Lon,
		dma_code = DmaCode,
		area_code = AreaCode
	}.

%%
country_data(CountryNum) ->
	Digraph = element(CountryNum, ?GEOIP_COUNTRY_DIGRAPH),
    Trigraph = element(CountryNum, ?GEOIP_COUNTRY_TRIGRAPH),
    Name = element(CountryNum, ?GEOIP_COUNTRY_NAMES),
	{Digraph, Trigraph, Name}.

%%
get_record_extra(?GEOIP_CITY_EDITION_REV1, <<"US">>, Cursor, Data) ->
    <<_:Cursor/binary, Extra:24/little, _/binary>> = Data,
    {Extra div 1000, Extra rem 1000};
get_record_extra(_, _, _, _) ->
    {0, 0}.
%%
get_string(Cursor, Offset, Data) ->
	case Data of
	<<_:Cursor/binary, String:Offset/binary, 0, _/binary>> ->
		{String, Cursor + Offset + 1};
	_ ->
		get_string(Cursor, Offset + 1, Data)
	end.

%%
validate(Database) ->
	%% Extends the MaxMind Unit Test
	case lookup(<<"24.24.24.24">>, Database) of 
	#geoip{
		country_code = <<"US">>,
		country_code3 = <<"USA">>,
		country_name = <<"United States">>,
		region = <<"NY">>,
		city = <<"Jamaica">>,
		postal_code = <<"11434">>,
		area_code = 718
	} ->
		ok;
	_ ->
		{error, validation}
	end.
