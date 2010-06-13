%% 
-module(ewok_erlang).

%% Base formula API for Erlang analysis
-export([erlangs/2, erlang_b/3, extended_erlang_b/4, erlang_c/3, engset/4]).

%% Useful derivative data
-export([wait_time/3, service_level/4]).

%% References:
%% http://en.wikipedia.org/wiki/Erlang_(unit)
%% http://www.mitan.co.uk/erlang/elgcmath.htm

%% A measure of intensity
erlangs(Rate, Duration) ->
	Rate * Duration.

%% A measure of the probability of blocking
erlang_b(Rate, Duration, Agents) ->
	E = erlangs(Rate, Duration),
	erlang_b_calc(E, Agents).

%% A measure of the probability of blocking when a percentage of users retry
extended_erlang_b(Rate, Duration, Agents, RecallFactor) ->
	E = erlangs(Rate, Duration),
	extended_erlang_b_calc(E, Agents, RecallFactor, E).

%% A measure of the probability of having to wait
erlang_c(Rate, Duration, Agents) ->
	E = erlangs(Rate, Duration),
	erlang_c_calc(E, Agents).

%% TODO: Implement this sometime
engset(_Rate, _Duration, _Agents, _Sources) ->
	not_implemented.

%%
wait_time(Rate, Duration, Agents) ->
	E = erlangs(Rate, Duration),
	EC = erlang_c_calc(E, Agents),
	EC * Duration / (Agents * (1 - E / Agents)).

%%
service_level(Rate, Duration, Agents, Target) ->
	E = erlangs(Rate, Duration),
	W = -(Agents - E) * Target / Duration,
	1 - erlang_c_calc(E, Agents) * math:exp(W).

%% private
%	
erlang_b_calc(E, Agents) ->
	1.0 / erlang_b_calc(1, Agents, E, 1.0).
%
erlang_b_calc(Count, Agents, E, Acc) when Count =< Agents ->
	Acc0 = 1.0 + Count / E * Acc,
	erlang_b_calc(Count + 1, Agents, E, Acc0);
erlang_b_calc(_, _, _, Acc) ->
	Acc.
	
%
extended_erlang_b_calc(E, Agents, RecallFactor, Acc) ->
	P = erlang_b_calc(Acc, Agents),
	R = E * P * RecallFactor,
	case E + R of
	Acc ->
		P;
	E0 ->
		extended_erlang_b_calc(E, Agents, RecallFactor, E0)
	end.

%
erlang_c_calc(E, Agents) ->
	X = math:pow(E, Agents) / factorial(Agents),
	Y = Agents / (Agents - E),
	F = X * Y,
	Sum = erlang_c_calc(E, Agents - 1, 0),
	F / (Sum + F).
%	
erlang_c_calc(E, A, Acc) when A >= 0 ->
	X = math:pow(E, A) / factorial(A),
	erlang_c_calc(E, A - 1, X + Acc);
erlang_c_calc(_, _, Acc) ->
	Acc.

%
factorial(0) ->
	1;
factorial(N) when N > 0 ->
	factorial(N, 1).
%
factorial(N, Acc) when N > 0 ->
	factorial(N - 1, N * Acc);
factorial(_, Acc) ->
	Acc.
