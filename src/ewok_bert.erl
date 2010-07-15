-module(ewok_bert).
-include("ewok.hrl").

-behaviour(ewok_codec).
-export([encode/1, decode/1]).

% @ref: http://bert-rpc.org/

encode(Term) ->
  term_to_binary(encode_term(Term)).

encode_term(Term) ->
  case Term of
    [] -> {bert, nil};
    true -> {bert, true};
    false -> {bert, false};
    Dict when is_record(Term, dict, 8) ->
      {bert, dict, dict:to_list(Dict)};
    List when is_list(Term) ->
      lists:map((fun encode_term/1), List);
    Tuple when is_tuple(Term) ->
      TList = tuple_to_list(Tuple),
      TList2 = lists:map((fun encode_term/1), TList),
      list_to_tuple(TList2);
    _Else -> Term
  end.

decode(Bin) ->
  decode_term(binary_to_term(Bin)).

decode_term(Term) ->
  case Term of
    {bert, nil} -> [];
    {bert, true} -> true;
    {bert, false} -> false;
    {bert, dict, Dict} ->
      dict:from_list(Dict);
    {bert, Other} ->
      {bert, Other};
    List when is_list(Term) ->
      lists:map((fun decode_term/1), List);
    Tuple when is_tuple(Term) ->
      TList = tuple_to_list(Tuple),
      TList2 = lists:map((fun decode_term/1), TList),
      list_to_tuple(TList2);
    _Else -> Term
  end.
