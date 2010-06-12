-module(xxtea2).
-export([encrypt/2, decrypt/2, ceiling/1]).
-export([test/0]).
-define(DELTA, 16#9E3779B9).

%% Convert such binary <<1,2,3,4>> to [16#4030201,4]
binary_to_int_list(List, IncludeLength) when is_list(List) ->
    binary_to_int_list(list_to_binary(List), IncludeLength);
binary_to_int_list(Binary, IncludeLength) ->
    BinaryLength = size(Binary),
    BinaryIndex = BinaryLength-1,
    case IncludeLength of
        true ->
            make_int_list(Binary, [0|[BinaryLength]], BinaryIndex, BinaryIndex bsr 2);
        false ->
            make_int_list(Binary, [0], BinaryIndex, BinaryIndex bsr 2)
    end.

%% Binary::binary, IntArray::list, BinaryIndex::int, IntListIndex::int
make_int_list(_, Array, -1, _) ->
    Array;
make_int_list(Binary, IntArray, BIndex, ILIndex) when (BIndex bsr 2) =/= ILIndex ->
    make_int_list(Binary, [0|IntArray], BIndex, BIndex bsr 2);
make_int_list(Binary, [ArrayHead|ArrayTail], BIndex, ILIndex) ->
    NewAddend = convert_byte_to_int(Binary, BIndex),
    CurrIntValue = int32(ArrayHead bor NewAddend),
    make_int_list(Binary, [CurrIntValue|ArrayTail], BIndex-1, ILIndex).
    
convert_byte_to_int(Binary, Index) ->
    <<_:Index/binary, ByteValue, _/binary>> = Binary,
    ByteValue bsl ((Index band 3) bsl 3).


%% Convert such list [16#4030201,4] to binary <<1,2,3,4>>
int_list_to_binary(IntArray, false) when is_list(IntArray) ->
    int_list_to_binary(IntArray, length(IntArray) bsl 2, [], 0, 0);
int_list_to_binary(IntArray, true) when is_list(IntArray) ->
    DataLength = lists:last(IntArray),
    case DataLength > (length(IntArray) bsl 2) of
        true ->
            bad_data;
        false ->
            int_list_to_binary(IntArray, DataLength, [], 0, 0)
    end.

int_list_to_binary(_, DataLength, BinaryList, DataLength, _) ->
    list_to_binary(lists:reverse(BinaryList));
int_list_to_binary([_|IntTail], DataLength, BinaryList, Index, IntArrayIndex) when (Index bsr 2) =/= IntArrayIndex ->
    int_list_to_binary(IntTail, DataLength, BinaryList, Index, Index bsr 2);
int_list_to_binary([IntHead|_]=IntArray, DataLength, BinaryList, Index, IntArrayIndex) ->
    int_list_to_binary(IntArray, DataLength, [get_byte_from_int(IntHead, Index)|BinaryList], Index+1, IntArrayIndex).

get_byte_from_int(Number, Index) ->
    (Number bsr ((Index band 3) bsl 3)) band 16#FF.


encrypt(Data, Key) when is_binary(Data), is_binary(Key) ->
	encrypt(binary_to_list(Data), binary_to_list(Key));
	
encrypt(Data, Key) when is_list(Data) and is_list(Key) ->
	io:format("key: ~p\n", [binary_to_int_list(Key, false)]),
	io:format("data: ~p\n", [binary_to_int_list(Data, true)]),
    C = encrypt_int_list(binary_to_int_list(Data, true), binary_to_int_list(Key, false)),
	io:format("enc: ~p\n", [C]),
	int_list_to_binary(C, false).
	
encrypt_int_list(Value, _Key) when is_list(Value) and length(Value) == 0 ->
    Value;
encrypt_int_list(Value, Key) when is_list(Key) and (length(Key) < 4) ->
    encrypt_int_list(Value, formal_key(Key));
encrypt_int_list(Value, Key) when is_list(Value) and is_list(Key) ->
    N = length(Value) - 1,
    Z = lists:last(Value),
    Sum = 0,
    Q = floor(6 + 52 div (N + 1)),
    encrypt_loop1(Value, Key, Z, Sum, Q).

encrypt_loop1(Value, Key, Z, Sum, Q) when Q>0 ->
    Sum2 = int32(Sum + ?DELTA),
    E = (Sum2 bsr 2) band 3,
    encrypt_loop2(Value, Key, [], Z, Sum2, E, 0, Q-1);
encrypt_loop1(Value, _Key, _Z, _Sum, _Q) ->
    Value.

%% List: NewI1 NewI2 NewI3 I4 -> NewI1 NewI2 NewI3 NewI4
%%       Y             Z   X   
encrypt_loop2([X], Key, EncryptList, Z, Sum, E, P, Q) ->
    Y = lists:last(EncryptList),
    Z2 = int32(X + calc_bit_operation_value(Y, Z, Key, P, E, Sum)),
    encrypt_loop1(lists:reverse([Z2|EncryptList]), Key, Z2, Sum, Q);
%% List: I1 I2 I3 I4  -> NewI1 I2 I3 I4  -> NewI1 NewI2 I3 I4 -> NewI1 NewI2 NewI3 I4
%%       X  Y     Z   -> Z     X  Y      ->         Z   X  Y  -> Y             Z   X
encrypt_loop2([X,Y|_]=Value, Key, EncryptList, Z, Sum, E, P, Q) ->
    Z2 = int32(X + calc_bit_operation_value(Y, Z, Key, P, E, Sum)),
    encrypt_loop2(tl(Value), Key, [Z2|EncryptList], Z2, Sum, E, P+1, Q).

calc_bit_operation_value(Y, Z, Key, P, E, Sum) ->
    Temp = lists:nth((P band 3 bxor E)+1, Key),
    Temp2 = (Z bsr 5) band 16#07FFFFFF,
    Temp3 = Y bsl 2,
    Temp4 = Temp2 bxor Temp3,
    Temp5 = (Y bsr 3) band 16#1FFFFFFF,
    Temp6 = Z bsl 4,
    Temp7 = Temp5 bxor Temp6,
    Temp8 = int32(Temp4 + Temp7),
    Temp9 = Sum bxor Y,
    Temp10 = Temp bxor Z,
    Temp11 = int32(Temp9 + Temp10),
    Temp12 = Temp8 bxor Temp11,
    Temp12.
    
    
decrypt(Data, _Key) when is_binary(Data) and size(Data) == 0 ->
    "";
decrypt(Data, Key) when is_binary(Data) ->
	io:format("key: ~p\n", [binary_to_int_list(Key, false)]),
	io:format("data: ~p\n", [binary_to_int_list(Data, false)]),
    P = decrypt_int_list(binary_to_int_list(Data, false), binary_to_int_list(Key, false)),
	io:format("dec: ~p\n", [P]),	
    int_list_to_binary(P, true).

decrypt_int_list(Data, Key) when is_list(Key) and (length(Key) < 4) ->
    decrypt_int_list(Data, formal_key(Key));
decrypt_int_list(Data, Key) when is_list(Data) and is_list(Key) ->
    N = length(Data) - 1,
    Y = hd(Data),
    Q = floor(6 + 52 div (N + 1)),
    Sum = int32(Q * ?DELTA),
    decrypt_loop1(Data, Key, Sum, Y, N).
    
decrypt_loop1(Data, Key, Sum, Y, N) when Sum =/= 0 ->
    E = (Sum bsr 2) band 3,
    decrypt_loop2(lists:reverse(Data), Key, [], Y, Sum, E, N, N);
decrypt_loop1(Data, _Key, _Sum, _Y, _N) ->
    Data.

%% Data: NewI4 NewI3 NewI2 I1 -> NewI4 NewI3 NewI2 NewI1
%%       Z            Y    X                       Y
decrypt_loop2([X], Key, DecryptList, Y, Sum, E, P, N) ->
    Z = lists:last(DecryptList),
    Y2 = int32(X - calc_bit_operation_value(Y, Z, Key, P, E, Sum)),
    decrypt_loop1([Y2|DecryptList], Key, int32(Sum-?DELTA), Y2, N);
%% Data: I4 I3 I2 I1  -> NewI4 I3 I2 I1  -> NewI4 NewI3 I2 I1 -> NewI4 NewI3 NewI2 I1
%%       X  Z     Y   -> Y     X  Z      ->       Y     X  Z     Z            Y    X
decrypt_loop2([X,Z|_]=ReverseData, Key, DecryptList, Y, Sum, E, P, N) ->
    Y2 = int32(X - calc_bit_operation_value(Y, Z, Key, P, E, Sum)),
    decrypt_loop2(tl(ReverseData), Key, [Y2|DecryptList], Y2, Sum, E, P-1, N).

%% Utils functions
formal_key(Key) ->
    formal_key2(lists:reverse(Key)).
formal_key2(ReverseKeyList) when length(ReverseKeyList) == 4 ->
    lists:reverse(ReverseKeyList);
formal_key2(ReverseKeyList) ->
    formal_key([0|ReverseKeyList]).

int32(Num) -> 
    N1 = Num band 16#FFFFFFFF, 
    case N1 =< 16#7FFFFFFF of
        true -> N1; 
        false -> N1 - 16#FFFFFFFF - 1 
    end.
    
floor(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T - 1;
        Pos when Pos > 0 -> T;
        _ -> T
    end.

ceiling(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.

%% Testing entrance
test() ->
    decrypt(encrypt("Arbow", "Xlands"), "Xlands").

