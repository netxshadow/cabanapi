-module(db_redis_util).


-export([encode/1]).
-export([encode_result/2]).
-export([decode/1]).
-export([hfoldl/3]).


encode(Map)when is_map(Map)->
    Options = [{spec, old}, {map_format, jsx}],
    Unpacked = msgpack:pack(filter_empty(Map), Options),
    encode_result(Unpacked, Map).



encode_result({error, _} = Err, Map) ->
    log:error("msgpack error ~p ~p", [Err, Map]),
    erlang:throw(msgpack_error);
encode_result(Binary, _) ->
    Binary.




decode(Binary)->
    {ok, Map} = msgpack:unpack(Binary, [{spec, new}, {unpack_str, as_binary}]),
    Map.







hfoldl(_Fun, Data, [])->
    Data;
hfoldl(Fun, Data, [Field, Binary | Rest])->
    hfoldl(Fun, Fun(Field, Binary, Data), Rest).




%%%%%%%%%%%%%%%% LOCAL





filter_empty(Map) when is_map(Map)->
    filter_empty(maps:to_list(Map));
filter_empty(List) when is_list(List)->
    lists:flatmap(fun(Val)-> 
        case Val of
            undefined -> [];
            {_, undefined} -> [];
            {_, null} -> [];
            {_, <<>>} -> [];
            null      -> [];
            <<>>      -> [];
            []        -> [];
            _         ->
                [filter_empty(Val)]

        end
    end, List);
filter_empty({Key, Val})->
    {Key, filter_empty(Val)};
filter_empty(Val)->
    Val.
