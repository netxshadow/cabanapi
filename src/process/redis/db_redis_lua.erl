-module(db_redis_lua).

-export([load_scripts/0]).
-export([q/2]).


load_scripts()->
    Dir = code:priv_dir(?APP_NAME) ++ "/lua/",
    SettingsList = 
    filelib:fold_files(Dir, "", true, fun(File, Acc)-> 
        Name = list_to_atom(filename:basename(File, ".lua")),
        {ok, LuaCode} = file:read_file(File),  
        Sha1 = list_to_binary(hexstring(crypto:hash(sha, LuaCode))),
        [{Name, {Sha1, LuaCode}} | Acc]
    end, []),
    log:info("[LUA] loaded", []),
    settings:set(SettingsList),
    ok.



% db_redis_lua:q(test, ["0"]).
q(FuncName, Query)->

    log:debug("[REDIS] ~p", [FuncName]),
    {Sha1, LuaCode} = settings:get(FuncName),

    Result = db_redis:q(["EVALSHA", Sha1 | Query]),
    case Result of
        {error, <<"NOSCRIPT", _/binary>>}  ->
            db_redis:q(["EVAL", LuaCode | Query]);
        _  ->
            Result
    end.




%%%%%%%%%%%%%%%% LOCAL




hexstring(<<X:128/big-unsigned-integer>>) ->
    lists:flatten(io_lib:format("~32.16.0b", [X]));
hexstring(<<X:160/big-unsigned-integer>>) ->
    lists:flatten(io_lib:format("~40.16.0b", [X]));
hexstring(<<X:256/big-unsigned-integer>>) ->
    lists:flatten(io_lib:format("~64.16.0b", [X]));
hexstring(<<X:512/big-unsigned-integer>>) ->
    lists:flatten(io_lib:format("~128.16.0b", [X])).
