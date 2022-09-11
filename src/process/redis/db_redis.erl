-module(db_redis).

-export([init/0]).
-export([q/1]).
-export([parse/2]).



q(Query)-> 
    log:debug("[REDIS] ~p", [Query]), 
    run_q(Query).


init() ->

    OPTS = application:get_all_env(erl_template), 

    Args = proplists:get_value( eredis, OPTS ),

    % io:format("~p~n",[Args]),
    % ok = db_redis_lua:load_scripts(),

    Type = proplists:get_value(type, Args),
    Host = proplists:get_value(host, Args),
    Port = proplists:get_value(port, Args),
    Pass = proplists:get_value(password, Args),
    % Size = proplists:get_value(pool_size, Args),
    % ok = settings:set(eredis_type, Type),
    Res = 
    case Type of
        cluster ->
            redis_cluster_sup:start_link(Args);
        single_node ->
            {ok,PID} = eredis:start_link(Host, Port,0,Pass),
            erlang:register(db_redis,PID),
            {ok,PID};
        _ ->
            ignore
    end,
    log:info("Start eredis: ~p ~p", [Type, Res]).



%%%%%%%%%%%%%%%%%%% LOCAL



run_q(Query)->
    run_q(Query, 0).

run_q(_, 15)->
    log:error("[CREDIS] limit_count_resend", []),
    {error, limit_count_resend};

run_q(Query = [_Command | _], Counter)->

    Result = eredis:q(db_redis, Query),

    case Result of

        {error, tcp_closed} ->
            run_q(Query, Counter+1);

        {error, no_connection} ->
            % reload(single_node, 0),
            timer:sleep(100),
            run_q(Query, Counter+1);

        {error, timeout} ->
            timer:sleep(100),
            run_q(Query, Counter+1);

        % Redis explicitly say our slot mapping is incorrect,
        % we need to refresh it
        {error, <<"MOVED ", _/binary>>} ->

            log:error("[CREDIS] MOVED", []),
            % reload(single_node, 0),
            timer:sleep(100),
            run_q(Query, Counter+1);

        {error, <<"CLUSTERDOWN ", _/binary>>} ->

            log:error("[CREDIS] CLUSTERDOWN", []),
            redis_cluster:reload(0),
            timer:sleep(100),
            run_q(Query, Counter+1);

        _ ->
            parse(Query, Result)
    end.




% get_worker(Query)->
%     case settings:get(eredis_type) of
%         cluster ->
%             {ServerPid, Version} = redis_cluster:worker(Query),
%             {cluster, ServerPid, Version};
%         Type ->
%             ServerPid = erlpool:pid(?MODULE),
%             {Type, ServerPid, undefined}
%     end.


% reload(cluster, Version)->
%     redis_cluster:try_reload(Version);
% reload(_, _)->
%     ok.




parse(_, {ok, <<"OK">>})->
    {ok, <<"OK">>};

parse(_, {ok, [undefined]})->
    undefined;
parse(_, {ok, undefined})->
    undefined;

parse([<<"GET">> | _], {ok, Binary}) when is_binary(Binary)->
    {ok, Binary};

parse([<<"GET">> | _], {ok, undefined})->
    undefined;


parse(_Query, Err = {error, <<"NOSCRIPT", _/binary>>})->
    Err;

parse(Query, {error, Message})->
    log:error("[REDIS_ERR_Q] ~p", [Query]),
    log:error("[REDIS_ERR_M] ~p", [Message]),
    {error, Message};

parse(_, Result)->
    Result.



