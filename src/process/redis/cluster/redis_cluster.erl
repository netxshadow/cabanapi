-module(redis_cluster).

-behaviour(gen_server).

%% API.
-export([worker/1]).
-export([pid/1]).
-export([reload/1]).
-export([try_reload/1]).
-export([get_all_pools/0]).

%% gen_server.
-export([start_link/1]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).


-define(LOG_INFO(LogMsg, LogArgs), log:info(LogMsg, LogArgs)).
-define(LOG_ERR(LogMsg, LogArgs),  log:error(LogMsg, LogArgs)).
-record(state, {
    init_nodes = [],
    pool_size = 1,
    nodes = [],
    version = 0
}).
-record(pools, {
    pools = [],
    version = 0,
    updated = 0
}).
-record(pool, {
    name,
    s_start,
    s_end
}).





worker(Command)->
    PoolKey = get_key_from_command(Command),
    Slot = get_key_slot(PoolKey),
    {PoolName, Version} = get_pool_by_slot(Slot),
    {erlpool:pid(PoolName), Version}.


pid(PoolName)->
    erlpool:pid(PoolName).


reload(Version) ->
    gen_server:call(?MODULE,{reload, Version}).







try_reload(Version) ->
    Pools = get_pools(),
    UnixTime = Pools#pools.updated,
    case (erlang:system_time(seconds) > (UnixTime + 1)) of
        true  -> reload(Version);
        false -> ignore
    end.






get_all_pools() ->
    Pools = get_pools(),
    lists:usort([Pool#pool.name || Pool <- Pools#pools.pools]).







%%%%%%%%%%%%%%%%%%% gen_server.





start_link(Args) ->
    gen_server:start_link({local,?MODULE}, ?MODULE, [Args], []).


init([Args]) ->
    Host = proplists:get_value(host, Args),
    Port = proplists:get_value(port, Args),
    Size = proplists:get_value(pool_size, Args),
    persistent_term:put({?MODULE, pools}, #pools{}),
    State = reload_slots(#state{
        pool_size = Size,
        init_nodes = [{Host, Port}],
        nodes = [{Host, Port}]
    }),
    {ok, State}.

handle_call({reload, Version}, _From, #state{version=Version} = State) ->    
    ?LOG_INFO("[CREDIS] RELOAD  ~p", [Version]),
    NewState =
    try
        reload_slots(State)
    catch T:R:S->
        ?LOG_ERR("[CREDIS] err ~p ~p ~p", [T, R, S]),
        State
    end,
    {reply, ok, NewState};
handle_call({reload, _}, _From, State) ->
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.






%%%%%%%%%%%%%%%%% INTERNAL





    

get_pools() ->
    Pools = persistent_term:get({?MODULE, pools}, []),
    Pools.






reload_slots(State) ->
    Slots = get_slots(State#state.nodes),

    {Pools, Nodes} = lists:foldl(fun([StartSlot, EndSlot | [[Host0, Port0 | _] | _]], {PoolsAcc, NodesAcc})->

        Host = binary_to_list(Host0),
        Port = binary_to_integer(Port0),
        % start pool
        PoolName = get_name(Host, Port),
        PoolRes =
        erlang:apply(erlpool,start_pool,[PoolName, [
            {size, State#state.pool_size},
            {group, redis_cluster_pool},
            {start_mfa, {eredis, start_link, [Host, Port]}}
        ]]),
        case PoolRes of
            ok -> ok;
            {error,{already_started,_}} -> ok;
            PoolErr ->
                exit(PoolErr)
        end,

        Pool = #pool{
            s_start = binary_to_integer(StartSlot),
            s_end = binary_to_integer(EndSlot),
            name = PoolName
        },
        {[Pool | PoolsAcc], [{Host, Port} | NodesAcc]}

    end, {[], []}, Slots),

    NewVersion = State#state.version + 1,
    PrevPools  = get_pools(),
    NewPools   = #pools{
        pools   = Pools,
        version = NewVersion,
        updated = erlang:system_time(seconds)
    },
    persistent_term:put({?MODULE, pools}, NewPools),
    ?LOG_INFO("[CREDIS] NEW SLOTS ~p", [Pools]),

    %% close old pools
    lists:foreach(fun(#pool{name = Name})-> 
        case lists:keyfind(Name, 2, Pools) of
            false -> 
                    catch erlpool:stop_pool(Name),
                    ?LOG_INFO("[CREDIS] CLOSED SLOT ~p", [Name]);
            _ -> ok
        end
    end, PrevPools#pools.pools),

    State#state{
        version = NewVersion,
        nodes   = lists:usort(Nodes) ++ State#state.init_nodes
    }.



get_name(Host, Port) ->
    list_to_atom(Host ++ "#" ++ integer_to_list(Port)).



get_slots([]) ->
    undefined;
get_slots([{Host, Port} | Nodes])->
    
    process_flag(trap_exit, true),
    Result = eredis:start_link(Host, Port, 0, "", no_reconnect, 500),
    process_flag(trap_exit, false),

    case Result of
        {ok, Connection} ->
          
          case eredis:q(Connection, ["CLUSTER", "SLOTS"]) of

            {ok, ClusterInfo} ->
                eredis:stop(Connection),
                ?LOG_INFO("[CREDIS] SLOTS OK", []),
                ClusterInfo;

            {error,<<"ERR unknown command 'CLUSTER'">>} = Err ->
                ?LOG_ERR("[CREDIS] err ~s ~p", [Host, Err]),
                eredis:stop(Connection),
                get_slots(Nodes);

            {error,<<"ERR This instance has cluster support disabled">>} = Err ->
                ?LOG_ERR("[CREDIS] err ~s ~p", [Host, Err]),
                eredis:stop(Connection),
                get_slots(Nodes);

            ErrOther ->
                ?LOG_ERR("[CREDIS] err ~s ~p", [Host, ErrOther]),
                eredis:stop(Connection),
                get_slots(Nodes)
        end;
        _ ->
            ?LOG_ERR("[CREDIS] err ~s ~p", [Host, Result]),
            get_slots(Nodes)
  end.














get_pool_by_slot(Slot)->
    Pools = get_pools(),
    PoolName = get_pool_by_slot_item(Slot, Pools#pools.pools),
    if
        PoolName =/= undefined ->
            {PoolName, Pools#pools.version};
        true ->
            {undefined, Pools#pools.version}
    end.
get_pool_by_slot_item(_, [])->
    undefined;
get_pool_by_slot_item(Slot, [#pool{s_start = Start, s_end = End, name = Name} | _])when Slot >= Start, Slot =< End ->
    Name;
get_pool_by_slot_item(Slot, [_ | Rest])->
    get_pool_by_slot_item(Slot, Rest).





%% =============================================================================
%% @doc Return the first key in the command arguments.
%% In a normal query, the second term will be returned
%%
%% If it is a pipeline query we will use the second term of the first term, we
%% will assume that all keys are in the same server and the query can be
%% performed
%%
%% If the pipeline query starts with multi (transaction), we will look at the
%% second term of the second command
%%
%% For eval and evalsha command we will look at the fourth term.
%%
%% For commands that don't make sense in the context of cluster
%% return value will be undefined.
%% @end
%% =============================================================================
get_key_from_command([[X|Y]|Z]) when is_bitstring(X) ->
    get_key_from_command([[bitstring_to_list(X)|Y]|Z]);
get_key_from_command([[X|Y]|Z]) when is_list(X) ->
    case string:to_lower(X) of
        "multi" ->
            get_key_from_command(Z);
        _ ->
            get_key_from_command([X|Y])
    end;
get_key_from_command([Term1,Term2|Rest]) when is_bitstring(Term1) ->
    get_key_from_command([bitstring_to_list(Term1),Term2|Rest]);
get_key_from_command([Term1,Term2|Rest]) when is_bitstring(Term2) ->
    get_key_from_command([Term1,bitstring_to_list(Term2)|Rest]);
get_key_from_command([Term1,Term2|Rest]) ->
    case string:to_lower(Term1) of
        "info" ->
            undefined;
        "config" ->
            undefined;
        "shutdown" ->
            undefined;
        "slaveof" ->
            undefined;
        "eval" ->
            get_key_from_rest(Rest);
        "evalsha" ->
            get_key_from_rest(Rest);
        _ ->
            Term2
    end;
get_key_from_command(_) ->
    undefined.

%% =============================================================================
%% @doc Get key for command where the key is in th 4th position (eval and
%% evalsha commands)
%% @end
%% =============================================================================
get_key_from_rest([_,KeyName|_]) when is_bitstring(KeyName) ->
    bitstring_to_list(KeyName);
get_key_from_rest([_,KeyName|_]) when is_list(KeyName) ->
    KeyName;
get_key_from_rest(_) ->
    undefined.

get_key_slot(Key) when is_bitstring(Key) ->
    get_key_slot(bitstring_to_list(Key));
get_key_slot(Key) ->
    KeyToBeHased = case string:chr(Key,${) of
        0 ->
            Key;
        Start ->
            case string:chr(string:substr(Key,Start+1),$}) of
                0 ->
                    Key;
                Length ->
                    if
                        Length =:= 1 ->
                            Key;
                        true ->
                            string:substr(Key,Start+1,Length-1)
                    end
            end
    end,
    redis_cluster_hash:hash(KeyToBeHased).