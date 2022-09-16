-module(caban_storage).
-author("Caban").

%% API
-export([
  get_all_workers/0,
  set_worker_value/2,
  get_worker_value/1
]).

% ==============================================================================================
% ===
% ==============================================================================================
get_all_workers() ->
  Storage = erl_template_app:get_storage_name(),
  case Storage of
    postgres  ->  get_all_workers( postgres );
    redis     ->  get_all_workers( redis, 10, [] );
    _         ->  ok
  end.
% ==============================================================================================
get_all_workers( postgres ) ->
  db_postgres:query(epgsql_pool_master, get_workers_values, []).
% ==============================================================================================
get_all_workers( redis, N, WorkersList ) when ( N > 0 )  ->
  db_redis:q(["MSET" | [N, 0]]),
  WorkersList1 =[[{ <<"worker_name">>, integer_to_binary(N) }, { <<"worker_value">>, <<"0">> }] | WorkersList],
  get_all_workers( redis, N-1, WorkersList1 );
% ==============================================================================================
get_all_workers( redis, _, WorkersList )  ->  WorkersList.
% ==============================================================================================
% ===
% ==============================================================================================
set_worker_value( Worker, Value ) ->
    set_worker_value( Worker, Value, erl_template_app:get_storage_name() ).
% ==============================================================================================
set_worker_value( Worker, Value, postgres ) ->
  Result = db_postgres:query(epgsql_pool_master, set_worker_value,
    [ { <<"worker_value">>, integer_to_list(Value) },
      { <<"worker_name">>, integer_to_list(Worker) }
    ]),
  log:info("Set worker postgres: ~p ~n", [Result]),
  Result;
% ==============================================================================================
set_worker_value( Worker, Value, redis ) ->
  Result = db_redis:q(["MSET" | [Worker, Value]]),
  log:info("Set worker redis: ~p ~n", [Result]),
  Result.
% ==============================================================================================
% ===
% ==============================================================================================
get_worker_value( Worker ) ->
  get_worker_value( Worker, erl_template_app:get_storage_name() ).
% ==============================================================================================
get_worker_value( Worker, postgres ) ->
  Result = db_postgres:query(epgsql_pool_master, get_worker_value,
    [{ <<"worker_name">>, integer_to_list(Worker) }]),
  log:info("Get worker postgres: ~p ~n", [Result]),
  Result;
% ==============================================================================================
get_worker_value( Worker, redis ) ->
  Result = db_redis:q(["MGET" | [Worker]]),
  log:info("Get worker redis: ~p ~n", [Result]),
  Result.
% ==============================================================================================
% ===
% ==============================================================================================
