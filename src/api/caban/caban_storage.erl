-module(caban_storage).
-author("Caban").

%% API
-export([
  get_all_workers/0,
  set_worker_value/2,
  get_worker_value/1
]).

get_all_workers() ->
  Storage = erl_template_app:get_storage_name(),
  case Storage of
    postgres  ->  get_all_workers_from_postgres();
    redis     ->  get_all_workers_from_redis(10, []);
    _         ->  ok
  end.
% ==============================================================================================
% ===
% ==============================================================================================
get_all_workers_from_postgres() ->
  db_postgres:query(epgsql_pool_master, get_workers_values, []).
% ==============================================================================================
% ===
% ==============================================================================================
get_all_workers_from_redis(N, WorkersList)  ->
  case N > 0 of
    true  ->
      db_redis:q(["MSET" | [N, 0]]),
      WorkersList1 =[[{ <<"worker_name">>, integer_to_binary(N) }, { <<"worker_value">>, <<"0">> }] | WorkersList],
      get_all_workers_from_redis(N-1, WorkersList1);
    false -> WorkersList
  end.
% ==============================================================================================
% ===
% ==============================================================================================
set_worker_value(Worker, Value) ->
  Storage = erl_template_app:get_storage_name(),
  case Storage of
    postgres  ->  set_worker_postgres(Worker, Value);
    redis     ->  set_worker_redis(Worker, Value);
    _         ->  ok
  end.
% ==============================================================================================
% ===
% ==============================================================================================
set_worker_postgres(Worker, Value) ->
  Result = db_postgres:query(epgsql_pool_master, set_worker_value,
    [ { <<"worker_value">>, integer_to_list(Value) },
      { <<"worker_name">>, integer_to_list(Worker) }
    ]),
  log:info("Set worker postgres: ~p ~n", [Result]),
  Result.
% ==============================================================================================
% ===
% ==============================================================================================
set_worker_redis(Worker, Value) ->
  Result = db_redis:q(["MSET" | [Worker, Value]]),
  log:info("Set worker redis: ~p ~n", [Result]),
  Result.
% ==============================================================================================
% ===
% ==============================================================================================
get_worker_value(Worker) ->
  Storage = erl_template_app:get_storage_name(),
  case Storage of
    postgres  ->  get_worker_postgres(Worker);
    redis     ->  get_worker_redis(Worker);
    _         ->  ok
  end.
% ==============================================================================================
% ===
% ==============================================================================================
get_worker_postgres(Worker) ->
  Result = db_postgres:query(epgsql_pool_master, get_worker_value,
    [{ <<"worker_name">>, integer_to_list(Worker) }]),
  log:info("Get worker postgres: ~p ~n", [Result]),
  Result.
% ==============================================================================================
% ===
% ==============================================================================================
get_worker_redis(Worker) ->
  Result = db_redis:q(["MGET" | [Worker]]),
  log:info("Get worker redis: ~p ~n", [Result]),
  Result.
% ==============================================================================================
% ===
% ==============================================================================================
