-module(caban_storage).
-author("Caban").

%% API
-export([
  get_workers_values/0,
  set_worker_value/2
]).

get_workers_values() ->
  Storage = erl_template_app:get_storage_name(),
  case Storage of
    postgres  ->  WorkersList = db_postgres:query(epgsql_pool_master, get_workers_values, []);
      %%log:info( "==================Workers List: ~p ", [WorkersList]), WorkersList;
    redis     ->  WorkersList = get_workers_from_redis(10, []);
      %%log:info( "==================Workers List: ~p ", [WorkersList]), WorkersList;
    _         ->  ok
  end.

set_worker_value(Worker, Value) ->
  Storage = erl_template_app:get_storage_name(),
  case Storage of
    postgres  ->
      Query = "update workers set worker_value = "
        ++ integer_to_list(Value) ++ " where worker_name = " ++ "'" ++ integer_to_list(Worker) ++ "'",
        db_postgres:query(epgsql_pool_master, custom_query, Query);
      %%log:info( "==================Query result: ~p ", [Result])
    redis     ->  db_redis:q(["MSET" | [Worker, Value]]);
    _         ->  ok
  end.

get_workers_from_redis(N, WorkersList)  ->
  case N > 0 of
    true  ->
      db_redis:q(["MSET" | [N, 0]]),
      WorkersList1 =[[{ <<"worker_name">>, integer_to_binary(N) }, { <<"worker_value">>, <<"0">> }] | WorkersList],
      get_workers_from_redis(N-1, WorkersList1);
    false -> WorkersList
  end.