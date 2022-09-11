-module(caban_controller).

-behaviour(gen_server).

-export([
  start_link/2,
  get_workers/1,
  start_all_workers/3,
  start_code_workers/3,
  get_workers_data/1,
  loop/1,
  loop/2,
  start_storage_workers/2
]).

-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-compile([export_all]).

-define(SERVER, ?MODULE).

-record(caban_controller_state, {num, workers}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link(Num, Workers) ->
  gen_server:start_link({local,list_to_atom(integer_to_list(Num))},?MODULE,[Num, Workers],[]).

init([Num, Workers]) ->
  {ok, #caban_controller_state{num = Num, workers = Workers}}.

handle_call(_Request, _From, State = #caban_controller_state{}) ->
  #caban_controller_state{num = Num, workers = Workers} = State,
  State1 = #caban_controller_state{num = Num, workers = Workers},
  {reply, State, State1}.

handle_cast(_Request, State = #caban_controller_state{}) ->
  {Num, M} = _Request,
  State1 = #caban_controller_state{num=Num, workers = M},
  {noreply, State1}.

handle_info(_Info, State = #caban_controller_state{}) ->
  {noreply, State}.

terminate(_Reason, _State = #caban_controller_state{}) ->
  ok.

code_change(_OldVsn, State = #caban_controller_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

start_all_workers(Self, N, StartFrom)  ->
  case StartFrom of
    storage ->
      WorkersList = caban_storage:get_workers_values(),
      WorkersMap = start_storage_workers(WorkersList, #{}),
      log:info( "===Workers map created: ~p ", [WorkersMap]),
      gen_server:cast(list_to_atom(integer_to_list(Self)), {start_all_workers, WorkersMap});
    code ->
      WorkersMap = start_code_workers(1, N, #{}),
      log:info( "===Workers map created: ~p ", [WorkersMap]),
      gen_server:cast(list_to_atom(integer_to_list(Self)), {start_all_workers, WorkersMap})
  end.
% ==============================================================================================
start_code_workers(I, N, M) ->
  case (I =< N) of
    true ->
      {ok, Pid} = caban_sup:start_worker(I, 0),
      M1 = maps:merge(M, #{I => Pid}),
      log:info( "Start worker with Name: ~p and Pid: ~p ", [I, Pid]),
      unlink(Pid),
      start_code_workers(I+1, N, M1);
    false -> M
  end.
% ==============================================================================================
% ===
% ==============================================================================================
start_storage_workers([], M) -> M;
start_storage_workers([H|T], M) ->
  WorkerName = proplists:get_value(<<"worker_name">>, H),
  WorkerValue = proplists:get_value(<<"worker_value">>, H),
  {ok, Pid} = caban_sup:start_worker(binary_to_integer(WorkerName), binary_to_integer(WorkerValue)),
  M1 = maps:merge(M, #{binary_to_integer(WorkerName) => Pid}),
  log:info( "Starting worker : ~p with value: ~p ", [binary_to_atom(WorkerName), binary_to_atom(WorkerValue)]),
  start_storage_workers(T, M1).
% ==============================================================================================
% ===
% ==============================================================================================
get_workers_data(Map) ->
  loop(Map).
% ==============================================================================================
loop(Map) when is_map(Map) ->
  Keys = maps:keys(Map),
  loop(Map, Keys).
% ==============================================================================================
loop(Map , []) ->
  Map;
% ==============================================================================================
loop(Map, [Head|Tail]) ->
  %%Value = maps:get(Head, Map),
  {Num, Count} = caban_worker:get_parameter(Head),
  Map1 = maps:merge(Map, #{Num => Count}),
  loop(Map1, Tail).
% ==============================================================================================
% ===
% ==============================================================================================
get_workers(N) ->
  case (N == <<"all">>) of
    true ->
      Map = get_workers_map(),
      M = get_workers_data(Map), M;
    false -> {Num, Count} = caban_worker:get_parameter(N), #{Num => Count}
  end.
% ==============================================================================================
% ===
% ==============================================================================================
get_workers_map() ->
  {caban_controller_state, _, Map} = gen_server:call(list_to_atom(integer_to_list(0)),{get_workers_map, 0}),
  Map.
% ==============================================================================================
% ===
% ==============================================================================================
set_worker_inc(N)  ->
  {Num, Count} = caban_worker:get_parameter(N),
  caban_worker:set_parameter(N, Count+1),
  caban_storage:set_worker_value(N, Count+1),
  #{N => ok}.
% ==============================================================================================
% ===
% ==============================================================================================
set_worker_dec(N)  ->
  {Num, Count} = caban_worker:get_parameter(N),
  caban_worker:set_parameter(N, Count-1),
  caban_storage:set_worker_value(N, Count-1),
  #{N => ok}.
% ==============================================================================================
% ===
% ==============================================================================================
set_worker_custom(W, N)  ->
  #{N => ok}.
% ==============================================================================================
% ===
% ==============================================================================================
