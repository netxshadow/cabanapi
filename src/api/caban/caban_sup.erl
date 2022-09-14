-module(caban_sup).
-behaviour(supervisor).
-compile([export_all]).

start_link()->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
  SupervisorSpecification = {one_for_all, 10, 60},
  ChildSpecifications =
    [],
  {ok, {SupervisorSpecification, ChildSpecifications}}.

start_caban() ->
  Config            = application:get_all_env( erl_template ),
  ControllerName    = proplists:get_value(controller_name, Config),
  WorkersCount      = proplists:get_value(workers_count, Config),
  WorkersStartFrom  = proplists:get_value(workers_start_from, Config),
  caban_sup:start_controller(ControllerName, #{}),
  %===Стартуем и инициализируем воркеры (code/storage) либо из кода, либо из стореджа (postgres/redis)===%
  caban_controller:start_all_workers(ControllerName, WorkersCount, WorkersStartFrom).

start_controller(Num, _) ->
  {ok, Pid} = supervisor:start_child(
    caban_sup,
    {Num,
      {caban_controller, start_link, [Num, #{}]},
      permanent,
      2000,
      worker,
      [caban_controller]}),
  {ok, Pid}.

start_worker(Num, Count) ->
  {ok, Pid} = supervisor:start_child(
    caban_sup,
    {Num,
      {caban_worker, start_link, [Num, Count]},
      permanent,
      2000,
      worker,
      [caban_worker]}),
  {ok, Pid}.