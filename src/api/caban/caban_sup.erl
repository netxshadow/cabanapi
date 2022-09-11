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
