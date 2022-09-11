-module(redis_cluster_sup).
-behaviour(supervisor).

%% Supervisor.
-export([start_link/1]).
-export([init/1]).


start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Args]).


init([Args]) ->
    Procs = [{redis_cluster,{redis_cluster, start_link, [Args]}, permanent, 5000, worker, [dynamic]}],
    {ok, {{one_for_one, 100, 1}, Procs}}.
