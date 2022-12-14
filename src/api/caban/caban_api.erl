-module(caban_api).
-author("Caban").

-export([
  request_v1/2, request_inc/2, request_dec/2, request_status/2
]).

% ==============================================================================================
% ===
% ==============================================================================================
request_v1(_, _) ->
  {200, #{ <<"caban result">> => <<"ok">> }}.
% ==============================================================================================
% ===
% ==============================================================================================
request_inc(Data, _) ->
  log:info("========== CABAN API TEST INC ======= ~p : ~s~n", [self(), Data]),
  WorkerID = maps:get(<<"worker">>, Data),
  Response = caban_controller:set_worker_inc(WorkerID),
  {200, Response}.
% ==============================================================================================
% ===
% ==============================================================================================
request_dec(Data, _) ->
  log:info("========== CABAN API TEST DEC ======= ~p : ~s~n", [self(), Data]),
  WorkerID = maps:get(<<"worker">>, Data),
  Response = caban_controller:set_worker_dec(WorkerID),
  {200, Response}.
% ==============================================================================================
% ===
% ==============================================================================================
request_status(Data, _) ->
  WorkerID = maps:get(<<"worker">>, Data),
  WorkersStatus = caban_controller:get_workers(WorkerID),
  log:info("========== CABAN API TEST STATUS ======= ~p : ~s~n", [self(), WorkersStatus]),
  {200, WorkersStatus }.
% ==============================================================================================
% ===
% ==============================================================================================
