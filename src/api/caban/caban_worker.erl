%%%-------------------------------------------------------------------
%%% @author Caban
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(caban_worker).

-behaviour(gen_server).

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3, set_parameter/2, get_parameter/1]).

-compile([export_all]).

-define(SERVER, ?MODULE).

-record(caban_worker_state, {num, count}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link(Num, Count) ->
  gen_server:start_link({local,list_to_atom(integer_to_list(Num))},?MODULE,[Num, Count],[]).

init([Num, Count]) ->
  {ok, #caban_worker_state{num = Num, count = Count}}.

handle_call(_Request, _From, State) ->
  #caban_worker_state{num = Num, count = Count} = State,
  State1 = #caban_worker_state{num = Num, count = Count},
  {reply, State1, State1}.

handle_cast(_Request, State) ->
  {_, Num, Value} = _Request,
  log:info( "TEST ======================================= Num ~p Count ~p ", [Num, Value]),
  State1 = #caban_worker_state{num=Num, count=Value},
  {noreply, State1}.

handle_info(_Info, State = #caban_worker_state{}) ->
  {noreply, State}.

terminate(_Reason, _State = #caban_worker_state{}) ->
  ok.

code_change(_OldVsn, State = #caban_worker_state{}, _Extra) ->
  {ok, State}.
%%%===================================================================
%%% Internal functions
%%%===================================================================
set_parameter(Num, Value) ->
  gen_server:cast(list_to_atom(integer_to_list(Num)), {set_parameter, Num, Value}).
% ==============================================================================================
% ===
% ==============================================================================================
get_parameter(Num) ->
  {caban_worker_state, Num1, Count1} = gen_server:call(list_to_atom(integer_to_list(Num)),{get_parameter, Num}),
  {Num1, Count1}.
% ==============================================================================================
% ===
% ==============================================================================================