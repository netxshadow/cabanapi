-module(erl_template_request).


-export ([
  request/4, 
  request/5, 
  request/6, 
  request/7
]).

-define(HTTPCOPTS, [{sync, true}, {body_format, binary}]).
% ==============================================================================================
% ===
% ==============================================================================================
request(get, Url, Headers, HttpOpts) -> 
  log:info("[~p] incoming request url: ~s~n", [self(), Url]),
  log:info("[~p] incoming request headers: ~p~n", [self(), Headers]),
  req(get, {Url, Headers}, HttpOpts, ?HTTPCOPTS, 0).
% ==============================================================================================
request(get, Url, Headers, HttpOpts, HttpCopts) -> 
  log:info("[~p] incoming request url: ~s~n", [self(), Url]),
  log:info("[~p] incoming request headers: ~p~n", [self(), Headers]),
  req(get, {Url, Headers}, HttpOpts ++ HttpCopts, ?HTTPCOPTS, 0).
% ==============================================================================================
request( Method, Url, Headers, "application/x-www-form-urlencoded; charset=utf-8", Body, HttpOpts) -> 
  log:info("[~p] incoming request url: ~s~n", [self(), Url]),
  log:info("[~p] incoming request headers: ~p~n", [self(), Headers]),
  log:info("[~p] incoming request body: ~s~n", [self(), Body]),

  req(Method, {Url, Headers, "application/x-www-form-urlencoded; charset=utf-8", Body}, HttpOpts, ?HTTPCOPTS, 0);
% ==============================================================================================
request( Method, Url, Headers, ContType, Body, HttpOpts) -> 
  log:info("[~p] incoming request url: ~s~n", [self(), Url]),
  log:info("[~p] incoming request headers: ~p~n", [self(), Headers]),
  log:info("[~p] incoming request body: ~s~n", [self(), (Body)]),

  req(Method, {Url, Headers, ContType, Body}, HttpOpts, ?HTTPCOPTS, 0).
% ==============================================================================================
request( Method, Url, Headers, ContType, Body, HttpOpts, HttpCopts) -> 
  log:info("[~p] incoming request url: ~s~n", [self(), Url]),
  log:info("[~p] incoming request headers: ~p~n", [self(), Headers]),
  log:info("[~p] incoming request body: ~s~n", [self(), (Body)]),

  req(Method, {Url, Headers, ContType, Body}, HttpOpts, ?HTTPCOPTS ++ HttpCopts, 0).
% ==============================================================================================
% ===
% ==============================================================================================
req(Method, Param, HttpOpts1, Options, Count) ->
  HttpOpts  = [{timeout, 40000}] ++ HttpOpts1,
  % HttpOpts  = HttpOpts1,
  % Time      = erlang:system_time(nanosecond),
  Resp      = httpc:request(Method, Param, HttpOpts, Options),
  % metrics:histogram_inc(http_incoming_request_duration_milliseconds, [], (erlang:system_time(nanosecond) - Time)),
  response(Resp, {Method, Param, proplists:delete(timeout, HttpOpts), Options}, Count).
% ==============================================================================================
% ===
% ==============================================================================================
response({error,socket_closed_remotely}, _, 3) ->
  log:info("[~p] {error,socket_closed_remotely} can not resend ~n", [self()]),
  {error,socket_closed_remotely};
% ==============================================================================================
response({error,socket_closed_remotely}, {Method, Param, HttpOpts, Options}, Count) ->
  log:info("[~p] {error,socket_closed_remotely} ~n", [self()]),
  req(Method, Param, HttpOpts, Options, Count + 1);
% ==============================================================================================
response({ok, {{_, 200, _}, _, RespBody}}, _, _) ->
  case jsx:is_json(RespBody) of
    true -> 
      log:info("[~p] incoming response code: ~p, body: ~s~n", [self(), 200, RespBody]),
      % metrics:response_data(ecom_action_response, jsx:decode(RespBody)),
      {ok, 200, jsx:decode(RespBody)};
    false -> 
      log:info("[~p] incoming response code: ~p, body: ~p~n", [self(), 200, RespBody]),
      {ok, 200, RespBody}
  end;
% ==============================================================================================
response({ok, {{_, HttpCode, _}, _, RespBody}}, _, _) ->
  case jsx:is_json(RespBody) of
    true -> 
      log:info("[~p] incoming response code: ~p, body: ~s~n", [self(), HttpCode, RespBody]),
      % metrics:response_data(ecom_action_response, jsx:decode(RespBody)),
      {error, HttpCode, jsx:decode(RespBody)};
    false -> 
      log:info("[~p] incoming response code: ~p, body: ~p~n", [self(), HttpCode, RespBody]),
      {error, HttpCode, RespBody}
  end;
% ==============================================================================================
response(Any, _, _) -> 
  log:info("[~p] incoming error response: ~p~n", [self(), Any]),
  Any.
% ==============================================================================================
% ===
% ==============================================================================================
