-module( erl_template_api_handler ).

-include("../include/api_errors.hrl").

-define(HEADERS, #{
  <<"Content-Type">> => <<"application/json">>,
  <<"Access-Control-Allow-Origin">> => <<"*">>,
  <<"Access-Control-Allow-Credentials">> => <<"true">>,
  <<"Access-Control-Allow-Methods">> => <<"GET, POST, PUT, DELETE, OPTIONS">>,
  <<"Access-Control-Allow-Headers">> => <<"Accept,Authorization,Cache-Control,Content-Type,DNT,If-Modified-Since,Keep-Alive,Origin,User-Agent,X-Requested-With,Access-Control-Allow-Origin,header">>
}).

-define(HEADERS_H, #{
  <<"Content-Type">> => <<"html">>,
  <<"Access-Control-Allow-Origin">> => <<"*">>,
  <<"Access-Control-Allow-Credentials">> => <<"true">>,
  <<"Access-Control-Allow-Methods">> => <<"GET, POST, PUT, DELETE, OPTIONS">>,
  <<"Access-Control-Allow-Headers">> => <<"Accept,Authorization,Cache-Control,Content-Type,DNT,If-Modified-Since,Keep-Alive,Origin,User-Agent,X-Requested-With,Access-Control-Allow-Origin,header">>
}).

-export([
  init/2
]).

% ==============================================================================================
% ===
% ==============================================================================================
init( Request, Opts ) ->
  io:format("In Request: ~p~n",[cowboy_req:headers(Request)]),
  Path    = cowboy_req:path( Request ),
  is_health_check(Path, Request, Opts).
  % method( cowboy_req:method( Request ), Headers, Request, Time, Opts ).
% ==============================================================================================
% ===
% ==============================================================================================
is_health_check( <<"/api/health_check">>, Request, Opts ) -> 
  response( is_health_check_result(), Request, 0, Opts );
% ==============================================================================================
is_health_check( Path, Request, Opts ) ->
  Time    = erlang:system_time(nanosecond),
  Headers = maps:get( headers, Request, #{} ),
  method( cowboy_req:method( Request ), Path, Headers, Request, Time, Opts ).
% ==============================================================================================
% ===
% ==============================================================================================
method( <<"POST">>, Path, Headers, Req, Time, Opts ) ->
  { ok, Body, Request } = cowboy_req:read_body( Req ),
  % io:format("In Body: ~p~n",[Body]),
  % log:info("Request Data: ~p~n",[Body]),
  json( post, jsx:is_json(Body), Body, Path, Headers, Request, Time, Opts );
method( <<"GET">>, Path, Headers, Req, Time, Opts ) ->
  json( get, false, <<>>, Path, Headers, Req, Time, Opts );
method( _, _, _, Request, Time, Opts ) ->
  response( ?METHOD_NOT_ALLOWED, Request, Time, Opts ).
% ==============================================================================================
% ===
% ==============================================================================================
json( get, _, _, Path, Headers, Request, Time, Opts ) ->
  QS = cowboy_req:parse_qs( Request ),
  log:info("[~p] erl_template request headers: ~s~n", [self(), jsx:encode(Headers)]),
  log:info("[~p] erl_template request content-type: ~s~n", [self(), (maps:get( <<"content-type">>, Headers, <<"unknown">> ))]),
  log:info("[~p] erl_template request GET data : ~p~n", [self(), QS]),
  path( get, Path, <<>>, maps:from_list( QS ), Request, Time, Opts );
json( post, true, Body, Path, Headers, Request, Time, Opts ) ->
  QS = cowboy_req:parse_qs( Request ),
  log:info("[~p] erl_template request headers: ~s~n", [self(), jsx:encode(Headers)]),
  log:info("[~p] erl_template request content-type: ~s~n", [self(), (maps:get( <<"content-type">>, Headers, <<"unknown">> ))]),
  log:info("[~p] erl_template request POST data: ~s~n", [self(), ( Body )]),
  path( post, Path, jsx:decode( Body ), maps:from_list( QS ), Request, Time, Opts );
json( post, false, Body, Path, Headers, Request, Time, Opts ) ->
  CType = maps:get( <<"content-type">>, cowboy_req:headers(Request) ),
  content_type( CType, Body, Path, Headers, Request, Time, Opts ).
% ==============================================================================================
% ===
% ==============================================================================================
content_type( <<"application/x-www-form-urlencoded">>, Body, Path, Headers, Request, Time, Opts ) ->
  QS      = cow_qs:parse_qs(Body),
  log:info("[~p] erl_template request headers: ~s~n", [self(), jsx:encode(Headers)]),
  log:info("[~p] erl_template request content-type: ~s~n", [self(), (<<"application/x-www-form-urlencoded">>)]),
  log:info("[~p] erl_template request POST data : ~s~n", [self(), jsx:encode(QS)]),
  path( post, Path, maps:from_list( QS ),  maps:from_list(cowboy_req:parse_qs( Request )), Request, Time, Opts );
content_type( _, _, _, _, Request, Time, Opts ) ->
  response( ?WRONG_REQUEST_DATA, Request, Time, Opts ).
% ==============================================================================================
% ===
% ==============================================================================================
path( _, <<"/api/health_check">>, _, _, Request, Time, Opts ) ->
  response( { 200, #{ <<"result">> => <<"ok">> }, health_check }, Request, Time, Opts );
path( Method, Path, Body, QS, Request, Time, Opts ) ->
  log:info("[~p] erl_template request path: ~s~n", [self(), Path]),
  Schema = ets:match( api_rules, { binary_to_list( Path ), '$1'} ),
  schema( Method, Schema, Body, QS, Request, Time, Opts ).
% ==============================================================================================
% ===
% ==============================================================================================
schema( _, [], _, _, Request, Time, Opts ) ->
  response( ?PATH_NOT_ALLOWED, Request, Time, Opts );
schema( get, [[Schema]], Body, QS, Request, Time, Opts ) ->
  Arguments = proplists:get_value( arguments, Schema ),
  Z = validation:validate( [], Arguments ),
  result( Z, Body, QS, Request, Time, Opts, Schema );
schema( post, [[Schema]], Body, QS, Request, Time, Opts ) when is_map( Body ) ->
  Arguments = proplists:get_value( arguments, Schema ),
  Z = validation:validate( maps:to_list(Body), Arguments ),
  result( Z, Body, QS, Request, Time, Opts, Schema ).
% ==============================================================================================
% ===
% ==============================================================================================  
result( ok, Body, QS, Request, Time, Opts, Schema ) ->
  Module    = proplists:get_value( module, Schema ),
  Function  = proplists:get_value( function, Schema ),
  Fields    = proplists:get_value( token_fields, Schema, [] ),
  Validate  = proplists:get_value( token_validation, Schema, true ),
  Result    = code:ensure_loaded( Module ),
  is_module( Result, Module, Function, Fields, Validate, Body, QS, Request, Time, Opts );
result( { error, Response }, _, _, Request, Time, Opts, _ ) ->
  response( Response, Request, Time, Opts ).
% ==============================================================================================
% ===
% ==============================================================================================
is_module( _, checkout, session_v1, Fields, Validate, Body, QS, Request, Time, Opts ) ->
  Headers   = maps:get( headers, Request, #{} ),
  AuthToken = maps:get( <<"x-auth-token">>, Headers, <<>>),
  Result = db_postgres:query( epgsql_pool_master,  merchant_public_key, QS ),
  { ok, IsTest } = application:get_env( erl_template, test_env ),
  is_merchant( Result, <<"GET">>, Fields, Validate, AuthToken, IsTest, Body, QS, checkout, session_v1, Request, Time, Opts );
% ==============================================================================================
is_module( { module, admin }, admin, session, Fields, Validate, Body, QS, Request, Time, Opts ) ->
  % Result = erlang:function_exported( Module, Function, 2),
  Method = cowboy_req:method( Request ),
  is_function( true, admin, session, Fields, Validate, Body, QS, Method, Request, Time, Opts );
% ==============================================================================================
is_module( { module, Module }, Module, Function, Fields, Validate, Body, QS, Request, Time, Opts ) ->
  Result = erlang:function_exported( Module, Function, 2),
  Method = cowboy_req:method( Request ),
  is_function( Result, Module, Function, Fields, Validate, Body, QS, Method, Request, Time, Opts );
is_module( { error, _ }, _, _, _, _, _, _, Request, Time, Opts ) ->
  response( ?NO_PROCESS_FUN, Request, Time, Opts ).
% ==============================================================================================
% ===
% ==============================================================================================
is_function( true, Module, Function, Fields, false, <<>>, QS, <<"GET">>, Request, Time, Opts ) ->
  IsTest    = application:get_env( erl_template, test_env, false ),
  is_merchant( <<>>, <<"GET">>, Fields, false, <<>>, IsTest, #{}, QS, Module, Function, Request, Time, Opts );
% ==============================================================================================
is_function( true, Module, Function, Fields, false, Body, QS, <<"GET">>, Request, Time, Opts ) ->
  IsTest    = application:get_env( erl_template, test_env, false ),
  is_merchant( <<>>, <<"GET">>, Fields, false, <<>>, IsTest, Body, QS, Module, Function, Request, Time, Opts );
% ==============================================================================================
is_function( true, Module, Function, Fields, false, Body, QS, <<"POST">>, Request, Time, Opts ) ->
  IsTest    = application:get_env( erl_template, test_env, false ),
  is_merchant( <<>>, <<"POST">>, Fields, false, <<>>, IsTest, Body, QS, Module, Function, Request, Time, Opts );
% ==============================================================================================
is_function( true, Module, Function, Fields, Validate, <<>>, QS, <<"GET">>, Request, Time, Opts ) ->
  Headers   = maps:get( headers, Request, #{} ),
  AuthToken = maps:get( <<"x-auth-token">>, Headers, <<>>),
  Result    = merchants:is_merchant( maps:get( <<"merchant_name">>, #{}, <<>> ) ),
  IsTest    = application:get_env( erl_template, test_env, false ),
  is_merchant( Result, <<"GET">>, Fields, Validate, AuthToken, IsTest, #{}, QS, Module, Function, Request, Time, Opts );
% ==============================================================================================
is_function( true, Module, Function, Fields, Validate, Body, QS, <<"GET">>, Request, Time, Opts ) ->
  Headers   = maps:get( headers, Request, #{} ),
  AuthToken = maps:get( <<"x-auth-token">>, Headers, <<>>),
  Result    = merchants:is_merchant( maps:get( <<"merchant_name">>, Body, <<>> ) ),
  IsTest    = application:get_env( erl_template, test_env, false ),
  is_merchant( Result, <<"GET">>, Fields, Validate, AuthToken, IsTest, Body, QS, Module, Function, Request, Time, Opts );
% ==============================================================================================
is_function( true, Module, Function, Fields, Validate, Body, QS, <<"POST">>, Request, Time, Opts ) ->
  Headers   = maps:get( headers, Request, #{} ),
  AuthToken = maps:get( <<"x-auth-token">>, Headers, <<>>),
  %===Plug from caban===%
  Result    = caban_merchants:is_merchant( maps:get( <<"merchant_name">>, Body, <<>> ) ),
  %===End plug===%
  IsTest    = application:get_env( erl_template, test_env, false ),
  is_merchant( Result, <<"POST">>, Fields, Validate, AuthToken, IsTest, Body, QS, Module, Function, Request, Time, Opts );
% ==============================================================================================
is_function(false, _, _, _, _, _, _, _, Request, Time, Opts) ->
  response( ?NO_PROCESS_FUN, Request, Time, Opts ).
% ==============================================================================================
% ===
% ==============================================================================================
is_merchant( _, _, _, <<"f1f1313234f1ecec32f4fb33">>, _, true, Body, QS, Module, Function, Request, Time, Opts ) ->
  log:info( "HTTP Request: ~p~n", [Body] ),
  response( Module:Function( Body, QS ), Request, Time, Opts );
is_merchant( [], _, _, true, _, _, _, _, _, _, Request, Time, Opts ) ->
  response( ?WRONG_MERCHANT, Request, Time, Opts );
is_merchant( _, _, _, true, <<>>, _, _, _, _, _, Request, Time, Opts ) ->
  response( ?INVALID_TOKEN, Request, Time, Opts );
  % checkout:session_v1

is_merchant( _, _, _, false, _, _, Body, _, admin, session, Request, Time, Opts ) ->
  % log:info( "HTTP Request : ~p~n", [Body] ),
  Headers1   = maps:get( headers, Request, #{} ),
  Headers    = maps:to_list(Headers1),
  response( admin:session( Body, Headers, cowboy_req:parse_cookies(Request) ), Request, Time, Opts );

is_merchant( _, _, _, false, _, _, Body, QS, checkout, session_v1, Request, Time, Opts ) ->
  Headers   = maps:get( headers, Request, #{} ),
  response( checkout:session_v1( Body, QS, Headers ), Request, Time, Opts );
is_merchant( _, _, _, false, _, _, Body, QS, Module, Function, Request, Time, Opts ) ->
  % log:info( "HTTP Request 11111: ~p~n", [Body] ),
  response( Module:Function( Body, QS ), Request, Time, Opts );
is_merchant( <<>>, _, _, _, _, _, _, _, _, _, Request, Time, Opts ) ->
  % log:info( "HTTP Request 11111: ~p~n", [Body] ),
  response( ?WRONG_MERCHANT, Request, Time, Opts );
is_merchant( PublicKey, Method, Fields, _, Token, IsTest, Body, QS, Module, Function, Request, Time, Opts ) ->
  try
    is_token( jwerl:verify( Token, rs256, PublicKey ), Fields, IsTest, Method,Body, QS, Module, Function, Request, Time, Opts )
  catch
    _:_:_R ->
      % log:info("[~p] R: ~p~n", [self(), _R]), 
      log:info("[~p] INTERNAL SERVER ERROR~n", [self()]),
      response( ?SERVER_ERROR, Request, Time, Opts )
  end.
% is_merchant( _, _, _, _, _, _, _, _, _, _, Request, Time, Opts ) -> 
%   log:info("[~p] PUBLIC KEY ERROR~n", [self()]),
%   % io:format("R: ~p~n",[R]),
%   % io:format("R: dasdasdasdasd"),
%   response( ?SERVER_ERROR, Request, Time, Opts ).
% ==============================================================================================
% ===
% ==============================================================================================
is_token( [], _, _,  _, _, _, _, _, Request,Time, Opts ) ->
  response( ?INVALID_TOKEN, Request, Time, Opts );
is_token( { ok, _ }, _, true, <<"GET">>, Body, QS, Module, Function, Request, Time, Opts ) ->
  % log:info( "HTTP Request: ~p~n", [Body] ),
  response( Module:Function( Body, QS ), Request, Time, Opts );
is_token( { ok, _ }, _, true, <<"POST">>, Body, QS, Module, Function, Request, Time, Opts ) ->
  % log:info( "HTTP Request: ~p~n", [Body] ),
  response( Module:Function( Body, QS ), Request, Time, Opts );
is_token( { ok, JWTData }, Fields, _, <<"GET">>, Body, QS, Module, Function, Request, Time, Opts ) ->
  is_data_token( JWTData, QS, Fields, {Body, QS}, <<"GET">>, Module, Function, Request, Time, Opts );
is_token( { ok, JWTData }, Fields, _, <<"POST">>, Body, QS, Module, Function, Request, Time, Opts ) ->
  is_data_token(JWTData, Body, Fields, {Body, QS}, <<"POST">>, Module, Function, Request, Time, Opts).
% ==============================================================================================
% ===
% ==============================================================================================
is_data_token(JWTData2, Body2, [], {Body, QS}, Method,  Module, Function, Request, Time, Opts) ->
  JWTData1 = maps:fold(fun
    ( browser_user_agent, _, Acc ) -> 
      maps:merge( Acc, #{ 
        <<"browser_user_agent">> => maps:get( <<"browser_user_agent">>, Body2, <<>> ) 
      } ) ;
    ( K, V, Acc ) -> 
      case is_atom( K ) of 
        true -> maps:merge( Acc, #{ atom_to_binary(K,latin1) => V } ); 
        false -> maps:merge( Acc, #{ K => V } ) 
      end 
  end, #{}, JWTData2 ),
  JWTData   = maps:remove( <<"timestamp">>, JWTData1 ),
  Body1 = maps:remove( <<"timestamp">>, Body2 ),
  TS = maps:get( <<"timestamp">>, JWTData1, <<>> ),
  TokenValid = ( ( JWTData == Body1 ) and ( TS /= <<>> )  ),
  is_valid_token(TokenValid, Module, Function, Method, Body, QS, Request, Time, Opts);
is_data_token( JWTData2, Body2, Fields, {Body, QS}, Method,  Module, Function, Request, Time, Opts) ->
  % io:format("~p~n",[Body2]),
  JWTData1 = maps:fold( fun
    ( browser_user_agent, _, Acc ) -> 
      maps:merge( Acc, #{ 
        <<"browser_user_agent">> => maps:get( <<"browser_user_agent">>, Body2, <<>> ) 
      } ) ;
    ( K, V, Acc ) -> 
      case is_atom( K ) of 
        true -> maps:merge( Acc, #{ atom_to_binary( K,latin1) => V } ); 
        false -> maps:merge( Acc, #{ K => V } ) 
      end 
  end, #{}, JWTData2 ),
  TS = maps:get( <<"timestamp">>, JWTData1, <<>> ),
  JWTData   = maps:remove( <<"timestamp">>, JWTData1 ),
  JWTFields = lists:foldl( fun( Key, Acc ) ->
    maps:put( list_to_binary( Key ), maps:get( list_to_binary(Key), Body2, <<>> ), Acc )
  end, #{}, Fields ),
  % io:format("~p~n",[JWTData1]),
  % io:format("~p~n",[JWTFields]),
  TokenValid = ( ( JWTData == JWTFields ) and ( TS /= <<>> )  ),
  is_valid_token(TokenValid, Module, Function, Method, Body, QS, Request, Time, Opts).
% ==============================================================================================
% ===
% ==============================================================================================
is_valid_token( true, Module, Function, _, Body, QS, Request, Time, Opts ) ->
  response( Module:Function( Body, QS ), Request, Time, Opts );
is_valid_token( false, _, _, _, _, _, Request, Time, Opts ) ->
  response( ?INVALID_TOKEN, Request, Time, Opts ).
% ==============================================================================================
% ===
% ==============================================================================================
response( { 301, _, Headers }, Request, Time, Opts ) when is_map(Headers) ->
  log:info("[~p] erl_template response code: ~p headers: ~s~n", [self(), 301, jsx:encode(Headers)]),
  % { ok, cowboy_req:reply(301, maps:merge(Headers, ?HEADERS), <<"">>, Request), Opts };
  response_with_metric( cowboy_req:reply(301, maps:merge(Headers, ?HEADERS), <<"">>, Request), 301, Opts, Time );
% ==============================================================================================
response( { 301, _, Headers }, Request, Time, Opts ) when is_list(Headers) ->
  log:info("[~p] erl_template response code: ~p headers: ~s~n", [self(), 301, jsx:encode(Headers)]),
  % { ok, cowboy_req:reply(301, maps:merge(Headers, ?HEADERS), <<"">>, Request), Opts };
  response_with_metric( cowboy_req:reply(301, maps:merge(maps:from_list(Headers), ?HEADERS), <<"">>, Request), 301, Opts, Time );
% ==============================================================================================
response( { 301, _, Headers }, Request, Time, Opts ) ->
  log:info("[~p] erl_template response code: ~p headers: ~p~n", [self(), 301, Headers]),
  % { ok, cowboy_req:reply(301, maps:merge(Headers, ?HEADERS), <<"">>, Request), Opts };
  response_with_metric( cowboy_req:reply(301, maps:merge(Headers, ?HEADERS), <<"">>, Request), 301, Opts, Time );
% ==============================================================================================
response( { 200, <<"set_cookie">>, Cookie, Response }, Request, _, Opts ) ->
  CookieSettings = #{
    max_age => 900, 
    secure => true, 
    http_only => true
  },
  % log:info( "[~p] HTTP Response: ~p~n", [self(), Response] ),
  log:info("[~p] erl_template response code: ~p data: ~s~n", [self(), 200, jsx:encode(Response)]),
  Request1 = cowboy_req:set_resp_cookie(<<"sessionid">>, Cookie, Request, CookieSettings),
  { ok, cowboy_req:reply( 200, ?HEADERS, jsx:encode(Response), Request1 ), Opts };
% ==============================================================================================
response( { Code, Response, health_check }, Request, _, Opts ) ->
  metrics:inc(health_check),
  % log:info("[~p] erl_template response code: ~s headers: ~s~n", [self(), Code, Response]),
  { ok, cowboy_req:reply( Code, ?HEADERS, jsx:encode(Response), Request ), Opts};
  % response_with_metric( cowboy_req:reply( Code, ?HEADERS, jsx:encode(Response), Request ), Opts, Time );
% ==============================================================================================
response( { Code, Response, { metrics, Headers } }, Request, _, Opts ) ->
  log:info("[~p] erl_template metrics response code: ~p~n", [self(), Code]),
  { ok, cowboy_req:reply( Code, Headers, Response, Request ), Opts};
  % response_with_metric( cowboy_req:reply( Code, Headers, Response, Request ), Opts, Time );
% ==============================================================================================
response( { Code, Response, { custom, Headers } }, Request, _, Opts ) ->
  log:info("[~p] erl_template response code: ~p data: ~p~n", [self(), Code, Response]),
  { ok, cowboy_req:reply( Code, Headers, Response, Request ), Opts};
  % response_with_metric( cowboy_req:reply( Code, Headers, Response, Request ), Opts, Time );
% ==============================================================================================
response( { Code, Response, html }, Request, Time, Opts ) ->
  log:info("[~p] erl_template response code: ~p html: ~s~n", [self(), Code, Response]),
  % log:info( "HTTP Response: ~p~n", [Response] ),
  % { ok, cowboy_req:reply( Code, ?HEADERS_H, Response, Request ), Opts};
  response_with_metric(  cowboy_req:reply( Code, ?HEADERS_H, Response, Request ), Code, Opts, Time );
% ==============================================================================================
response( { Code, Response, Headers }, Request, Time, Opts ) ->
  log:info("[~p] erl_template response code: ~p data: ~s~n", [self(), Code, jsx:encode(Response)]),
  % log:info( "HTTP Response: ~p~n", [Response] ),
  % { ok, cowboy_req:reply( Code, maps:merge(Headers, ?HEADERS), jsx:encode(Response), Request ), Opts};
  response_with_metric( cowboy_req:reply( Code, maps:merge(Headers, ?HEADERS), jsx:encode(Response), Request ), Code, Opts, Time);
% ==============================================================================================
response( { Code, Response }, Request, Time, Opts ) ->
  log:info("[~p] erl_template response code: ~p data: ~s~n", [self(), Code, jsx:encode(Response)]),
  % log:info( "HTTP Response: ~p~n", [Response] ),
  % { ok, cowboy_req:reply( Code, ?HEADERS, jsx:encode(Response), Request ), Opts };
  response_with_metric( cowboy_req:reply( Code, ?HEADERS, jsx:encode(Response), Request ), Code, Opts, Time );
% ==============================================================================================
response( Response, Request, Time, Opts ) ->
  log:info("[~p] erl_template response code: ~p data: ~s~n", [self(), 200, jsx:encode(Response)]),
  % log:info( "HTTP Response: ~p~n", [Response] ),
  % { ok, cowboy_req:reply( 200, ?HEADERS, jsx:encode(Response), Request ), Opts }.
  response_with_metric( cowboy_req:reply( 200, ?HEADERS, jsx:encode(Response), Request ), 200, Opts, Time ).
% ==============================================================================================
% ===
% ==============================================================================================
response_with_metric( Response, Code, Opts, Time) ->
  PassedTime = erlang:system_time(nanosecond) - Time,
  metrics:histogram_inc(http_request_duration_milliseconds, [Code], (PassedTime)),
  { ok, Response, Opts}.
% ==============================================================================================
% ===
% ==============================================================================================
is_health_check_result() -> { 200, #{ <<"result">> => <<"ok">> }, health_check }.
% ==============================================================================================
% ===
% ==============================================================================================
