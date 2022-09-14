-module(db_postgres).

-behavihour(ecpool_worker).

-export([
  init/0,
  connect/1,
  query/3
]).

%PROD SLAVE rr1d40gl9wrglhq.cinbgn1ydgk0.eu-central-1.rds.amazonaws.com - слейв

% ==============================================================================================
% ===
% ==============================================================================================
init( ) ->

  OPTS = application:get_all_env(erl_template),

  % TietoDB  = proplists:get_value( db_psql_tieto, OPTS ),
  % ecpool:start_pool( epgsql_pool_tieto, db_postgres, [
  %   {pool_size, 2},
  %   {pool_type, round_robin},
  %   {auto_reconnect, 3},
  %   {encoding,  utf8}
  % ] ++ TietoDB),

  MasterDatabaseOPTS = proplists:get_value( db_psql_master, OPTS ),

  % MasterPort     = proplists:get_value(port, MasterDatabaseOPTS), 
  % MasterHostName = proplists:get_value(hostname, MasterDatabaseOPTS), 
  % MasterDatabase = proplists:get_value(database, MasterDatabaseOPTS),
  % MasterUserName = proplists:get_value(username, MasterDatabaseOPTS), 
  % MasterPassword = proplists:get_value(password, MasterDatabaseOPTS), 

  QueriesPath = proplists:get_value(queries, MasterDatabaseOPTS),
  db_postgres_util:load_queries( QueriesPath ),

  ecpool:start_pool( epgsql_pool_master, db_postgres, [
    {pool_size, 10},
    {pool_type, round_robin},
    {auto_reconnect, 3},
    {encoding,  utf8}
  ] ++ MasterDatabaseOPTS),

  SlaveDatabaseOPTS = proplists:get_value( db_psql_slave, OPTS ),

  % SlavePort     = proplists:get_value(port, SlaveDatabaseOPTS), 
  % SlaveHostName = proplists:get_value(hostname, SlaveDatabaseOPTS), 
  % SlaveDatabase = proplists:get_value(database, SlaveDatabaseOPTS),
  % SlaveUserName = proplists:get_value(username, SlaveDatabaseOPTS), 
  % SlavePassword = proplists:get_value(password, SlaveDatabaseOPTS), 


  ecpool:start_pool( epgsql_pool_slave, db_postgres, [
    {pool_size, 10},
    {pool_type, round_robin},
    {auto_reconnect, 3},
    {encoding,  utf8}
  ] ++ SlaveDatabaseOPTS).

connect(Opts) ->
    SSL      = proplists:get_value(ssl,  Opts), 
    Host     = proplists:get_value(hostname, Opts), 
    Port     = proplists:get_value(port, Opts), 
    Username = proplists:get_value(username, Opts),
    Password = proplists:get_value(password, Opts),
    Database = proplists:get_value(database, Opts),
  
    
    % epgsql:connect( Host, Username, Password, [
    %   {port,      proplists:get_value( port, Opts )}, 
    %   {database,  proplists:get_value( database, Opts )}
    % ]).

  {ok, Pid} = epgsql:connect(#{
    host => Host,
    username => Username,
    password => Password,
    database => Database,
    port => Port,
    ssl => SSL
  }),
  log:info("Database connect: ~p~n", [Pid]),
  {ok, Pid}.

  % epgsql:connect(HostName, UserName, Password, [
  %   {port,      Port}, 
  %   {database,  Database}
  % ]).
% ==============================================================================================
% ===
% ==============================================================================================
query( PoolName, Name, [] ) ->
  log:info("DB Query 1 ~n", []),
  SearchTerm = { Name, '$1' },
  is_template( ets:match( queries, SearchTerm ), PoolName, [] );
query( PoolName, custom_query, Query ) ->
  log:info("DB Query 2 ~n", []),
  %response( epgsql:squery(db_gc, Query) );
  response( do_query(PoolName, Query) );
query(PoolName,  Name, Params ) ->
  log:info("DB Query 3 ~n", []),
  SearchTerm = { Name, {'$1', '$2', '$3'} },
  is_template( ets:match( queries, SearchTerm ), PoolName, Params ).
% ==============================================================================================
% ===
% ==============================================================================================
is_template( [], _, _ ) -> log:info("DB Query 4 ~n", []), [];
is_template( [[Query]], PoolName, [] ) ->
  log:info("DB Query 5 ~n", []),
  do_query( PoolName, Query );
is_template( [[Query, Keys, _]], PoolName, Params ) ->
  log:info("DB Query 6 ~n", []),
  build_query( PoolName, Query, Keys, Params ).
% ==============================================================================================
% ===
% ==============================================================================================
build_query( PoolName, Template, Keys, Params ) when is_list( Params ) ->
  log:info("DB Query 7 ~n Template ~p ~n Keys ~p ~n Params ~p ~n", [Template, Keys, Params]),
  Args = lists:map(fun( X ) ->
    proplists:get_value( list_to_binary( X ), Params, <<>> )
  end, Keys ),
  log:info("DB Query Args ~p ~n", [Args]),
  Query = binary_to_list( iolist_to_binary( io_lib:format( Template, Args ) ) ),
  do_query( PoolName, Query );
build_query( PoolName, Template, Keys, Params ) when is_map( Params ) ->
  log:info("DB Query 8 ~n", []),
  Args = lists:map(fun( X ) ->
    maps:get( list_to_binary( X ), Params, <<>> )
  end, Keys),
  Query = binary_to_list( iolist_to_binary( io_lib:format( Template, Args ) ) ), 
  do_query( PoolName, Query );
build_query( _, _, _, _ ) -> log:info("DB Query 9 ~n", []), [].
% ==============================================================================================
% === 
% % ==============================================================================================
% Cannot extract specs (check logs for details)
% do_query( Q ) -> [].
do_query( PoolName, Query ) ->
  log:info("DB Query 10 ~p ~n", [Query]),
  Result = ecpool:with_client( PoolName, fun(Client) -> epgsql:squery(Client, Query) end),
  % io:format("~n~p~n", [binary:replace(list_to_binary(Query), [<<"\n">>, <<"\r">>, <<"\t">>], <<" ">>, [global])]),
  % io:format("~p~n",[Result]),
  % Result = epgsql:squery(db_gc, Query),
  response( Result ).
% ==============================================================================================
% ===
% ==============================================================================================
response( { ok, _, Cols, Rows } ) -> log:info("DB Query 11 ~n", []), convert( Cols, Rows );
response( { ok, Cols, Rows } ) -> log:info("DB Query 12 ~n", []), convert( Cols, Rows );
response( { ok, _S } ) -> log:info("DB Query 14 ~n", []), convert( [], [] );
response( _ ) -> log:info("DB Query 15 ~n", []), convert( [], [] ).
% ==============================================================================================
% ===
% ==============================================================================================
convert( Columns, Rows = [[_]|_] ) ->
  log:info("DB Query 16 ~n", []),
  lists:map( fun( Row ) ->
    lists:zipwith(fun
      ( { _, Name, _, _, _, _ }, Value ) -> { Name, Value };
      ( { _, Name, _, _, _, _, _, _, _ }, Value ) -> { Name, Value }
    end, Columns, tuple_to_list( Row ) )
  end,  Rows );
convert( Columns, [Row] ) ->
  log:info("DB Query 17 ~n", []),
  lists:zipwith(fun
    ( { _, Name, _, _, _, _ }, Value ) -> { Name, Value };
    ( { _, Name, _, _, _, _, _, _, _ }, Value ) -> { Name, Value }
  end, Columns, tuple_to_list( Row ) );
convert( Columns, Rows ) ->
  log:info("DB Query 18 ~n", []),
  [ lists:zipwith(fun
    ( { _, Name, _, _, _, _ }, Value ) -> { Name, Value };
    ( { _, Name, _, _, _, _, _, _, _ }, Value ) -> { Name, Value }
  end, Columns, tuple_to_list( Row ) ) || Row <- Rows ].
% ==============================================================================================
% ===
% ==============================================================================================
