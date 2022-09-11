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
  SearchTerm = { Name, '$1' },
  is_template( ets:match( queries, SearchTerm ), PoolName, [] );
query( PoolName, custom_query, Query ) ->
  %response( epgsql:squery(db_gc, Query) );
  response( do_query(PoolName, Query) );
query(PoolName,  Name, Params ) ->
  SearchTerm = { Name, {'$1', '$2', '$3'} },
  is_template( ets:match( queries, SearchTerm ), PoolName, Params ).
% ==============================================================================================
% ===
% ==============================================================================================
is_template( [], _, _ ) -> [];
is_template( [[Query]], PoolName, [] ) ->
  do_query( PoolName, Query );
is_template( [[Query, Keys, _]], PoolName, Params ) ->
  build_query( PoolName, Query, Keys, Params ).
% ==============================================================================================
% ===
% ==============================================================================================
build_query( PoolName, Template, Keys, Params ) when is_list( Params ) ->
  Args = lists:map(fun( X ) ->
    proplists:get_value( list_to_binary( X ), Params, <<>> )
  end, Keys ),
  Query = binary_to_list( iolist_to_binary( io_lib:format( Template, Args ) ) ), 
  do_query( PoolName, Query );
build_query( PoolName, Template, Keys, Params ) when is_map( Params ) ->
  Args = lists:map(fun( X ) ->
    maps:get( list_to_binary( X ), Params, <<>> )
  end, Keys),
  Query = binary_to_list( iolist_to_binary( io_lib:format( Template, Args ) ) ), 
  do_query( PoolName, Query );
build_query( _, _, _, _ ) -> [].
% ==============================================================================================
% === 
% % ==============================================================================================
% Cannot extract specs (check logs for details)
% do_query( Q ) -> [].
do_query( PoolName, Query ) -> 
  % io:format("~p~n",[Query]),
  Result = ecpool:with_client( PoolName, fun(Client) -> epgsql:squery(Client, Query) end),
  % io:format("~n~p~n", [binary:replace(list_to_binary(Query), [<<"\n">>, <<"\r">>, <<"\t">>], <<" ">>, [global])]),
  % io:format("~p~n",[Result]),
  % Result = epgsql:squery(db_gc, Query),
  response( Result ).
% ==============================================================================================
% ===
% ==============================================================================================
response( { ok, _, Cols, Rows } ) -> convert( Cols, Rows );
response( { ok, Cols, Rows } ) -> convert( Cols, Rows );
response( { ok, _S } ) -> convert( [], [] );
response( _ ) -> convert( [], [] ).
% ==============================================================================================
% ===
% ==============================================================================================
convert( Columns, Rows = [[_]|_] ) ->
  lists:map( fun( Row ) ->
    lists:zipwith(fun
      ( { _, Name, _, _, _, _ }, Value ) -> { Name, Value };
      ( { _, Name, _, _, _, _, _, _, _ }, Value ) -> { Name, Value }
    end, Columns, tuple_to_list( Row ) )
  end,  Rows );
convert( Columns, [Row] ) ->
  lists:zipwith(fun
    ( { _, Name, _, _, _, _ }, Value ) -> { Name, Value };
    ( { _, Name, _, _, _, _, _, _, _ }, Value ) -> { Name, Value }
  end, Columns, tuple_to_list( Row ) );
convert( Columns, Rows ) -> 
  [ lists:zipwith(fun
    ( { _, Name, _, _, _, _ }, Value ) -> { Name, Value };
    ( { _, Name, _, _, _, _, _, _, _ }, Value ) -> { Name, Value }
  end, Columns, tuple_to_list( Row ) ) || Row <- Rows ].
% ==============================================================================================
% ===
% ==============================================================================================
