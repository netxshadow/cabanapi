-module( db_postgres_util ).

-export([
  load_queries/1
]).

% ==============================================================================================
% ===
% ==============================================================================================
load_queries( Path ) ->
  ets:delete_all_objects(queries),
  is_directory( file:list_dir( Path ), Path ).
% ==============================================================================================
% ===
% ==============================================================================================
is_directory( {error, _}, Path ) -> 
  log:error( "Qeuries directroy [~p] doesn't exists.", [Path]),
  [];
is_directory( {ok, Files}, Path ) -> 
  lists:map(fun( File ) -> 
    is_query_file( string:tokens(File,"."), Path )
  end, Files).
% ==============================================================================================
% ===
% ==============================================================================================
is_query_file( [Name, "queries", "conf"] , Path) ->
  File = file:consult( Path ++ "/" ++ Name ++ ".queries.conf"),
  log:info("Loading query file [~p]",[Name ++ ".queries.conf"]),
  analyze_query_file( File, Name );
is_query_file( File, _ ) -> 
  log:error("File [~p]. Is not query file",[File]),
  [].
% ==============================================================================================
% ===
% ==============================================================================================
analyze_query_file( {ok,[{queries, []}]}, Name ) -> 
  log:info("Query file [~p] is empty.",[Name]),
  [];
analyze_query_file( {ok,[{queries, Queries}]}, _ ) ->
  lists:map(fun( Query ) -> 
    Result = analyze_query_schema( Query ),
    case Result of
      {ok,Data} ->
        ets:insert(queries,Data);
      error -> []
    end
  end, Queries);
analyze_query_file( Error, Name ) -> 
  log:error("Error reading file [~p] reason [~p]", [Error, Name]),
  [].
% ==============================================================================================
% ===
% ==============================================================================================
analyze_query_schema( Query ) ->
  case erlang:size( Query ) > 1 of
    true ->
      check_query( erlang:tuple_to_list( Query ) );
    false -> error
  end.
% ==============================================================================================
% ===
% ==============================================================================================
check_query( [Name, Query | Conf] ) ->
  SplitedQuery = string:split( Query, "~s", all ),
  case erlang:length( SplitedQuery ) of
    1 -> {ok, {Name, Query}};
    Len -> 
      query_args( Len, Conf, [Name, Query] )
  end.
% ==============================================================================================
% ===
% ==============================================================================================
query_args( Len, Args, [QueryName, Query] ) when length(Args) == 2 -> 
  ArgsResult = lists:foldl(fun
    (Element, ok) ->
      args_size(Len, Element, QueryName);
    (_, error) -> error
  end, ok, Args),
  [Arguments, Validation] = Args,
  case ArgsResult of
    ok -> {ok, {QueryName, {Query, Arguments, Validation}}};
    error -> error
  end;
query_args( _, _, _ ) -> error.
% ==============================================================================================
% ===
% ==============================================================================================
args_size( Len, Element, _ ) when is_list( Element ) 
  andalso erlang:length( Element ) == Len - 1 -> ok;
args_size( _, _, QueryName ) -> 
  log:error( "Query [~p] has wrong argumets/validation size.",[QueryName]),
  error.
% ==============================================================================================
% ===
% ==============================================================================================  

