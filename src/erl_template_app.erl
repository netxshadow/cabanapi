%%%-------------------------------------------------------------------
%% @doc epay public API
%% @end
%%%-------------------------------------------------------------------

-module(erl_template_app).

-behaviour(application).

-export([
  start/2, 
  stop/1
]).

-export([
  api_rules/1,
  get_storage_name/0
]).

% ==============================================================================================
% ===
% ==============================================================================================
start(_StartType, _StartArgs) ->

  metrics:init(),

  Config    = application:get_all_env( erl_template ), 

  APIRules  = proplists:get_value(api_rules, Config, []),

  ets:new(queries,[named_table,public]),
  ets:new(api_rules,[named_table,public]),

  db_postgres:init(), 
  db_redis:init(), 
  
  api_rules( APIRules ),

  Dispatch = cowboy_router:compile([
    {'_', [        
      {"/[...]", erl_template_api_handler, []}
    ]}
  ]), 

  TransOpts = proplists:get_value( cowboy_trans_opts, Config ),

  {ok, _} = cowboy:start_clear(http, TransOpts, #{
    stream_handlers => [cowboy_compress_h, cowboy_stream_h], 
    env => #{dispatch => Dispatch}
  }),

  R = erl_template_sup:start_link(),

  supervisor:start_child(erl_template_sup, {caban_sup, {caban_sup, start_link, []},
    permanent, 2000, supervisor, [caban_sup]}),

  caban_sup:start_caban(),

  R.

% ==============================================================================================
% ===
% ==============================================================================================
stop(_State) -> ok.
% ==============================================================================================
% ===
% ==============================================================================================
api_rules( Path ) ->
  ets:delete_all_objects(api_rules),
  is_directory( file:list_dir( Path ), Path ).
% ==============================================================================================
% ===
% ==============================================================================================
is_directory( {error, _}, Path ) -> 
  log:error( "API rules directroy [~p] doesn't exists.", [Path]),
  [];
is_directory( {ok, Files}, Path ) -> 
  lists:map(fun( File ) -> 
    is_rules_file( string:tokens(File,"."), Path )
  end, Files).
% ==============================================================================================
% ===
% ==============================================================================================
is_rules_file( [Name, "conf"] , Path) ->
  File = file:consult( Path ++ "/" ++ Name ++ ".conf"),
  log:info("Loading query file [~p]",[Name ++ ".conf"]),
  analyze_rules_file( File, Name );
  is_rules_file( File, _ ) -> 
  log:error("File [~p]. Is not API rules file",[File]),
  [].
% ==============================================================================================
% === 
% ==============================================================================================
analyze_rules_file( {ok, []}, Name ) -> 
  log:info("Query file [~p] is empty.",[Name]),
  [];
analyze_rules_file( {ok, [Rule]}, _ ) ->
  Keys = [
    { name,      is_list },
    { path,      is_list },
    { module,    is_atom },
    { function,  is_atom },
    { arguments, is_list },
    { token_fields, is_list },
    { token_validation, is_atom }
  ],
  ArgsResult = lists:foldl(fun
    (Arg, ok) ->
      analyze_rules_schema( Arg, Keys );
    (_, error) -> error
  end, ok, Rule),
  case ArgsResult of
    ok -> 
      Path = proplists:get_value(path, Rule),
      ets:insert(api_rules,{Path,Rule});
    error -> []
  end,
  log:info( "Result: ~p ", [ArgsResult]);
analyze_rules_file( Error, Name ) -> 
  log:error("Error reading file [~p] reason [~p]", [Error, Name]),
  [].
% ==============================================================================================
% ===
% ==============================================================================================
analyze_rules_schema( { Name, Value }, Keys ) -> 
  check_rule( Name, proplists:get_value( Name, Keys, '@' ), Value );
analyze_rules_schema( _, _ ) -> error.
% ==============================================================================================
% ===
% ==============================================================================================
check_rule( _, [], _ ) -> error;
check_rule( arguments, true, Values ) -> 
  Keys = [
    {name,      is_list},
    {type,      is_atom},
    {regexp,    is_list},
    {required,  is_boolean}
  ],
  lists:foldl( fun
    ( ValueArgs, ok ) -> 
      check_arg( ValueArgs, Keys );
    ( _, error) -> error
  end, ok, Values );
check_rule( _, true, _ ) -> ok;
check_rule( Name, false, Value ) -> 
  log:error( "Wrong argument type [~p] [~p]. Skip API rule.", [Name, Value]),
  error;
check_rule( Name, Fun, Value ) ->
  check_rule( Name, erlang:Fun(Value), Value ).
% ==============================================================================================
% ===
% ==============================================================================================
check_arg( [], _ ) -> error;
check_arg( true, _ ) -> ok;
check_arg( false, {Name, Value} ) -> 
  log:error( "Wrong argument type [~p] [~p]. Skip API rule.", [Name, Value]),
  error;
check_arg( Fun, {fun_name, Value, Name} ) -> 
  check_arg( erlang:Fun( Value ), {Name, Value} );
check_arg( ValueArgs, Keys ) ->
  lists:foldl( fun
    ( {Name, Value}, ok ) -> 
      check_arg( proplists:get_value(Name, Keys, []), {fun_name, Value, Name});
    ( _, error ) -> error
  end, ok, ValueArgs ).
% ==============================================================================================
% ===
% ==============================================================================================
get_storage_name() -> postgres.

% ==============================================================================================
% ===
% ==============================================================================================