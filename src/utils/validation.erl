-module( validation ).

-include("../../src/include/api_errors.hrl").

-export([
  validate/2
]).

% ==============================================================================================
% ===
% ==============================================================================================
result({}) -> ok;
result({keys, Keys}) -> 
  {error,?KEYS_ERROR(Keys)};
result({type, Keys}) -> 
  {error,?TYPE_ERROR(Keys)};
result({req, Keys}) -> 
  {error,?REQUIRED_ERROR(Keys)};
result({reg, Keys}) -> 
  {error,?REGEXP_ERROR(Keys)}.
% ==============================================================================================
% ===
% ==============================================================================================
validate( Args, Schema ) ->
  result( validate( Schema, Args, {} ) ).
% ==============================================================================================
% ===
% ==============================================================================================
validate( [], _, Acc ) -> Acc;
validate( [Schema|Rest], Body, Acc ) ->
  Name = list_to_binary(proplists:get_value( name, Schema, [] )),
  Value = proplists:get_value( Name, Body, not_exists ),
  Required  = proplists:get_value(required, Schema, false),
  NAcc = check_key( Value, Required, Schema, Name, Acc ),
  validate( Rest, Body, NAcc ).
% ==============================================================================================
% ===
% ==============================================================================================
check_key( not_exists, false, _, _, Acc ) -> Acc;
check_key( not_exists, _, _, Name, {} ) -> 
  {keys, [Name]};
check_key( not_exists, _, _, Name, {keys,Keys} ) -> 
  {keys, Keys ++ [Name]};
check_key( not_exists, Required, Schema, Name, {_,_} ) -> 
  check_key( not_exists, Required, Schema, Name, {} );
check_key( Value, _, Schema, Name, {} ) -> 
  Type = proplists:get_value( type, Schema, [] ),
  type_of( Value, Type, Name, Schema, {} );
check_key( Value, _, Schema, Name, {EType,_} = Acc ) when EType /= keys -> 
  Type = proplists:get_value( type, Schema, [] ),
  type_of( Value, Type, Name, Schema, Acc );
check_key( _, _, _, _, Acc ) -> Acc.
% ==============================================================================================
% ===
% ==============================================================================================
type_of( Value, int, Name, Schema, Acc ) when is_integer(Value) -> 
  required(integer_to_binary(Value), Name, Schema, Acc);
type_of( Value, array, Name, Schema, Acc ) when is_list(Value) -> 
  required(Value, Name, Schema, Acc);
type_of( Value, array, Name, Schema, Acc ) when is_map(Value) -> 
  required(Value, Name, Schema, Acc);
type_of( Value, string, Name, Schema, Acc ) when is_binary(Value) ->
  required(Value, Name, Schema, Acc);
type_of( Value, boolean, Name, Schema, Acc ) when is_boolean(Value) -> 
  required(atom_to_binary(Value,latin1), Name, Schema, Acc);
type_of( Value, float, Name, Schema, Acc ) when is_float(Value) ->
  Val = io_lib:format("~.2f",[Value]),
  required( list_to_binary(Val), Name, Schema, Acc);
type_of( _, Type, Name, _, {} ) -> {type,[{Name,Type}]};
type_of( _, Type, Name, _, {type,Acc} ) -> {type,Acc ++ [{Name,Type}]};
type_of( _, _, _, _, Acc ) -> Acc.
% ==============================================================================================
% ===
% ==============================================================================================
required(Value, Name, Schema, Acc) ->
  RegExp    = proplists:get_value(regexp, Schema, <<>>),
  Required  = proplists:get_value(required, Schema, false),
  check_value(Value, RegExp, Required, Name, Acc).
% ==============================================================================================
% ===
% ==============================================================================================
check_value( <<>>, "", true, Name, {} ) -> {req, [Name]};
check_value( <<>>, "", true, Name, {req, Acc} ) -> {req, Acc ++ [Name]};
check_value( <<>>, "", true, _Name, {_, Acc} ) -> Acc;
check_value( <<>>, _, true, Name, {} ) -> {req, [Name]};
check_value( <<>>, _, true, Name, {req, Acc} ) -> {req, Acc ++ [Name]};
check_value( <<>>, _, true, _Name, {T, Acc} ) -> {T,Acc};
check_value( Value, _, true, _, Acc ) when is_list(Value) -> Acc;
check_value( Value, _, true, _, Acc ) when is_map(Value) -> Acc;
check_value( Value, RegExp, true, Name, Acc ) ->
  regexp( re:run( Value, RegExp), byte_size(Value), Name, Acc );
check_value( _, "", false, _, Acc ) -> Acc;
check_value( _, _, _, _, Acc ) -> Acc.
% ==============================================================================================
% ===
% ==============================================================================================
regexp( nomatch, _, Name, {} ) -> { reg, [Name] };
regexp( nomatch, _, Name, {reg,Acc} ) -> {reg, Acc ++ [Name] };
regexp( {match,[{0,Len}]}, Len, _Name, {} ) -> {};
regexp( {match,[{0,Len}]}, Len, _Name, Acc ) -> Acc;
regexp( {match,[{0,_Len}]}, _Len1, Name, {} ) -> { reg, [Name] };
regexp( {match,[{0,_Len}]}, _Len1, Name, {reg,Acc} ) -> {reg, Acc ++ [Name] };
regexp( {match,[{Val,_}]}, _, Name, {reg,Acc} ) when Val > 0 -> {reg, Acc ++ [Name] };
regexp( {match,[{Val,_}]}, _, Name, {} ) when Val > 0 -> {reg, [Name] };
regexp( _, _, _, Acc ) ->  Acc.
% ==============================================================================================
% ===
% ==============================================================================================
