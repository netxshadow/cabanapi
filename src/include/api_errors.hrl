% ==============================================================================================
% ===
% ==============================================================================================
-define(METHOD_NOT_ALLOWED,{200,[
  {<<"result">>,      <<"error">>},
  {<<"message">>,     <<"Method you have been reqeusted not allowed.">>},
  {<<"type">>,        <<"api_error">>},
  {<<"error_code">>,  <<"10">>},
  {<<"data">>,        [{}]}
]}).
% ==============================================================================================
% ===
% ==============================================================================================
-define(WRONG_REQUEST_DATA,{200,[
  {<<"result">>,      <<"error">>},
  {<<"message">>,     <<"Wrong Content-Type in request. Should be application/json.">>},
  {<<"type">>,        <<"api_error">>},
  {<<"error_code">>,  <<"11">>},
  {<<"data">>,        [{}]}
]}).
% ==============================================================================================
% ===
% ==============================================================================================
-define(PATH_NOT_ALLOWED,{200,[
  {<<"result">>,      <<"error">>},
  {<<"message">>,     <<"API that u have been call not exists or not allowed.">>},
  {<<"type">>,        <<"api_error">>},
  {<<"error_code">>,  <<"12">>},
  {<<"data">>,        [{}]}
]}).
% ==============================================================================================
% ===
% ==============================================================================================
-define(KEYS_ERROR(Keys), {200,[
  {<<"result">>,      <<"error">>},
  {<<"type">>,        <<"validation">>},
  {<<"error_code">>,  <<"20">>},
  {<<"message">>, list_to_binary( lists:foldl( fun
    ( Key, [] ) ->
      "Wrong request. Keys [ " ++ binary_to_list(Key);
    ( Key, Acc ) ->
      Acc ++ ", " ++ binary_to_list(Key)
  end, [], Keys ) ++ " ] are missing." ) },
  {<<"data">>,        [{<<"keys">>,Keys}]}
]}).
% ==============================================================================================
% ===
% ==============================================================================================
-define(TYPE_ERROR(Keys), {200,[
  {<<"result">>,      <<"error">>},
  {<<"error_code">>,  <<"21">>},
  {<<"type">>,        <<"validation">>},
  {<<"message">>, list_to_binary( lists:foldl( fun
    ( {Key, Type}, [] ) ->
      "Wrong request. Keys [ " ++ binary_to_list(Key) 
        ++ "(" ++ atom_to_list( Type ) ++ ")";
    ( {Key, Type}, Acc ) ->
      Acc ++ ", " ++ binary_to_list(Key)
        ++ "(" ++ atom_to_list( Type ) ++ ")"
  end, [], Keys ) ++ " ] has wrong_type." ) },
  {<<"data">>,        [{<<"keys">>,Keys}]}
]}).
% ==============================================================================================
% ===
% ==============================================================================================
-define(REQUIRED_ERROR(Keys), {200,[
  {<<"result">>,      <<"error">>},
  {<<"error_code">>,  <<"22">>},
  {<<"type">>,        <<"validation">>},
  {<<"message">>, list_to_binary( lists:foldl( fun
    ( Key, [] ) ->
      "Wrong request. Values [ " ++ binary_to_list(Key);
    ( Key, Acc ) ->
      Acc ++ ", " ++ binary_to_list(Key)
  end, [], Keys ) ++ " ] are required." )},
  {<<"data">>,        [{<<"keys">>,Keys}]}
]}).
% ==============================================================================================
% ===
% ==============================================================================================
-define(REGEXP_ERROR(Keys), {200,[
  {<<"result">>,      <<"error">>},
  {<<"error_code">>,  <<"22">>},
  {<<"type">>,        <<"validation">>},
  {<<"message">>, list_to_binary( lists:foldl( fun
    ( Key, [] ) ->
      "Wrong request. Keys [ " ++ binary_to_list(Key);
    ( Key, Acc ) ->
      Acc ++ ", " ++ binary_to_list(Key)
  end, [], Keys ) ++ " ] contains wrong values." )},
  {<<"data">>,        [{<<"keys">>,Keys}]}
]}).
% ==============================================================================================
% ===
% ==============================================================================================
-define(NO_PROCESS_FUN,{200,[
  {<<"result">>,      <<"error">>},
  {<<"message">>,     <<"Processing function doesn't exists.">>},
  {<<"type">>,        <<"processing">>},
  {<<"error_code">>,  <<"31">>},
  {<<"data">>,        [{}]}
]}).
% ==============================================================================================
% ===
% ==============================================================================================
-define(SERVER_ERROR,{200,[
  {<<"result">>,      <<"error">>},
  {<<"message">>,     <<"Something went wrong. Please try again later.">>},
  {<<"type">>,        <<"processing">>},
  {<<"error_code">>,  <<"32">>},
  {<<"data">>,        [{}]}
]}).
% ==============================================================================================
% ===
% ==============================================================================================
-define(MPI_REQ_ERROR(BinCode),{200,[
  {<<"type">>,        <<"processing">>},
  {<<"result">>,      <<"error">>},
  {<<"message">>,     <<"MPI API request error. API response code (",BinCode/binary,")">>},
  {<<"error_code">>,  <<"33">>},
  {<<"data">>,        [{}]}
]}).
% ==============================================================================================
% ===
% ==============================================================================================
-define(MPI_RESP_ERROR,{200,[
  {<<"type">>,        <<"processing">>},  
  {<<"result">>,      <<"error">>},
  {<<"message">>,     <<"MPI API response parsing error.">>},
  {<<"error_code">>,  <<"34">>},
  {<<"data">>,        [{}]}
]}).
% ==============================================================================================
% ===
% ==============================================================================================
-define(PAN_ERROR,{200,[
  {<<"type">>,        <<"validation">>},
  {<<"result">>,      <<"error">>},
  {<<"keys">>,        [ <<"card_number">> ]},
  {<<"message">>,     <<"Invalid PAN.">>},
  {<<"error_code">>,  <<"41">>}
]}).
% ==============================================================================================
% ===
% ==============================================================================================
-define(EXP_DATE_ERROR,{200,[
  {<<"type">>,        <<"validation">>},
  {<<"keys">>,        [ <<"card_expire_year">>, <<"card_expire_month">> ]},
  {<<"result">>,      <<"error">>},
  {<<"message">>,     <<"Expired card.">>},
  {<<"error_code">>,  <<"42">>}
]}).
% ==============================================================================================
% ===
% ==============================================================================================
-define( INVALID_TOKEN, { 200, #{
  <<"data">>        => #{},
  <<"type">>        => <<"processing">>,
  <<"message">>     => <<"Invalid authorization token.">>,
  <<"error_code">>  => <<"35">>,
  <<"result">>      => <<"error">>
}}).
% ==============================================================================================
% ===
% ==============================================================================================
-define( WRONG_ORDER_ID, { 200, #{
  <<"data">>        => #{},
  <<"type">>        => <<"processing">>,
  <<"message">>     => <<"Not unique transaction order_id.">>,
  <<"error_code">>  => <<"37">>,
  <<"result">>      => <<"error">>
}}).
% ==============================================================================================
% ===
% ==============================================================================================
-define( WRONG_MERCHANT, { 200, #{
  <<"data">>        => #{},
  <<"type">>        => <<"processing">>,
  <<"message">>     => <<"Invalid merchant name. Merchant does not exists.">>,
  <<"error_code">>  => <<"36">>,
  <<"result">>      => <<"error">>
}}).
% ==============================================================================================
% ===
% ==============================================================================================
-define( TID_NOT_EXIST, { 200, #{
  <<"data">>        => #{},
  <<"type">>        => <<"processing">>,
  <<"message">>     => <<"Transaction does not exists.">>,
  <<"error_code">>  => <<"39">>,
  <<"result">>      => <<"error">>
}}).
% ==============================================================================================
% ===
% ==============================================================================================
-define( BALANCE_IS_ENOUGH, { 200, #{
  <<"data">>        => #{},
  <<"type">>        => <<"processing">>,
  <<"message">>     => <<"Insufficient balance to complete the operation.">>,
  <<"error_code">>  => <<"51">>,
  <<"result">>      => <<"error">>
}}).
% ==============================================================================================
% ===
% ==============================================================================================
-define( DCC_NOT_ALLOWED, { 200, #{
  <<"data">>        => #{},
  <<"type">>        => <<"processing">>,
  <<"message">>     => <<"DCC is not allowed for this card.">>,
  <<"error_code">>  => <<"51">>,
  <<"result">>      => <<"error">>
}}).
% ==============================================================================================
% ===
% ==============================================================================================
-define( ACTION_NOT_ALLOWED, { 200, #{
  <<"data">>        => #{},
  <<"type">>        => <<"processing">>,
  <<"message">>     => <<"Action not allowed.">>,
  <<"error_code">>  => <<"30">>,
  <<"result">>      => <<"error">>
}}).
% ==============================================================================================
% ===
% ==============================================================================================
-define( REVERSED, { 200, #{
  <<"data">>        => #{},
  <<"type">>        => <<"processing">>,
  <<"message">>     => <<"Transaction reversed or amount of reversal to large.">>,
  <<"error_code">>  => <<"40">>,
  <<"result">>      => <<"error">>
}}).
% ==============================================================================================
% ===
% ==============================================================================================
-define( REFUNDED, { 200, #{
  <<"data">>        => #{},
  <<"type">>        => <<"processing">>,
  <<"message">>     => <<"Transaction refunded or amount of refund to large.">>,
  <<"error_code">>  => <<"40">>,
  <<"result">>      => <<"error">>
}}).
% ==============================================================================================
% ===
% ==============================================================================================
-define( NO_FOUND_NAME, { 200, #{
  <<"data">>        => #{},
  <<"type">>        => <<"validation">>,
  <<"message">>     => <<"There are no names to create a report.">>,
  <<"error_code">>  => <<"41">>,
  <<"result">>      => <<"error">>
}}).
% ==============================================================================================
% ===
% ==============================================================================================
-define( INCORRECT_DATE, { 200, #{
  <<"data">>        => #{},
  <<"type">>        => <<"validation">>,
  <<"message">>     => <<"Field date is invalid.">>,
  <<"error_code">>  => <<"42">>,
  <<"result">>      => <<"error">>
}}).
% ==============================================================================================
% ===
% ==============================================================================================
-define( SESSION_EXPIRED, { 200, #{
  <<"data">> => #{
    <<"type">>        => <<"processing">>,
    <<"message">>     => <<"Session expired or not exists.">>,
    <<"error_code">>  => <<"37">>
  },
  <<"result">> => <<"error">>
}}).
% ==============================================================================================
% ===
% ==============================================================================================
-define(REFERENCE_NOT_FOUND, { 200, #{
  <<"data">> => #{
    <<"type">>        => <<"processing">>,
    <<"message">>     => <<"Transaction reference is not found.">>,
    <<"error_code">>  => <<"43">>
  },
  <<"result">> => <<"error">>
}}).