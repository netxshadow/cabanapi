-define( REVERSAL_RESPONSE ( Result, Type, Message, ErrorCode, Data ),
{200, #{
  <<"data">>        => maps:merge(Data, #{
    <<"type">>        => Type,
    <<"message">>     => Message,
    <<"error_code">>  => ErrorCode
  }),
  <<"result">>      => Result 
}}).
% ==============================================================================================
% ===
% ==============================================================================================
-define( CHECKOUT_STEP_RESPONSE ( Result, Page, Type, Description, Payment ),
{200, #{
  <<"data">> => #{
    <<"page">> => Page,
    <<"page_data">> => #{
      <<"type">>        => Type,
      <<"payment">>     => Payment,
      <<"description">> => Description
    }
  },
  <<"result">>  => Result
}}).
% ==============================================================================================
% ===
% ==============================================================================================
-define( MERCHANT_REGISTER ( Result, MerchantData, Description ),
{200, #{
  <<"data">> => #{
    <<"type">>          => <<"merchant_register">>,
    <<"description">>   => Description,
    <<"merchant_data">> => MerchantData
  },
  <<"result">>  => Result
}}).
% ==============================================================================================
% ===
% ==============================================================================================
-define( LOOKUP_RESPONSE ( Result, OTP, Code, Description, OrderID ),
{200, #{
  <<"data">> => #{
    <<"otp">>               => OTP,
    <<"type">>              => <<"lookup">>,
    <<"order_id">>          => OrderID,
    <<"error_code">>        => Code,
    <<"error_desciption">>  => Description
  },
  <<"result">>  => Result
}}).
% ==============================================================================================
% ===
% ==============================================================================================
-define( CARD_STATUS_RESPONSE ( Result, Code, Description, OrderID ),
{200, #{
  <<"data">> => #{
    <<"type">>              => <<"card_status">>,
    <<"order_id">>          => OrderID,
    <<"error_code">>        => Code,
    <<"error_desciption">>  => Description
  },
  <<"result">>  => Result
}}).
% ==============================================================================================
% ===
% ==============================================================================================
-define( PAYMENT_RESPONSE ( Result, Code, Description, Status, OrderID ),
{200, #{
  <<"data">> => #{
    <<"type">>              => <<"payment">>,
    <<"order_id">>          => OrderID,
    <<"error_code">>        => Code,
    <<"payment_status">>    => Status,
    <<"error_desciption">>  => Description
  },
  <<"result">>  => Result
}}).
% ==============================================================================================
% ===
% ==============================================================================================
