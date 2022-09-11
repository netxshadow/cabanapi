-module(utils).

-export([
  timezone/0
]).

% ==============================================================================================
% === utils:timezone()
% ==============================================================================================
timezone() -> 
  {X, Y, Z} = os:timestamp(),
  <<
    (integer_to_binary(X))/binary,
    (integer_to_binary(Y))/binary,
    (integer_to_binary(Z))/binary
  >>.
% ==============================================================================================
% ===
% ==============================================================================================