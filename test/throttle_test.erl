-module(throttle_test).

-include_lib("eunit/include/eunit.hrl").

-include("throttle.hrl").

init_test() -> ok = throttle:init().

init_twice_test() -> ok = throttle:init().

set_bucket_test() ->
    ok = throttle:set_bucket(a, 10, 1, 1, 1000).

set_bucket_negative_capacity_test() ->
    {error,
     "Capacity/Max/Within should be larger "
     "than zero."} =
	throttle:set_bucket(a, -10, 0, 1, 1000).

set_bucket_0_tokens_test() ->
    {error, "Tokens should be non-negative integer."} =
	throttle:set_bucket(a, 10, -1, 1, 1000).

take_tokens_ok_test() ->
    ok = throttle:take_tokens(a, 1).

take_tokens_negative_numbers_test() ->
    {error, "Number shoule be larger than zero."} =
	throttle:take_tokens(a, -1).

take_tokens_not_granted_test() ->
    {error, "Demands not granted."} =
	throttle:take_tokens(a, 100).

take_tokens_key_not_found_test() ->
    {error, "Key not found."} = throttle:take_tokens(b, 10).

query_bucket_test() ->
    #bucket{key = K, capacity = C, tokens = T, max = M,
	    within = W, total_granted = TG} =
	throttle:query_bucket(a),
    K = "a",
    C = 10,
    T = 0,
    M = 1,
    W = 1000,
    TG = 1.

query_bucket_key_not_found_test() ->
    {error, "Key not found."} = throttle:query_bucket(b).
