%%%--------------------------------------------------------------------
%%% @author
%%% @doc Throttle library based on token/leaky bucket algorithm.
%%%      It keeps a dedicated bucket per each key and calculates
%%%      the accumulated tokens by far on the fly.
%%%      Throttle interfaces:
%%%      throttle:start() -> ok.
%%%      Start throttle service.
%%%      throttle:set_bucket(Key, Capacity, Tokens, Max, Within)
%%%          -> ok.
%%%      Set new/replenish bucket with specified configuration:
%%%          @parameter Key - Key identifier of bucket,
%%%          @parameter Capacity - Bucket capacity, tokens above this would be dropped,
%%%          @parameter Tokens - Init numbers of token preset,
%%%          @parameter Max - Maximum granted tokens in within time,
%%%          @parameter Within - Within time in millisecond.
%%%
%%%      throttle:take_tokens(Key, Number) -> ok | error.
%%%      Take out numbers of tokens from bucket.
%%%      'error' returned if throttled.
%%%          @parameter Key - Key identifier of bucket,
%%%          @parameter Number - Numbers of tokens to take out.
%%%
%%%      throttle:reset_bucket(Key) -> ok.
%%%      Reset bucket to remove all existing tokens in it.
%%%          @parameter Key - Key identifier of bucket,
%%%
%%%      throttle:query_bucket(Key) -> not_found | #bucket{}.
%%%      Get bucket information in a single call.
%%%          @parameter Key - Key identifier of bucket,
%%%
%%%      Usage Example:
%%%      % Create a new bucket, named "my_bucket", with 10 tokens put in
%%%      % the bucket, the rate is 25 most per 1s (1000 millisecond).
%%%      throttle:set_bucket("my_bucket", 10, 25, 1000),
%%%      % Try to demand/take 10 tokens out of the bucket
%%%      throttle:take_tokens("my_bucket", 10),
%%%      % Reset to remove all tokens from the bucket
%%%      throttle:reset_bucket("my_bucket"),
%%%      % Query to see full recorded fields of bucket
%%%      throttle:query_bucket("my_bucket")
%%% @end
%%%--------------------------------------------------------------------
-module(throttle).

-behaviour(application).

%% API
-export([start/0, stop/0]).

-export([query_bucket/1, reset_bucket/1, set_bucket/5,
	 take_tokens/2]).

%% application callbacks
-export([start/2, stop/1]).

%% records
-record(bucket,
	{key  :: binary(), capacity  :: integer(),
	 tokens  :: integer(), max  :: integer(),
	 within  :: integer(), last_time  :: integer(),
	 total_granted  :: integer()}).

%% types
-type bucket() :: #bucket{}.

-define(TABLE, token_buckets).

%%%====================================================================
%%% API
%%%====================================================================

%% @private
-spec start() -> ok.

start() -> application:start(?MODULE).

%% @private
-spec stop() -> ok.

stop() -> application:stop(?MODULE).

-spec set_bucket(key:key(), capacity:integer(),
		 tokens:integer(), threshold:integer(),
		 duration:integer()) -> true.

set_bucket(Key, Capacity, Tokens, Max, Within) ->
    Now = erlang:system_time(millisecond),
    ets:insert(?TABLE,
	       {Key, Capacity, Tokens, Max, Within, Now, 0}).

-spec take_tokens(key:key(), number:number()) -> ok |
						 error.

take_tokens(Key, Number) ->
    Now = erlang:system_time(millisecond),
    Buckets = ets:lookup(?TABLE, Key),
    case Buckets of
      [] -> error;
      [{Key, C, T, M, W, L, TG}] ->
	  UpdatedTokens0 = T + (Now - L) * M div W,
	  % overflow if above Capacity of bucket
	  UpdatedTokens = case UpdatedTokens0 > C of
			    true -> C;
			    false -> UpdatedTokens0
			  end,
	  case Number =< UpdatedTokens of
	    true ->
		ets:update_element(?TABLE, Key,
				   [{3, UpdatedTokens - Number}, {6, Now},
				    {7, TG + Number}]),
		ok;
	    false ->
		ets:update_element(?TABLE, Key,
				   [{3, UpdatedTokens}, {6, Now}]),
		error
	  end
    end.

-spec reset_bucket(key:key()) -> ok.

reset_bucket(Key) ->
    Now = erlang:system_time(millisecond),
    ets:update_element(?TABLE, Key,
		       [{3, 0}, {6, Now}, {7, 0}]).

-spec query_bucket(key:key()) -> not_found | bucket().

query_bucket(Key) ->
    Now = erlang:system_time(millisecond),
    case ets:lookup(?TABLE, Key) of
      [] -> not_found;
      [{Key, C, T, M, W, L, TG}] ->
	  UpdatedTokens0 = T + (Now - L) * M div W,
	  % overflow if above Capacity of bucket
	  UpdatedTokens = case UpdatedTokens0 > C of
			    true -> C;
			    false -> UpdatedTokens0
			  end,
	  ets:update_element(?TABLE, Key,
			     [{3, UpdatedTokens}, {6, Now}]),
	  #bucket{key = Key, capacity = C, tokens = UpdatedTokens,
		  last_time = Now, max = M, within = W,
		  total_granted = TG}
    end.

%%%====================================================================
%%% application callbacks
%%%====================================================================

start(_Type, _StartArgs) ->
    throttle_ets_sup:start_link().

stop(_State) -> ok.
