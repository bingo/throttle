%%%--------------------------------------------------------------------
%%% @author bingo.zhao@gmail.com
%%% @doc Throttle ets storage server.
%%%
%%%      Employ ETS table to store bucket data.
%%% @end
%%%--------------------------------------------------------------------
-module(throttle_ets).

-include("throttle.hrl").

%% API
-export([init/0, query_bucket/1, reset_bucket/1,
	 set_bucket/5, take_tokens/2]).

-define(TABLE, token_buckets).

%%%====================================================================
%%% API
%%%====================================================================
-spec init() -> ok.

%% @doc Initialize throttle service.
init() ->
    ets:new(?TABLE,
	    [set, public, named_table, {write_concurrency, true},
	     {read_concurrency, true}]),
    ok.

-spec set_bucket(Key :: key(),
		 Capacity :: non_neg_integer(),
		 Tokens :: non_neg_integer(), Max :: non_neg_integer(),
		 Within :: non_neg_integer()) -> ok | {error, term()}.

%% @doc Set new/replenish bucket with specified configuration.
%% @param Key - Key identifier of bucket,
%% @param Capacity - Bucket capacity, tokens above this would be dropped,
%% @param Tokens - Init numbers of token preset,
%% @param Max - Maximum granted tokens in within time,
%% @param Within - Within time in millisecond.
set_bucket(Key, Capacity, Tokens, Max, Within)
    when is_atom(Key) ->
    set_bucket(atom_to_list(Key), Capacity, Tokens, Max,
	       Within);
set_bucket(_, _, Tokens, _, _) when Tokens < 0 ->
    {error, "Tokens should be non-negative integer."};
set_bucket(_, Capacity, _, Max, Within)
    when Capacity =< 0 orelse Max =< 0 orelse Within =< 0 ->
    {error,
     "Capacity/Max/Within should be larger "
     "than zero."};
set_bucket(Key, Capacity, Tokens, Max, Within) ->
    Now = erlang:system_time(millisecond),
    Tokens0 = case Capacity < Tokens of
		true -> Capacity;
		false -> Tokens
	      end,
    ets:insert(?TABLE,
	       {Key, Capacity, Tokens0, Max, Within, Now, 0}),
    ok.

    %make sure tokens < capacity, otherwise only capacity tokens set

-spec take_tokens(Key :: key(),
		  Number :: non_neg_integer()) -> ok | {error, term()}.

%% @doc Take out numbers of tokens from bucket, 'error' returned if throttled.
%% @param Key - Key identifier of bucket,
%% @param Number - Numbers of tokens to take out.
take_tokens(Key, Number) when is_atom(Key) ->
    take_tokens(atom_to_list(Key), Number);
take_tokens(_Key, Number) when Number < 0 ->
    {error, "Number shoule be larger than zero."};
take_tokens(Key, Number) ->
    Now = erlang:system_time(millisecond),
    Buckets = ets:lookup(?TABLE, Key),
    case Buckets of
      [] -> {error, "Key not found."};
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
		{error, "Demands not granted."}
	  end
    end.

-spec reset_bucket(Key :: key()) -> ok.

%% @doc Reset bucket to remove all existing tokens in it.
%% @param Key - Key identifier of bucket.
reset_bucket(Key) when is_atom(Key) ->
    reset_bucket(atom_to_list(Key));
reset_bucket(Key) ->
    Now = erlang:system_time(millisecond),
    ets:update_element(?TABLE, Key,
		       [{3, 0}, {6, Now}, {7, 0}]).

-spec query_bucket(Key :: key()) -> {error, term()} |
				    bucket().

%% @doc Get bucket information in a single call.
%% @param Key - Key identifier of bucket.
query_bucket(Key) when is_atom(Key) ->
    query_bucket(atom_to_list(Key));
query_bucket(Key) ->
    Now = erlang:system_time(millisecond),
    case ets:lookup(?TABLE, Key) of
      [] -> {error, "Key not found."};
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
