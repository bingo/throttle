%%%--------------------------------------------------------------------
%%% @author bingo.zhao@gmail.com
%%% @headerfile "throttle.hrl"
%%% @end
%%%--------------------------------------------------------------------
-module(throttle).

-include("throttle.hrl").

%% API
-export([init/0, query_bucket/1, reset_bucket/1,
	 set_bucket/5, take_tokens/2]).

%%%====================================================================
%%% API
%%%====================================================================

%% functions spec.
-spec init() -> ok.

%% @doc Initialize throttle service.
init() ->
    case application:start(?MODULE) of
      ok ->
	  {ok, Store} = application:get_env(?MODULE, store),
	  Store:init();
      {error, {already_started, ?MODULE}} -> ok
    end.

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
set_bucket(Key, Capacity, Tokens, Max, Within) ->
    {ok, Store} = application:get_env(?MODULE, store),
    Store:set_bucket(Key, Capacity, Tokens, Max, Within).

-spec take_tokens(Key :: key(),
		  Number :: non_neg_integer()) -> ok | {error, term()}.

%% @doc Take out numbers of tokens from bucket, 'error' returned if throttled.
%% @param Key - Key identifier of bucket,
%% @param Number - Numbers of tokens to take out.
take_tokens(Key, Number) ->
    {ok, Store} = application:get_env(?MODULE, store),
    Store:take_tokens(Key, Number).

-spec reset_bucket(Key :: key()) -> ok.

%% @doc Reset bucket to remove all existing tokens in it.
%% @param Key - Key identifier of bucket.
reset_bucket(Key) ->
    {ok, Store} = application:get_env(?MODULE, store),
    Store:reset_bucket(Key).

-spec query_bucket(Key :: key()) -> not_found |
				    bucket().

%% @doc Get bucket information in a single call.
%% @param Key - Key identifier of bucket.
query_bucket(Key) ->
    {ok, Store} = application:get_env(?MODULE, store),
    Store:query_bucket(Key).
