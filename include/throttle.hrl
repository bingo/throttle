%%%--------------------------------------------------------------------
%%% @doc Throttle library based on token/leaky bucket algorithm.
%%%
%%%      It keeps a dedicated bucket per each key and calculates
%%%      the accumulated tokens by far on the fly.
%%%      
%%%      <b>Note:</b> Internally bucket key id stored as binary(),
%%%      so key identified by atom 'a' or "a" are the same.
%%%
%%%      <b>Usage Example:</b>
%%%
%%%      % Create a new bucket, named "my_bucket", with 10 tokens put in
%%%      % the bucket, the rate is 25 most per 1s (1000 millisecond).
%%%
%%%      `throttle:set_bucket("my_bucket", 10, 25, 1000),'
%%% 
%%%      % Try to demand/take 10 tokens out of the bucket
%%%
%%%      `throttle:take_tokens("my_bucket", 10),'
%%%
%%%      % Reset to remove all tokens from the bucket
%%%
%%%      `throttle:reset_bucket("my_bucket"),'
%%% 
%%%      % Query to see full recorded fields of bucket
%%%
%%%      `throttle:query_bucket("my_bucket")'
%%% @end
%%%--------------------------------------------------------------------

%% records
-record(bucket,
	{key  :: key(), capacity  :: non_neg_integer(),
	 tokens :: non_neg_integer(), max  :: non_neg_integer(),
	 within  :: non_neg_integer(), last_time  :: non_neg_integer(),
	 total_granted  :: non_neg_integer()}).

%% types
-type key() :: atom | string().
-type bucket() :: #bucket{}.