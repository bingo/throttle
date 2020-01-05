%% Copyright & License.
-module(throttle).
-behaviour(gen_server).

%% API.
-export([start_link/0, stop/0]).
-export([set_bucket/4, take_tokens/2, reset_bucket/1, query_bucket/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(bucket, {
    key,
    tokens=0,
    threshold=0,
    duration,
    last_time,
    total_granted=0
}).
-record(state, {}).

-type(bucket() :: #bucket{}).

-define(SERVER, ?MODULE).
-define(TABLE, token_buckets).

%% API.

%% @private
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @private
-spec stop() -> stopped.
stop() ->
    gen_server:call(?SERVER, stop).

-spec set_bucket(key:key(), tokens:integer(), threshold:integer(), duration:integer()) -> true.
set_bucket(Key, Tokens, Threshold, Duration) ->
    Now = erlang:system_time(millisecond),
    ets:insert(?TABLE, {Key, Tokens, Threshold, Duration, Now, 0}).
-spec take_tokens(key:key(), number:number()) -> ok | error.
take_tokens(Key, Number) ->
    Now = erlang:system_time(millisecond),
    Buckets = ets:lookup(?TABLE,Key), 
    case Buckets of
        [] -> error;
        [{Key,T,Th,D,L,TG}] ->
            UpdatedTokens = T + (Now - L) * Th div D,
            case Number =< UpdatedTokens of 
                true ->
                    ets:update_element(?TABLE, Key, [{2, UpdatedTokens - Number}, {5, Now}, {6, TG + Number}]),
                    ok;
                false ->
                    ets:update_element(?TABLE, Key, [{2, UpdatedTokens}, {5, Now}]),
                    error
            end
    end.
-spec reset_bucket(key:key()) -> ok.
reset_bucket(Key) ->
    Now = erlang:system_time(millisecond),
    ets:update_element(?TABLE, Key, [{2,0}, {5,Now}, {6,0}]).
-spec query_bucket(key:key()) -> notfound | bucket().
query_bucket(Key) ->
    Now = erlang:system_time(millisecond),
    case ets:lookup(?TABLE, Key) of
        [] -> notfound;
        [{Key,T,Th,D,L,TG}] ->
            UpdatedTokens = T + (Now - L) * Th div D,
            ets:update_element(?TABLE, Key, [{2, UpdatedTokens}, {5,Now}]),
            #bucket{key=Key,tokens=UpdatedTokens,last_time=Now,threshold=Th,duration=D,total_granted=TG}
    end.
%% API functions

%% gen_server.

%% @private
init([]) ->
    ?TABLE = ets:new(?TABLE, [set, public,
        named_table, {write_concurrency, true}, {read_concurrency, true}]),
    {ok, #state{}}.

%% @private
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}. 

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
