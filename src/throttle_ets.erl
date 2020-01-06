%%%--------------------------------------------------------------------
%%% @author
%%% @doc Throttle ets storage server.
%%%      Employ ETS table to store bucket data, though, when server
%%%      terminated, the ETS table is destroyed.
%%% @end
%%%--------------------------------------------------------------------
-module(throttle_ets).
-behaviour(gen_server).

%% API
-export([start_link/0, stop/0]).

%% gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(SERVER, ?MODULE).
-define(TABLE, token_buckets).

-record(state, {}).

%%%====================================================================
%%% API
%%%====================================================================

%% @private
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @private
-spec stop() -> stopped.
stop() ->
    gen_server:cast(?SERVER, stop).

%%%====================================================================
%%% gen_server callbacks
%%%====================================================================

%% @private
init([]) ->
    ?TABLE = ets:new(?TABLE, [set, public,
        named_table, {write_concurrency, true}, {read_concurrency, true}]),
    {ok, #state{}}.

%% @private
handle_call(_Request, _From, State) ->
    {reply, ignored, State}. 

%% @private
handle_cast(stop, State) ->
    {stop, normal, State};
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
