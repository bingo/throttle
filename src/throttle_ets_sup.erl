%%%--------------------------------------------------------------------
%%% @author
%%% @doc Throttle service supervisor.
%%% @end
%%%--------------------------------------------------------------------
-module(throttle_ets_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%====================================================================
%%% API
%%%====================================================================

%% @private
-spec start_link() -> {ok, pid()}.

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%====================================================================
%%% supervisor callbacks
%%%====================================================================

%% @private
init([]) ->
    Server = {throttle_ets, {throttle_ets, start_link, []},
	      transient, brutal_kill, worker, [throttle_ets]},
    Children = [Server],
    RestartStrategy = {one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.
