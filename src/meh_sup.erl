%%% @author Kristof Hetzl <kristof@Kristofs-MacBook-Pro-2.local>
%%% @copyright (C) 2016, Kristof Hetzl
%%% @doc
%%% Top level supervisor for meh.
%%% @end
%%% Created : 19 Sep 2016 by Kristof Hetzl <kristof@Kristofs-MacBook-Pro-2.local>
-module(meh_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%% API functions
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%% Supervisor callbacks
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    AChild = {meh_worker, {meh_worker, start_link, []},
              Restart, Shutdown, Type, ['AModule']},

    {ok, {SupFlags, [AChild]}}.

%%% Internal functions
