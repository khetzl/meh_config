%%% @author Kristof Hetzl <kristof@Kristofs-MacBook-Pro-2.local>
%%% @copyright (C) 2016, Kristof Hetzl
%%% @doc
%%% Application module for host specific configuration app.
%%% @end
%%% Created : 19 Sep 2016 by Kristof Hetzl <kristof@Kristofs-MacBook-Pro-2.local>
-module(meh_config_app).

-behaviour(application).

%% API
-export([reload/0, reload/1]).

%% Application callbacks
-export([start/0, start/2, stop/0, stop/1]).

%% API
reload() ->
    meh_worker:reload().

reload(Config) when is_list(Config) ->
    meh_worker:reload(Config).

%%% Application callbacks
start() ->
    application:ensure_all_started(meh_config).

start(_StartType, _StartArgs) ->
    case meh_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
                end.
stop() ->
    application:stop(meh_config).

stop(_State) ->
    ok.

%%% Internal functions
