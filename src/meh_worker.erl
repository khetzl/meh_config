%%% @author Kristof Hetzl <kristof@Kristofs-MacBook-Pro-2.local>
%%% @copyright (C) 2016, Kristof Hetzl
%%% @doc
%%%
%%% @end
%%% Created : 19 Sep 2016 by Kristof Hetzl <kristof@Kristofs-MacBook-Pro-2.local>
-module(meh_worker).

-behaviour(gen_server).

%% FIXME: use proper logger.
-define(ERROR(Fmt, Args), io:format(Fmt, Args)).

%% API
-export([start_link/0, reload/0, reload/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_TIMEOUT, 5000).

-record(state, {}).

%%% API
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc
%% Reload the substitution defined in the sys.config. Meh will load the file
%% again, and read the key definitions from the file set in the app config.
%% @end
reload() ->
    Timeout = application:get_env(meh_config, call_timeout, ?DEFAULT_TIMEOUT),
    gen_server:call(?SERVER, {reload_subs, app_config}, Timeout).

%% @doc
%% Reload the config variable substitution based on a new config. Can load new files.
%% @end
reload(Config) when is_list(Config) ->
    Timeout = application:get_env(meh_config, call_timeout, ?DEFAULT_TIMEOUT),
    gen_server:call(?SERVER, {reload_subs, Config}, Timeout).

%%% gen_server callbacks
init([]) ->
    init_subs(),
    {ok, #state{}}.

handle_call({reload_subs, app_config}, _From, State) ->
    init_subs(),
    Reply = ok,
    {reply, Reply, State};
handle_call({reload_subs, Config}, _From, State) ->
    %% Reload the provided substitution Config.
    do_subs(Config),
    Reply = ok,
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions
%%% @doc Initialize substitution of config variables from files.
init_subs() ->
    case application:get_env(meh_config, subs) of
        undefined ->
            ok;
        {ok, FileProplist} ->
            do_subs(FileProplist)
    end.

%%% @doc Substitute/Set values for keys defined in external files.
do_subs(FileProplist) ->
    lists:foreach(fun read_and_set_from_file/1, FileProplist).

read_and_set_from_file({Filename, AppKeyProplist}) ->
    case file:consult(Filename) of
        {error, Reason} ->
            ?ERROR("File (~p) couldn't be read due to error:~p~n",
                   [Filename, Reason]),
            throw({file_read, Reason});
        {ok, [InputKeys]} when is_list(InputKeys) ->
            set_keys(AppKeyProplist, InputKeys);
        {ok, InputKeys} when is_list(InputKeys) ->
            set_keys(AppKeyProplist, InputKeys);
        {ok, Unexpected} ->
            ?ERROR("Unexpected content:~p~n", [Unexpected]),
            throw({unexpected_input, Unexpected})
    end.

set_keys(AppKeyProplist, FileInput) ->
    lists:foreach(fun({KeyFromFile, AppKeysToSet}) ->
                          case proplists:get_value(KeyFromFile, FileInput) of
                              undefined ->
                                  %% TODO: maybe there should be a default-version
                                  throw({undefined_key, KeyFromFile});
                               Value ->
                                  set_all_in_app_config(AppKeysToSet, Value)
                          end
                  end, AppKeyProplist).

set_all_in_app_config(AppKeysToSet, Value) ->
    lists:foreach(fun({App, Key}) ->
                          application:set_env(App, Key, Value)
                  end, AppKeysToSet).
            
