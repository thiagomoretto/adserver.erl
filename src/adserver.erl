%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc adserver.

-module(adserver).
-author("Mochi Media <dev@mochimedia.com>").
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the adserver server.
start() ->
    adserver_deps:ensure(),
    ensure_started(crypto),
    application:start(adserver).


%% @spec stop() -> ok
%% @doc Stop the adserver server.
stop() ->
    application:stop(adserver).
