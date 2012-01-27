%% @author Mochi Media <dev@mochimedia.com>
%% @copyright adserver Mochi Media <dev@mochimedia.com>

%% @doc Callbacks for the adserver application.

-module(adserver_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for adserver.
start(_Type, _StartArgs) ->
    adserver_deps:ensure(),
    adserver_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for adserver.
stop(_State) ->
    ok.
