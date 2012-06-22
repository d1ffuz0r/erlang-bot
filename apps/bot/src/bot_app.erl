-module(bot_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
start() ->
    application:start(inets),
    application:start(exmpp),
    application:start(bot).

start(_StartType, _StartArgs) ->
    bot_sup:start_link().

stop(_State) ->
    ok.
