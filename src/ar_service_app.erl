-module(ar_service_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ar_service_sup:start_link().

stop(_State) -> ok.


