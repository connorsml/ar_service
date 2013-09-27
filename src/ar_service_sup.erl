-module(ar_service_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ar_service_sup},
			  ar_service_sup, []).

init([]) ->
    Children = [{ar_service, {ar_service, start_link, []},
		 permanent, 5000, worker, [ar_service]}],
    {ok, {{one_for_one, 5, 10}, Children}}.


