-module(wok_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(CHILD(I, Type, Shutdown), {I, {I, start_link, []}, permanent, Shutdown, Type, [I]}).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  {ok, {
     {one_for_one, 5, 10},
     [
      ?CHILD(wok_config, worker, 5000),
      ?CHILD(wok_services_sup, supervisor, infinity),
      ?CHILD(wok_topics_sup, supervisor, infinity)
     ]
    }
  }.

