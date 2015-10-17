% @hidden
-module(wok_services_sup).
-behaviour(supervisor).
-include("../include/wok.hrl").

-export([start_link/0, start_child/1, terminate_child/1, workers/0, available_workers/0]).
-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, temporary, 2000, Type, [I]}).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Message) ->
  case available_workers() of
    0 ->
      {queue, Message};
    N when N > 0 ->
      supervisor:start_child(?MODULE, [Message])
  end.

terminate_child(Child) ->
  case supervisor:terminate_child(?MODULE, Child) of
    ok ->
      supervisor:delete_child(?MODULE, Child);
    E ->
      E
  end.

workers() ->
  case lists:keyfind(active, 1, supervisor:count_children(?MODULE)) of
    {active, Active} ->
      Active;
    _ ->
      0
  end.

available_workers() ->
  case {workers(), wok_config:conf([wok, messages, max_services_fork], ?DEFAULT_MAX_SERVICES_FORK)} of
    {Workers, Max} when Workers < Max ->
      Max - Workers;
    _ ->
      0
  end.

init([]) ->
  {ok, { {simple_one_for_one, 0, 1}, [?CHILD(wok_service, worker)]} }.

