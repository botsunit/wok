% @hidden
-module(wok_services_sup).
-behaviour(supervisor).
-include("../include/wok.hrl").

-export([start_link/0, start_child/1, terminate_child/1, workers/0, available_workers/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(MessageTransfert) ->
  case available_workers() of
    0 ->
      {queue, MessageTransfert};
    N when N > 0 ->
      supervisor:start_child(?MODULE, [MessageTransfert])
  end.

terminate_child(Child) ->
  supervisor:terminate_child(?MODULE, Child).

workers() ->
  case lists:keyfind(active, 1, supervisor:count_children(?MODULE)) of
    {active, Active} ->
      Active;
    _ ->
      0
  end.

available_workers() ->
  case {workers(), doteki:get_env([wok, messages, max_services_fork], ?DEFAULT_MAX_SERVICES_FORK)} of
    {Workers, Max} when Workers < Max ->
      Max - Workers;
    _ ->
      0
  end.

init([]) ->
  SupFlags = #{strategy => simple_one_for_one,
               intensity => 0,
               period => 1},
  ChildSpecs = [#{id => wok_service,
                  start => {wok_service, start_link, []},
                  shutdown => 2000}],
  {ok, {SupFlags, ChildSpecs}}.

