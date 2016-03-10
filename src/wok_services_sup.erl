% @hidden
-module(wok_services_sup).
-compile([{parse_transform, lager_transform}]).
-behaviour(supervisor).
-include_lib("wok_message_handler/include/wok_message_handler.hrl").
-include("../include/wok.hrl").

-export([start_link/0, start_child/1, terminate_child/1, workers/0, available_workers/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(#message_transfert{consume_method = one_for_all} = MessageTransfert) ->
  case available_workers() of
    0 ->
      {queue, MessageTransfert};
    N when N > 0 ->
      lager:info("Start service one_for_all"),
      start_child2(MessageTransfert)
  end;
start_child(#message_transfert{consume_method = one_for_one, service_name = ServiceName} = MessageTransfert) ->
  Childs = lists:usort(
             [buclists:keyfind(registered_name, 1, erlang:process_info(P), undefined) ||
              {_,P,_,_} <- supervisor:which_children(?MODULE)]),
  case lists:member(ServiceName, Childs) of
    true ->
      {queue, MessageTransfert};
    false ->
      lager:info("Start service one_for_one : ~p | ~p", [ServiceName, Childs]),
      start_child2(MessageTransfert)
  end.

start_child2(MessageTransfert) ->
  case supervisor:start_child(?MODULE, [MessageTransfert]) of
    {ok, Child} -> {ok, Child};
    {ok, Child, _} -> {ok, Child};
    {error, Reason} -> {error, Reason, MessageTransfert}
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

