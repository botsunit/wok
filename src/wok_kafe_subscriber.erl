% @hidden
-module(wok_kafe_subscriber).
-behaviour(kafe_consumer_subscriber).
-compile([{parse_transform, lager_transform}]).
-include_lib("kafe/include/kafe_consumer.hrl").
-include_lib("../include/wok_message_handler.hrl").

-export([
         init/4
         , handle_message/2
        ]).

-record(state, {group_id,
                topic,
                partition,
                services}).

init(GroupID, Topic, Partition, _Args) ->
  {ok, #state{group_id = GroupID,
              topic = Topic,
              partition = Partition,
              services = wok_message_path:get_message_path_handlers(
                           doteki:get_env([wok, messages, services],
                                          doteki:get_env([wok, messages, controllers],
                                                         doteki:get_env([wok, messages, controlers], []))))}}.

handle_message(#message{topic = Topic,
                        partition = Partition,
                        offset = Offset,
                        key = Key,
                        value = Value} = _Message, #state{services = ServicesDef} = State) ->
  case wok_message:new(Topic, Partition, Offset, Key, Value) of
    {ok, WokMessage} ->
      Services =  wok_message_path:get_message_handlers(
                    wok_message:to(WokMessage),
                    ServicesDef),
      consume(Services, WokMessage, ServicesDef);
    {error, Error} ->
      lager:info("Can't parse message at offset ~p from topic ~s, partition ~p: ~p",
                 [Offset, Topic, Partition, Error])
  end,
  {ok, State}.

consume([], _, _) ->
  ok;
consume([{Route, Params}|Rest], WokMessage, ServicesDef) ->
  case maps:get(Route, ServicesDef, undefined) of
    undefined ->
      lager:info("Ignore message ~s from ~s", [wok_message:uuid(WokMessage),
                                               wok_message:from(WokMessage)]);
    {Module, Function} = Action ->
      WokMessage1 = wok_message:set_params(WokMessage, Params),
      WokMessage2 = wok_message:set_action(WokMessage1, Action),
      WokMessage3 = wok_message:set_to(WokMessage2, Route),
      WokMessage4 = wok_message:set_global_state(WokMessage3),
      % TODO metrics
      % TODO middlewares
      case wok_middlewares:incoming_message(WokMessage4) of
        {ok, WokMessage5} ->
          Response = erlang:apply(Module, Function, [WokMessage5]),
          case wok_message:get_response(Response) of
            noreply ->
              ok;
            {reply, _, _, _, _} ->
              case wok_middlewares:outgoing_message(Response) of
                {ok, Response1} ->
                  case wok_message:get_response(Response1) of
                    noreply ->
                      ok;
                    {reply, Topic, From, To, Body} ->
                      wok_message:provide(Topic, From, To, Body)
                  end;
                {stop, Middleware, Reason} ->
                  lager:info("Middleware ~s stop response for message ~s from ~s reason: ~p",
                             [Middleware,
                              wok_message:uuid(WokMessage5),
                              wok_message:from(WokMessage5),
                              Reason])
              end
          end;
        {stop, Middleware, Reason} ->
          lager:info("Middleware ~s stop message ~s from ~s reason: ~p",
                     [Middleware,
                      wok_message:uuid(WokMessage4),
                      wok_message:from(WokMessage4),
                      Reason])
      end
  end,
  consume(Rest, WokMessage, ServicesDef).

