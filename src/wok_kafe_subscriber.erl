% @hidden
-module(wok_kafe_subscriber).
-behaviour(kafe_consumer_subscriber).
-compile([{parse_transform, lager_transform}]).
-include_lib("kafe/include/kafe_consumer.hrl").
-include("../include/wok_message_handler.hrl").
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-export([test_action/1]).
-endif.

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
consume([Current|Rest], WokMessage, ServicesDef) ->
  consume(Current, WokMessage, ServicesDef),
  consume(Rest, WokMessage, ServicesDef);
consume({Route, Params}, WokMessage, ServicesDef) ->
  case maps:get(Route, ServicesDef, undefined) of
    undefined ->
      lager:info("Ignore message ~s from ~s", [wok_message:uuid(WokMessage),
                                               wok_message:from(WokMessage)]),
      no_route;
    {Module, Function} = Action ->
      WokMessage1 = wok_message:set_params(WokMessage, Params),
      WokMessage2 = wok_message:set_action(WokMessage1, Action),
      WokMessage3 = wok_message:set_to(WokMessage2, Route),
      WokMessage4 = wok_message:set_global_state(WokMessage3),
      % TODO metrics
      case wok_middlewares:incoming_message(WokMessage4) of
        {ok, WokMessage5} ->
          Response = try
                       case erlang:apply(Module, Function, [WokMessage5]) of
                         R when is_record(R, wok_message) ->
                           R;
                         Err ->
                           lager:error("Wok stop: ~p:~p/1 invalid response: ~p", [Module, Function, Err]),
                           init:stop(1)
                       end
                     catch
                       Class:Reason ->
                         lager:error("Wok stop: ~p:~p/1 internal error!~n  => Stacktrace:~s",
                                     [Module, Function, lager:pr_stacktrace(erlang:get_stacktrace(), {Class, Reason})]),
                         init:stop(1)
                     end,
          send_response(Response);
        {stop, Middleware, Reason} = Stop ->
          lager:info("Middleware ~s stop message ~s from ~s reason: ~p",
                     [Middleware,
                      wok_message:uuid(WokMessage4),
                      wok_message:from(WokMessage4),
                      Reason]),
          Stop
      end
  end.

send_response(Response) when is_record(Response, wok_message) ->
  case wok_message:get_response(Response) of
    noreply ->
      noreply;
    {reply, _, _, _, _} ->
      case wok_middlewares:outgoing_message(Response) of
        {ok, Response1} ->
          case wok_message:get_response(Response1) of
            noreply ->
              {noreply, middlewares};
            {reply, Topic, From, To, Body} ->
              case wok_message:provide(Topic, From, To, Body) of
                {ok, _} ->
                  lager:debug("Message ~p (from ~p, to ~s) provided to ~p", [Body, From, To, Topic]),
                  {reply, Response1};
                Error ->
                  lager:error("Error when providing essage ~p (from ~p, to ~s) to ~p: ~p", [Body, From, To, Topic, Error]),
                  init:stop(1)
              end
          end;
        {stop, Middleware, Reason} = Stop ->
          lager:info("Middleware ~s stop response for message ~s from ~s reason: ~p",
                     [Middleware,
                      wok_message:uuid(Response),
                      wok_message:from(Response),
                      Reason]),
          Stop
      end
  end;
send_response(_) ->
  noreply.

-ifdef(TEST).
test_action(Message) ->
  wok_message:reply(
    Message,
    {<<"topic1">>, 1},
    <<"to">>,
    <<"message response">>).

wok_kafe_subscriber_test_() ->
  {setup,
   fun() ->
       meck:new(wok_message, [passthrough]),
       meck:expect(wok_message, set_global_state, fun(Message) ->
                                                      Message
                                                  end),
       meck:expect(wok_message, provide, 4, {ok, all_good}),
       meck:new(wok_middlewares),
       meck:expect(wok_middlewares, incoming_message, fun(Message) ->
                                                          {ok, Message}
                                                      end),
       meck:expect(wok_middlewares, outgoing_message, fun(Message) ->
                                                          {ok, Message}
                                                      end)
   end,
   fun(_) ->
       meck:unload(wok_middlewares),
       meck:unload(wok_message)
   end,
   [
     fun() ->
         WokMessage = #wok_message{
                         request = #msg{uuid = <<"UUID">>,
                                        from = <<"from">>,
                                        to = <<"dest/1">>,
                                        body = <<"message body">>,
                                        offset = 1,
                                        key = <<>>,
                                        message = <<1, 2, 3, 4, 5>>,
                                        topic = <<"topic">>,
                                        partition = 0}},
         ServicesDef = #{<<"dest/1">> => {?MODULE, test_action},
                         <<"dest/2">> => {?MODULE, test_action_undef}},
         ?assertMatch(
            {reply, #wok_message{
                       request = #msg{
                                    uuid = <<"UUID">>,
                                    from = <<"from">>,
                                    to = <<"dest/1">>,
                                    body = <<"message body">>,
                                    params = #{},
                                    offset = 1,
                                    key = <<>>,
                                    message = <<1, 2, 3, 4, 5>>,
                                    topic = <<"topic">>,
                                    partition = 0},
                       response = #msg{
                                     from = <<"dest/1">>,
                                     to = <<"to">>,
                                     body = <<"message response">>,
                                     topic = <<"topic1">>,
                                     partition = 1},
                       reply = true,
                       action = {?MODULE, test_action}}},
            consume({<<"dest/1">>, #{}}, WokMessage, ServicesDef))
     end
   ]}.

wok_kafe_subscriber_noreply_by_outgoing_middleware_test_() ->
  {setup,
   fun() ->
       meck:new(wok_message, [passthrough]),
       meck:expect(wok_message, set_global_state, fun(Message) ->
                                                      Message
                                                  end),
       meck:expect(wok_message, provide, 4, {ok, all_good}),
       meck:new(wok_middlewares),
       meck:expect(wok_middlewares, incoming_message, fun(Message) ->
                                                          {ok, Message}
                                                      end),
       meck:expect(wok_middlewares, outgoing_message, fun(Message) ->
                                                          {ok,
                                                           wok_message:noreply(Message)}
                                                      end)
   end,
   fun(_) ->
       meck:unload(wok_middlewares),
       meck:unload(wok_message)
   end,
   [
     fun() ->
         WokMessage = #wok_message{
                         request = #msg{uuid = <<"UUID">>,
                                        from = <<"from">>,
                                        to = <<"dest/1">>,
                                        body = <<"message body">>,
                                        offset = 1,
                                        key = <<>>,
                                        message = <<1, 2, 3, 4, 5>>,
                                        topic = <<"topic">>,
                                        partition = 0}},
         ServicesDef = #{<<"dest/1">> => {?MODULE, test_action},
                         <<"dest/2">> => {?MODULE, test_action_undef}},
         ?assertMatch(
            {noreply, middlewares},
            consume({<<"dest/1">>, #{}}, WokMessage, ServicesDef))
     end
   ]}.

wok_kafe_subscriber_stop_by_outgoing_middleware_test_() ->
  {setup,
   fun() ->
       meck:new(wok_message, [passthrough]),
       meck:expect(wok_message, set_global_state, fun(Message) ->
                                                      Message
                                                  end),
       meck:expect(wok_message, provide, 4, {ok, all_good}),
       meck:new(wok_middlewares),
       meck:expect(wok_middlewares, incoming_message, fun(Message) ->
                                                          {ok, Message}
                                                      end),
       meck:expect(wok_middlewares, outgoing_message, fun(_Message) ->
                                                          {stop, test_middleware, outgoing}
                                                      end)
   end,
   fun(_) ->
       meck:unload(wok_middlewares),
       meck:unload(wok_message)
   end,
   [
     fun() ->
         WokMessage = #wok_message{
                         request = #msg{uuid = <<"UUID">>,
                                        from = <<"from">>,
                                        to = <<"dest/1">>,
                                        body = <<"message body">>,
                                        offset = 1,
                                        key = <<>>,
                                        message = <<1, 2, 3, 4, 5>>,
                                        topic = <<"topic">>,
                                        partition = 0}},
         ServicesDef = #{<<"dest/1">> => {?MODULE, test_action},
                         <<"dest/2">> => {?MODULE, test_action_undef}},
         ?assertMatch(
            {stop, test_middleware, outgoing},
            consume({<<"dest/1">>, #{}}, WokMessage, ServicesDef))
     end
   ]}.

wok_kafe_subscriber_stop_by_incoming_middleware_test_() ->
  {setup,
   fun() ->
       meck:new(wok_message, [passthrough]),
       meck:expect(wok_message, set_global_state, fun(Message) ->
                                                      Message
                                                  end),
       meck:expect(wok_message, provide, 4, {ok, all_good}),
       meck:new(wok_middlewares),
       meck:expect(wok_middlewares, incoming_message, fun(_Message) ->
                                                          {stop, test_middleware, incoming}
                                                      end)
   end,
   fun(_) ->
       meck:unload(wok_middlewares),
       meck:unload(wok_message)
   end,
   [
     fun() ->
         WokMessage = #wok_message{
                         request = #msg{uuid = <<"UUID">>,
                                        from = <<"from">>,
                                        to = <<"dest/1">>,
                                        body = <<"message body">>,
                                        offset = 1,
                                        key = <<>>,
                                        message = <<1, 2, 3, 4, 5>>,
                                        topic = <<"topic">>,
                                        partition = 0}},
         ServicesDef = #{<<"dest/1">> => {?MODULE, test_action},
                         <<"dest/2">> => {?MODULE, test_action_undef}},
         ?assertMatch(
            {stop, test_middleware, incoming},
            consume({<<"dest/1">>, #{}}, WokMessage, ServicesDef))
     end
   ]}.

wok_kafe_subscriber_no_route_test_() ->
  {setup,
   fun() ->
       ok
   end,
   fun(_) ->
       ok
   end,
   [
     fun() ->
         WokMessage = #wok_message{
                         request = #msg{uuid = <<"UUID">>,
                                        from = <<"from">>,
                                        to = <<"dest/3">>,
                                        body = <<"message body">>,
                                        offset = 1,
                                        key = <<>>,
                                        message = <<1, 2, 3, 4, 5>>,
                                        topic = <<"topic">>,
                                        partition = 0}},
         ServicesDef = #{<<"dest/1">> => {?MODULE, test_action},
                         <<"dest/2">> => {?MODULE, test_action_undef}},
         ?assertMatch(
            no_route,
            consume({<<"dest/3">>, #{}}, WokMessage, ServicesDef))
     end
   ]}.
-endif.
