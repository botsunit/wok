% @hidden
-module(wok_dispatcher).
-compile([{parse_transform, lager_transform}]).
-behaviour(gen_server).
-include("../include/wok.hrl").
-include_lib("wok_message_handler/include/wok_message_handler.hrl").

-define(SERVER, ?MODULE).

-export([start_link/0, handle/1]).
-export([finish/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

handle(Message) ->
  gen_server:cast(?SERVER, {handle, Message}).

finish(Child, Result) ->
  gen_server:cast(?SERVER, {terminate, Child, Result}).

%% ------------------------------------------------------------------

init(_Args) ->
  erlang:send_after(1000, self(), fetch),
  {ok, get_service_handlers(#{})}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({handle, {<<>>, Message}}, State) ->
  try
    case erlang:apply(wok_config:conf([wok, messages, handler], 
                                      ?DEFAULT_MESSAGE_HANDLER), 
                      parse, [Message]) of
      {ok, #message{to = To} = ParserMessage, _} ->
        _ = consume(ParserMessage, get_services(To, State), State);
      {error, Reason} ->
        lager:error("Error parsing message: ~p", [Reason]);
      _ ->
        lager:error("Wrong message parser return. See wok_message_handler for more informations")
    end
  catch
    Class:Reason1 ->
      lager:error("Parser faild: ~p:~p~n~p", [Class, Reason1, erlang:get_stacktrace()])
  end,
  {noreply, State};
handle_cast({terminate, Child, Result}, State) ->
  case Result of
    noreply -> 
      ok;
    {reply, Topic, Message} ->
      case wok_middlewares:outgoing(Message) of
        {ok, Message1} ->
          case wok:provide(Topic, Message1) of
            {ok, _} ->
              lager:debug("Message provided");
            E ->
              lager:error("Faild to provide message : ~p", [E]) % TODO: Queue message and retry later
          end;
        {stop, Middleware, Reason} ->
          lager:debug("Middleware ~p stop message ~p reason: ~p", [Middleware, Message, Reason])
      end;
    _ ->
      lager:error("Invalid response : ~p", [Result]),
      ignore
  end,
  _ = case wok_services_sup:terminate_child(Child) of
        ok -> 
          case wok_config:conf([wok, messages, local_consumer_group],
                               wok_config:conf([wok, messages, consumer_group], undefined)) of
            undefined ->
              lager:error("Missing consumer group in configuration"),
              exit(config_error);
            LocalConsumerGroup ->
              LocalQueue = bucs:to_atom(
                             wok_config:conf([wok, messages, local_queue_name], 
                                             ?DEFAULT_LOCAL_QUEUE)),
              case pipette:out(LocalQueue, #{consumer => LocalConsumerGroup}) of
                {ok, Data} ->
                  {ParsedMessage, Service, Action} = binary_to_term(Data),
                  force_consume(ParsedMessage, Service, Action);
                {error, E2} ->
                  lager:error("Pipette out error: ~p", [E2]),
                  ok
              end
          end;
        E1 ->
          lager:error("Faild to stop service #~p : ~p", [Child, E1])
      end,
  {noreply, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(fetch, State) ->
  case wok_config:conf([wok, messages, local_consumer_group],
                       wok_config:conf([wok, messages, consumer_group], undefined)) of
    undefined ->
      lager:error("Missing consumer group in configuration"),
      exit(config_error);
    LocalConsumerGroup ->
      LocalQueue = bucs:to_atom(
                     wok_config:conf([wok, messages, local_queue_name], 
                                     ?DEFAULT_LOCAL_QUEUE)),
      case pipette:ready(LocalQueue) of
        true ->
          [case pipette:out(LocalQueue, #{consumer => LocalConsumerGroup}) of
             {ok, Data} ->
               {ParsedMessage, Service, Action} = binary_to_term(Data),
               force_consume(ParsedMessage, Service, Action);
             {error, E} ->
               lager:error("Pipette out error : ~p", [E]),
               ok
           end || _ <- lists:seq(1, wok_config:conf([wok, messages, max_services_fork], ?DEFAULT_MAX_SERVICES_FORK))];
        false ->
          erlang:send_after(1000, self(), fetch)
      end
  end,
  {noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

get_service_handlers(State) ->
  State#{services => lists:foldl(fun({ServiceName, Handler}, Services) ->
                                     maps:put(ServiceName, Handler, Services)
                                 end, #{}, wok_config:conf([wok, messages, services], 
                                                           wok_config:conf([wok, messages, controlers], [])))}.

get_services(To, State) when is_binary(To) ->
  get_services([To],State);
get_services(Tos, #{services := Services}) when is_list(Tos) ->
  lists:foldl(fun(To, Acc) ->
                  lists:umerge(lists:sort(Acc),
                               lists:sort(
                                 get_services(
                                   binary:split(To, <<"/">>, [global]), 
                                   maps:keys(Services), [])))
              end, [], Tos).

get_services(_, [], Result) ->
  Result;
get_services(To, [Service|Services], Result) ->
  get_services(To, Services, service_match(To, binary:split(bucs:to_binary(Service), <<"/">>, [global]), Service, Result)).

service_match(_, [], ServiceName, Result) ->
  [ServiceName|Result];
service_match([], X, _, Result) when X =/= [] ->
  Result;
service_match([X|_], [Y|_], _, Result) when X =/= Y,
                                            X =/= <<"*">>,
                                            Y =/= <<"*">> ->
  Result;
service_match([X|To], [Y|Service], ServiceName, Result) when X == Y;
                                                             X == <<"*">>;
                                                             Y == <<"*">> ->
  service_match(To, Service, ServiceName, Result).

consume(_, [], _) ->
  ok;
consume(ParsedMessage, Services, #{services := ServicesActions}) ->
  case wok_middlewares:ingoing(ParsedMessage) of
    {ok, ParsedMessage1} ->
      case wok_config:conf([wok, messages, local_consumer_group],
                           wok_config:conf([wok, messages, consumer_group], undefined)) of
        undefined ->
          lager:info("Missing consumer group in configuration"),
          exit(config_error);
        LocalConsumerGroup ->
          LocalQueue = bucs:to_atom(
                         wok_config:conf([wok, messages, local_queue_name], 
                                         ?DEFAULT_LOCAL_QUEUE)),
          lists:foreach(fun(Service) ->
                            LocalConsumerGroupOffset = pipette:offset(LocalQueue, #{consumer => LocalConsumerGroup}),
                            case pipette:queue(LocalQueue) of
                              {LocalQueue, LocalConsumerGroupOffset, _, _, _} ->
                                case wok_services_sup:start_child({ParsedMessage1, Service, maps:get(Service, ServicesActions)}) of
                                  {ok, Child} ->
                                    gen_server:cast(Child, serve);
                                  {ok, Child, _} ->
                                    gen_server:cast(Child, serve);
                                  {queue, Data} ->
                                    queue(LocalQueue, Data);
                                  {error, Reason} ->
                                    lager:error("Faild to start service : ~p", [Reason]),
                                    error
                                end;
                              {LocalQueue, CurrentConsumerGroupOffset, _, _, _} when CurrentConsumerGroupOffset > LocalConsumerGroupOffset ->
                                queue(LocalQueue, {ParsedMessage1, Service, maps:get(Service, ServicesActions)});
                              Other ->
                                lager:error("Queue ~p error: ~p", [LocalQueue, Other])
                            end
                        end, Services)
      end;
    {stop, Middleware, Reason} ->
      lager:debug("Middleware ~p stop message ~p reason: ~p", [Middleware, ParsedMessage, Reason])
  end.

force_consume(ParsedMessage, Service, Action) ->
  lager:debug("Force consume message ~p:~p(~p)", [Service, Action, ParsedMessage]),
  case wok_services_sup:start_child({ParsedMessage, Service, Action}) of
    {ok, Child} ->
      gen_server:cast(Child, serve);
    {ok, Child, _} ->
      gen_server:cast(Child, serve);
    {queue, {ParsedMessage, Service, Action}} ->
      force_consume(ParsedMessage, Service, Action);
    {error, Reason} ->
      lager:error("Faild to start service : ~p", [Reason]),
      error
  end.

queue(LocalQueue, {ParsedMessage, _Service, _Action} = Data) ->
  case pipette:in(LocalQueue, term_to_binary(Data)) of
    {ok, LocalOffset} ->
      lager:debug("Message ~p queued with local offset ~p", [ParsedMessage, LocalOffset]),
      ok;
    {error, Reason} ->
      lager:error("Faild to queue message ~p to ~p: ", [ParsedMessage, LocalQueue, Reason]),
      error
  end.

