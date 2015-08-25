% @hidden
-module(wok_dispatcher).
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
  gen_server:call(?SERVER, {handle, Message}).

finish(Child, Result) ->
  gen_server:cast(?SERVER, {terminate, Child, Result}).

%% ------------------------------------------------------------------

init(_Args) ->
  erlang:send_after(5, self(), fetch),
  {ok, get_service_handlers(#{})}.

handle_call({handle, {<<>>, Message}}, _From, State) ->
  case erlang:apply(wok_config:conf([wok, messages, handler], 
                                    ?DEFAULT_MESSAGE_HANDLER), 
                    parse, [Message]) of
    {ok, #message{to = To} = ParserMessage, _} ->
      _ = consume(ParserMessage, get_services(To, State)),
      {reply, ok, State};
    {error, Reason} ->
      lager:info("Error parsing message: ~p", [Reason]),
      {reply, error, State}
  end;
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({terminate, Child, Result}, State) ->
  case Result of
    noreply -> 
      ok;
    {reply, Topic, Message} ->
      wok:provide(Topic, Message);
    _ ->
      lager:error("Invalid response : ~p", [Result]),
      ignore
  end,
  _ = case wok_services_sup:terminate_child(Child) of
        ok -> 
          case wok_config:conf([wok, messages, local_consumer_group],
                               wok_config:conf([wok, messages, consumer_group], undefined)) of
            undefined ->
              lager:info("Missing consumer group in configuration"),
              exit(config_error);
            LocalConsumerGroup ->
              LocalQueue = eutils:to_atom(
                             wok_config:conf([wok, messages, local_queue_name], 
                                             ?DEFAULT_LOCAL_QUEUE)),
              case pipette:out(LocalQueue, #{consumer => LocalConsumerGroup}) of
                {ok, Data} ->
                  {ParsedMessage, Service} = binary_to_term(Data),
                  force_consume(ParsedMessage, Service);
                {error, _} ->
                  ok
              end
          end;
        _ ->
          lager:error("Faild to stop service #~p", [Child])
      end,
  {noreply, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(fetch, State) ->
  case wok_config:conf([wok, messages, local_consumer_group],
                       wok_config:conf([wok, messages, consumer_group], undefined)) of
    undefined ->
      lager:info("Missing consumer group in configuration"),
      exit(config_error);
    LocalConsumerGroup ->
      LocalQueue = eutils:to_atom(
                     wok_config:conf([wok, messages, local_queue_name], 
                                     ?DEFAULT_LOCAL_QUEUE)),
      [case pipette:out(LocalQueue, #{consumer => LocalConsumerGroup}) of
         {ok, Data} ->
           {ParsedMessage, Service} = binary_to_term(Data),
           force_consume(ParsedMessage, Service);
         {error, _} ->
           ok
       end || _ <- lists:seq(1, wok_config:conf([wok, messages, max_services_fork], ?DEFAULT_MAX_MESSAGES))]
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
                                 end, #{}, wok_config:conf([wok, messages, services], []))}.

get_services(<<"*">>, #{services := Services}) ->
  maps:values(Services);
get_services(To, #{services := Services}) ->
  case maps:get(eutils:to_binary(To), Services, maps:get(<<"*">>, Services, undefined)) of
    undefined ->
      [];
    MF ->
      [MF]
  end.

consume(ParsedMessage, Services) ->
  case wok_config:conf([wok, messages, local_consumer_group],
                        wok_config:conf([wok, messages, consumer_group], undefined)) of
    undefined ->
      lager:info("Missing consumer group in configuration"),
      exit(config_error);
    LocalConsumerGroup ->
      LocalQueue = eutils:to_atom(
                     wok_config:conf([wok, messages, local_queue_name], 
                                     ?DEFAULT_LOCAL_QUEUE)),
      lists:foreach(fun(Service) ->
                        LocalConsumerGroupOffset = pipette:offset(LocalQueue, #{consumer => LocalConsumerGroup}),
                        case pipette:queue(LocalQueue) of
                          {LocalQueue, LocalConsumerGroupOffset, _, _, _} ->
                            case wok_services_sup:start_child({ParsedMessage, Service}) of
                              {ok, Child} ->
                                gen_server:cast(Child, serve);
                              {ok, Child, _} ->
                                gen_server:cast(Child, serve);
                              {queue, Data} ->
                                queue(LocalQueue, Data);
                              {error, Reason} ->
                                lager:info("Faild to start service : ~p", [Reason]),
                                error
                            end;
                          _ ->
                            queue(LocalQueue, {ParsedMessage, Service})
                        end
                    end, Services)
  end.

force_consume(ParsedMessage, Service) ->
  case wok_services_sup:start_child({ParsedMessage, Service}) of
    {ok, Child} ->
      gen_server:cast(Child, serve);
    {ok, Child, _} ->
      gen_server:cast(Child, serve);
    {queue, {ParsedMessage, Service}} ->
      force_consume(ParsedMessage, Service);
    {error, Reason} ->
      lager:info("Faild to start service : ~p", [Reason]),
      error
  end.

queue(LocalQueue, {ParsedMessage, _Service} = Data) ->
  case pipette:in(LocalQueue, term_to_binary(Data)) of
    {ok, LocalOffset} ->
      lager:debug("Message ~p queued with local offset ~p", [ParsedMessage, LocalOffset]),
      ok;
    {error, Reason} ->
      lager:info("Faild to queue message ~p to ~p: ", [ParsedMessage, LocalQueue, Reason]),
      error
  end.

