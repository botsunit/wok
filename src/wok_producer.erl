-module(wok_producer).
-include("../include/wok.hrl").

-export([
         start/0
         , start/1
         , start/2
         , handle/3
         , handle/4
         , provide/5
         , provide/4
         , provide/2
        ]).

% Behavior

-callback messages([{Topic :: binary(), [Partition :: integer]}],
                   NumMessage :: integer()) ->
  [{MessageID :: integer(),
    Topic :: binary(),
    Partition :: integer(),
    Message :: term()}].

-callback response([OK :: integer()], [KO :: integer()]) ->
  stop | ok.

% API

% @hidden
start() ->
  wok_async_producer:start().

% @hidden
start(Topic) ->
  wok_async_producer:start(Topic).

% @hidden
start(Topic, Partition) ->
  wok_async_producer:start(Topic, Partition).

handle(From, To, Body) ->
  handle(From, To, Body, []).

handle(From, To, Body, Options) ->
  {Handler, Options1} = case doteki:get_env([wok, messages, handler], ?DEFAULT_MESSAGE_HANDLER) of
                          {Module, Options0} ->
                            {Module, buclists:merge_keylists(1, Options, Options0)};
                          Module ->
                            {Module, Options}
                        end,
  erlang:apply(Handler, create, [From, To, Body, Options1]).

provide(Topic, From, To, Body) ->
  provide(Topic, From, To, Body, []).

provide(Topic, From, To, Body, Options) ->
  Message = handle(From, To, Body, Options),
  provide(Topic, Message).

provide({Topic, Partition}, Message) when is_integer(Partition), is_binary(Message) ->
  kafe:produce([{Topic, [{Message, Partition}]}]);
provide({Topic, Key}, Message) when is_binary(Key), is_binary(Message) ->
  kafe:produce([{Topic, [{Key, Message}]}]);
provide(Topic, Message) when is_binary(Message) ->
  kafe:produce([{Topic, [Message]}]);
provide(Topic, {From, To, Body}) ->
  provide(Topic, From, To, Body);
provide(Topic, {From, To, Body, Options}) ->
  provide(Topic, From, To, Body, Options).

