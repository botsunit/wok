# Messages

## Message controlers

## Message handler

## Initializer

## Producer

### Store a message for the producer in a message controller

To store a message for a producer, you must call the function `wok_message:encode_reply/6` or `wok_message:encode_reply/5`.

Then you must store the response (`Topic`, `Partition`, `Message`) in your database.

> You can call `wok_message:encode_reply/[5,6]` as many time you needed it.

The last instruction in your controller must be a call to `wok_response:async_reply/1` :

```erlang
my_controller(Message) ->
  Body = wok_message:content(Message),
  % Do something with Body
  % ...

  ResponseMessage = ...,

  % Encode and store the message
  case wok_message:encode_reply(Message, {<<"topic">>, <<"mYk3Y">>}, <<"to/bot/controller">>, ResponseMessage) of
    {ok, Topic, Partition, MessageTransfert} ->
       store_in_database(Topic, Partition, MessageTransfert);
    {error, _Reason} ->
       lager:error("Encode response faild..."),
       init:stop()
  end,

  % Terminate
  wok_message:async_reply(Message).
```

```Elixir
def my_controller(message) do
  body = Wok.Message.content(message),
  # Do something with body
  # ...

  response_message = ...,

  # Encode and store the message
  case Wok.Message.encode_reply(message, {"topic", "mYk3Y"}, "to/bot/controller", response_message) do
    {:ok, topic, partition, message_transfert} ->
       store_in_database(topic, partition, message_transfert)
    {:error, _reason} ->
       :lager.error("Encode response faild..."),
       :init.stop()
  end,

  # Terminate
  Wok.Message.async_reply(message).
```

### Store a message for a producer outside of a message controller

If you want to provide a message outside a message controller, you must encode the message using `wok_message:encode_message/4` 

Then you must store the response (`Topic`, `Partition`, `Message`) in your database.

> You can call `wok_message:encode_message/4` as many time you need it.

## Producer configuration

To use an async producer, you must declare a producer handler module in the wok configuration :

You must declare the usage of an async producer by adding the option `consumer` in the wok configuration, with a module name as value.

```erlang
[
  {wok, [
    % ...
    {producer, [
      {handler, my_producer_handler}
    ]},
    % ...
  ]
].
```

```elixir
[
  {wok, [
    % ...
    {producer, [
      {handler, my_producer_handler}
    ]},
    % ...
  ]
].
```

The `my_producer_handler` module must respond to the `wok_producer` behavior.

```
-module(wok_producer).

-callback messages(Topic :: binary(),
                   Partition :: integer(),
                   NumMessage :: integer()) ->
  [{MessageID :: integer(),
    Topic :: binary(),
    Partition :: integer(),
    Message :: opaque_message_transfert()}].

-callback response(MessageID :: integer(), 
                   Response :: ok 
                   | {error, term()}
                   | {stop, atom(), term()}) ->
  stop | exit | next.
```

So, when the producer starts for a new round, it will first call `Handler:messages/3`. This function must return, maximally, the next `NumMessage` messages to send.

> The `MessageID` will be stored in the message heanders.

The producer will iterate on this list and after having sent each message, it will call the `Handler:response/2` function. This function is called with the message ID and a term indicating if the message has been sent or not. It must return an atom indicating that :

* the producer can send the next message (`next`),
* the producer must stop sending messages for this round (`stop`)
* the producer must stop sending messages and the producer (for this topic/partition) must terminate (`exit`).

When a consumer _exit_ you can restart it, using the `wok_producers:start/0`, `wok_producers:start/1` or `wok_producers:start/2` functions.

