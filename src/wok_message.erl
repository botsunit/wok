-module(wok_message).
-include("../include/wok.hrl").
-include("../include/wok_message_handler.hrl").

-export([
         new/5
         , new_req/5
         , build_event_message/5
         , set_params/2
         , set_action/2
         , set_to/2
         , set_global_state/1
         , set_local_state/2
         , get_local_state/1
         , get_response/1

         , content/1
         , uuid/1
         , from/1
         , to/1
         , headers/1
         , body/1
         , body/2
         , topic/1
         , partition/1
         , params/1
         , global_state/1
         , local_state/1
         , custom_data/1
         , custom_data/2
         , custom_data/3

         , response/1
         , response_from/1
         , response_from/2
         , response_to/1
         , response_to/2
         , response_body/1
         , response_body/2

         , noreply/1
         , reply/4
         , reply/5
         , encode_reply/5
         , encode_reply/4
         , async_reply/1
         , encode_message/4

         , provide/2
         , provide/4
         , provide/5
        ]).

-export_type([message/0]).

-type message() :: term().
-type opaque() :: binary().

% @hidden
new(Topic, Partition, Offset, Key, Value) ->
  Handler = case doteki:get_env([wok, messages, handler], ?DEFAULT_MESSAGE_HANDLER) of
              {Module, _} -> Module;
              Module -> Module
            end,
  case erlang:apply(Handler, parse, [Value]) of
    {ok, Msg, _} ->
      {ok, #wok_message{
              request = Msg#msg{
                          offset = Offset,
                          key = Key,
                          message = Value,
                          topic = Topic,
                          partition = Partition}}};
    Error ->
      Error
  end.

% @hidden
% for tests
new_req(From, To, Headers, Message, UUID) ->
  #wok_message{request = #msg{
                            from = From,
                            to = To,
                            headers = Headers,
                            message = Message,
                            uuid = UUID}}.

% @hidden
set_params(#wok_message{request = Req} = Message, Params) ->
  Message#wok_message{request = Req#msg{params = Params}}.

% @hidden
build_event_message(Payload, From, MessageID, BuildBodyFun, Options) ->
  #wok_message{
     request = #msg{
                  uuid = bucs:to_binary(uuid:to_string(uuid:uuid4())),
                  from = From,
                  to = <<"to_bot">>,
                  headers = #{message_id => MessageID},
                  body = BuildBodyFun(Payload, Options),
                  message = Payload,
                  offset = buclists:keyfind(offset, 1, Options, 0),
                  topic = buclists:keyfind(topic, 1, Options, <<"topic">>),
                  partition = buclists:keyfind(partition, 1, Options, 1)}}.

% @hidden
set_action(Message, Action) ->
  Message#wok_message{action = Action}.

% @hidden
set_to(#wok_message{request = Req} = Message, Route) ->
  Message#wok_message{request = Req#msg{to = Route}}.

% @hidden
set_global_state(Message) ->
  Message#wok_message{global_state = wok_state:state()}.

% @hidden
set_local_state(Message, State) ->
  Message#wok_message{local_state = State}.

% @hidden
get_local_state(#wok_message{local_state = State}) ->
  State.

% @hidden
get_response(#wok_message{reply = false}) ->
  noreply;
get_response(#wok_message{response = #msg{topic = Topic,
                                          partition = Partition,
                                          from = From,
                                          to = To,
                                          body = Body}}) ->
  case Partition of
    undefined ->
      {reply, Topic, From, To, Body};
    _ ->
      {reply, {Topic, Partition}, From, To, Body}
  end.

% @doc
% Return the incoming message as map
% @end
-spec content(message()) -> map().
content(Message) ->
  #{uuid => uuid(Message),
    from => from(Message),
    to => to(Message),
    headers => headers(Message),
    body => body(Message),
    params => params(Message)}.

% @doc
% Return the incoming message UUID
% @end
-spec uuid(message()) -> binary() | undefined.
uuid(#wok_message{request = Message}) ->
  wok_message_handler:get_uuid(Message).

% @doc
% Return the incoming message from
% @end
-spec from(message()) -> binary() | undefined.
from(#wok_message{request = Message}) ->
  wok_message_handler:get_from(Message).

% @doc
% Return the incoming message to
% @end
-spec to(message()) -> binary() | undefined.
to(#wok_message{request = Message}) ->
  wok_message_handler:get_to(Message).

% @doc
% Return the incoming message headers
% @end
-spec headers(message()) -> binary() | undefined.
headers(#wok_message{request = Message}) ->
  wok_message_handler:get_headers(Message).

% @doc
% Return the incoming message body
% @end
-spec body(message()) -> binary() | undefined.
body(#wok_message{request = Message}) ->
  wok_message_handler:get_body(Message).

% @doc
% Update the incoming message body
% @end
-spec body(message(), term()) -> message().
body(#wok_message{request = Request} = Message, Body) ->
  Message#wok_message{request = Request#msg{body = Body}}.

% @doc
% Return the incoming message topic
% @end
-spec topic(message()) -> binary().
topic(#wok_message{request = Request}) ->
  wok_message_handler:get_topic(Request).

% @doc
% Return the incoming message partition
% @end
-spec partition(message()) -> binary().
partition(#wok_message{request = Request}) ->
  wok_message_handler:get_partition(Request).

% @doc
% Return the incoming message params
% @end
-spec params(message()) -> binary() | undefined.
params(#wok_message{request = Message}) ->
  wok_message_handler:get_params(Message).

% @doc
% Return the global state for the given message
% @end
-spec global_state(message()) -> term() | undefined.
global_state(#wok_message{global_state = GlobalState}) ->
  GlobalState.

% @doc
% Return the local state for the given message
% @end
-spec local_state(message()) -> term() | undefined.
local_state(#wok_message{local_state = LocalState}) ->
  LocalState.

% @doc
% Return the custom datas as map for the given message
% @end
-spec custom_data(message()) -> map().
custom_data(#wok_message{custom_data = CustomData}) ->
  CustomData.

% @doc
% Return the custom data value for the given message and key
% @end
-spec custom_data(message(), atom()) -> any() | undefined.
custom_data(#wok_message{custom_data = CustomData}, Key)  when is_atom(Key)->
  maps:get(Key, CustomData, undefined).

% @doc
% Add (or replace) a custom data for the given message
% @end
-spec custom_data(message(), atom(), any()) -> {ok, any(), message()} | {ok, message()}.
custom_data(#wok_message{custom_data = CustomData} = Message, Key, Value)  when is_atom(Key) ->
  case maps:get(Key, CustomData, undefined) of
    undefined ->
      {ok, Message#wok_message{
             custom_data = maps:put(Key, Value, CustomData)}};
    Old ->
      {ok, Old, Message#wok_message{
                  custom_data = maps:put(Key, Value, CustomData)}}
  end.

% @doc
% Return the outgoing message as map
% @end
-spec response(message()) -> map().
response(Message) ->
  #{from => response_from(Message),
    to => response_to(Message),
    body => response_body(Message)}.

% @doc
% Return the outgoing message from
% @end
-spec response_from(message()) -> binary().
response_from(#wok_message{response = Response}) ->
  wok_message_handler:get_from(Response).

% @doc
% Update the outgoing message from
% @end
-spec response_from(message(), binary()) -> message().
response_from(#wok_message{response = Response} = Message, From) ->
  Message#wok_message{response = Response#msg{from = From}}.

% @doc
% Return the outgoing message to
% @end
-spec response_to(message()) -> binary().
response_to(#wok_message{response = Response}) ->
  wok_message_handler:get_to(Response).

% @doc
% Update the outgoing message to
% @end
-spec response_to(message(), binary()) -> message().
response_to(#wok_message{response = Response} = Message, To) ->
  Message#wok_message{response = Response#msg{to = To}}.

% @doc
% Return the outgoing message body
% @end
-spec response_body(message()) -> binary().
response_body(#wok_message{response = Response}) ->
  wok_message_handler:get_body(Response).

% @doc
% Update the outgoing message body
% @end
-spec response_body(message(), term()) -> message().
response_body(#wok_message{response = Response} = Message, Body) ->
  Message#wok_message{response = Response#msg{body = Body}}.

% @doc
% Set a no reply response
% @end
-spec noreply(message()) -> message().
noreply(Message) ->
  Message#wok_message{reply = false}.

% @doc
% Set the response message
% @end
-spec reply(Msg :: message(),
            Topic :: binary() | {binary(), integer()} | {binary(), binary()},
            From :: binary(),
            To :: binary(),
            Body :: term()) -> message().
reply(Message, {Topic, Partition}, From, To, Body) ->
  Message#wok_message{
    reply = true,
    response = #msg{
                  from = From,
                  to = To,
                  body = Body,
                  topic = Topic,
                  partition = Partition}};
reply(Message, Topic, From, To, Body) ->
  Message#wok_message{
    reply = true,
    response = #msg{
                  from = From,
                  to = To,
                  body = Body,
                  topic = Topic,
                  partition = undefined}}.
% @doc
% Set the response message
%
% Since the sender is not specified, we will use the recipient of the incoming message
% @end
-spec reply(Msg :: message(),
            Topic :: binary() | {binary(), integer()} | {binary(), binary()},
            To :: binary(),
            Body :: term()) -> message().
reply(Message = #wok_message{request = #msg{to = From}}, Topic, To, Body) ->
  reply(Message, Topic, From, To, Body).

% @doc
% Send a message
% @end
-spec provide(Topic :: binary() | {binary(), integer()} | {binary(), binary()},
              From :: binary(),
              To :: binary(),
              Body :: term()) -> {ok, term()} | {error, term()}.
provide(Topic, From, To, Body) ->
  wok_producer:provide(Topic, From, To, Body).

% @doc
% Send a message
% @end
-spec provide(Topic :: binary() | {binary(), integer()} | {binary(), binary()},
              From :: binary(),
              To :: binary(),
              Body :: term(),
              Options :: list()) -> {ok, term()} | {error, term()}.
provide(Topic, From, To, Body, Options) ->
  wok_producer:provide(Topic,  From, To, Body, Options).

% @doc
% Send a message
% @end
-spec provide(Topic :: binary() | {binary(), integer()} | {binary(), binary()},
              Message :: binary()) -> {ok, term()} | {error, term()}.
provide(Topic, Message) ->
  wok_producer:provide(Topic, Message).

% @doc
% Encore a message for an async producer
% @end
-spec encode_reply(Message :: message(),
                   Topic :: binary()
                   | {Topic :: binary(), Partition :: integer()}
                   | {Topic :: binary(), Key :: binary()},
                   From :: binary(),
                   To :: binary(),
                   Body :: term()) ->
  {ok, Topic :: binary(), Partition :: integer(), EncodedMessage :: opaque()}
  | {stop, Middleware :: atom(), Reason :: term()}
  | {error, term()}.
encode_reply(Message, {Topic, Key}, From, To, Body) when is_binary(Key) ->
  encode_reply(Message, {Topic, kafe:default_key_to_partition(Topic, Key)}, From, To, Body);
encode_reply(Message, Topic, From, To, Body) when is_binary(Topic) ->
  encode_reply(Message, {Topic, kafe_rr:next(Topic)}, From, To, Body);
encode_reply(Message, {Topic, Partition}, From, To, Body) when is_integer(Partition) ->
  Reply = reply(Message, Topic, From, To, Body),
  case wok_middlewares:outgoing_message(Reply) of
    {ok, Reply0} when is_record(Reply0, wok_message) ->
      {ok, Topic, Partition, base64:encode(term_to_binary(Reply0))};
    {stop, Middleware, Reason} = Stop ->
      lager:debug("Middleware ~p stop message ~p reason: ~p", [Middleware, Reply, Reason]),
      Stop;
    Other ->
      lager:error("Invalid response for message ~p: ~p", [Reply, Other]),
      {error, invalid_response}
  end.

% @doc
% Encore a message for an async producer
%
% Since the sender is not specified, we will use the recipient of the incoming message
% @end
-spec encode_reply(Message :: wok_msg:wok_msg(),
                   Topic :: binary()
                   | {Topic :: binary(), Partition :: integer()}
                   | {Topic :: binary(), Key :: binary()},
                   To :: binary(),
                   Body :: term()) ->
  {ok, Topic :: binary(), Partition :: integer(), EncodedMessage :: opaque()}
  | {error, term()}.
encode_reply(Message = #wok_message{request = #msg{to = From}}, Topic, To, Body) ->
  encode_reply(Message, Topic, From, To, Body).

% @equiv async_reply(Msg, false)
async_reply(Msg) ->
  async_reply(Msg, false).

% @doc
% Set an async response
%
% If the second parameter is set to true, the async producer will send queued messages.
% @end
-spec async_reply(wok_msg:wok_msg(), true | false) -> wok_msg:wok_msg().
async_reply(Msg, false) ->
  noreply(Msg);
async_reply(Msg, true) ->
  wok_async_producer:send(),
  noreply(Msg).

% @doc
% Encode a reponse for an async producer
% @end
-spec encode_message(Topic :: binary()
                     | {Topic :: binary(), Partition :: integer()}
                     | {Topic :: binary(), Key :: binary()},
                     From :: binary(),
                     To :: binary(),
                     Body :: term()) ->
  {ok, Topic :: binary(), Partition :: integer(), EncodedMessage :: opaque()}
  | {error, term()}.
encode_message(Topic, From, To, Body) ->
  encode_message(#wok_message{}, Topic, From, To, Body).

% @hidden
encode_message(Message, {Topic, Key}, From, To, Body) when is_binary(Key) ->
  encode_message(Message, {Topic, kafe:default_key_to_partition(Topic, Key)}, From, To, Body);
encode_message(Message, Topic, From, To, Body) when is_binary(Topic) ->
  encode_message(Message, {Topic, kafe_rr:next(Topic)}, From, To, Body);
encode_message(Message, {Topic, Partition}, From, To, Body) when is_integer(Partition) ->
  {ok, Topic, Partition, base64:encode(term_to_binary(reply(Message, Topic, From, To, Body)))}.
