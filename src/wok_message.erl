-module(wok_message).

-export([
         content/1
         , content/2
         , content_has_map/1
         , uuid/1
         , from/1
         , to/1
         , headers/1
         , body/1
         , params/1
         , param/2
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

-type opaque_message_transfert() :: binary().

-spec content(wok_msg:wok_msg()) -> wok_message_handler:message().
content(Msg) ->
  wok_msg:get_message(Msg).

-spec content_has_map(wok_msg:wok_msg()) -> map().
content_has_map(Msg) ->
  wok_msg:get_message_has_map(Msg).

-spec content(wok_msg:wok_msg(), wok_message_handler:message()) -> wok_msg:wok_msg().
content(Msg, Message) ->
  wok_msg:set_message(Msg, Message).

-spec uuid(wok_msg:wok_msg() | wok_message_handler:message()) -> binary().
uuid(Msg) ->
  wok_msg:get_uuid(content(Msg)).

-spec from(wok_msg:wok_msg() | wok_message_handler:message()) -> binary().
from(Msg) ->
  wok_msg:get_from(content(Msg)).

-spec to(wok_msg:wok_msg() | wok_message_handler:message()) -> binary().
to(Msg) ->
  wok_msg:get_to(content(Msg)).

-spec headers(wok_msg:wok_msg() | wok_message_handler:message()) -> binary().
headers(Msg) ->
  wok_msg:get_headers(content(Msg)).

-spec body(wok_msg:wok_msg() | wok_message_handler:message()) -> binary().
body(Msg) ->
  wok_msg:get_body(content(Msg)).

-spec params(wok_msg:wok_msg() | wok_message_handler:message()) -> map().
params(Msg) ->
  wok_msg:get_params(content(Msg)).

-spec param(wok_msg:wok_msg() | wok_message_handler:message(), binary() | list() | atom()) -> binary() | undefined.
param(Msg, Param) ->
  wok_msg:get_param(content(Msg), Param).

-spec global_state(wok_msg:wok_msg()) -> any().
global_state(Msg) ->
  wok_msg:get_global_state(Msg).

-spec local_state(wok_msg:wok_msg()) -> any().
local_state(Msg) ->
  wok_msg:get_local_state(Msg).

-spec custom_data(wok_msg:wok_msg()) -> map().
custom_data(Msg) ->
  wok_msg:get_custom_data(Msg).

-spec custom_data(wok_msg:wok_msg(), atom()) -> any().
custom_data(Msg, Key) when is_atom(Key) ->
  #{Key := Data} = wok_msg:get_custom_data(Msg),
  Data.

-spec custom_data(wok_msg:wok_msg(), atom(), any()) -> {ok, wok_msg:wok_msg(), any()}
                                                       | {ok, wok_msg:wok_msg()}.
custom_data(Msg, Key, Value) when is_atom(Key) ->
  case wok_msg:get_custom_data(Msg) of
    #{Key := Old} = CustomData ->
      {ok, Old, wok_msg:set_custom_data(Msg, maps:put(Key, Value, CustomData))};
    CustomData ->
      {ok, wok_msg:set_custom_data(Msg, maps:put(Key, Value, CustomData))}
  end.

-spec response(wok_msg:wok_msg()) -> {Topic :: term(),
                                      From :: binary(),
                                      To :: binary(),
                                      Body :: term()}.
response(Msg) ->
  wok_msg:get_response(Msg).

-spec response_from(wok_msg:wok_msg()) -> binary().
response_from(Msg) ->
  {_, From, _, _} = response(Msg),
  From.

-spec response_to(wok_msg:wok_msg()) -> binary().
response_to(Msg) ->
  {_, _, To, _} = response(Msg),
  To.

-spec response_body(wok_msg:wok_msg()) -> binary().
response_body(Msg) ->
  {_, _, _, Body} = response(Msg),
  Body.

-spec response_from(wok_msg:wok_msg(), binary()) -> wok_msg:wok_msg().
response_from(Msg, From) ->
  {Topic, _, To, Body} = response(Msg),
  wok_msg:set_response(Msg, Topic, From, To, Body).

-spec response_to(wok_msg:wok_msg(), binary()) -> wok_msg:wok_msg().
response_to(Msg, To) ->
  {Topic, From, _, Body} = response(Msg),
  wok_msg:set_response(Msg, Topic, From, To, Body).

-spec response_body(wok_msg:wok_msg(), term()) -> wok_msg:wok_msg().
response_body(Msg, Body) ->
  {Topic, From, To, _} = response(Msg),
  wok_msg:set_response(Msg, Topic, From, To, Body).

-spec noreply(wok_msg:wok_msg()) -> wok_msg:wok_msg().
noreply(Msg) ->
  wok_msg:set_noreply(Msg).

-spec reply(Msg :: wok_msg:wok_msg(),
            Topic :: binary() | {binary(), integer()} | {binary(), binary()},
            To :: binary(),
            Body :: term()) -> wok_msg:wok_msg().
reply(Msg, Topic, To, Body) ->
  reply(Msg, Topic, undefined, To, Body).

-spec reply(Msg :: wok_msg:wok_msg(),
            Topic :: binary() | {binary(), integer()} | {binary(), binary()},
            From :: binary(),
            To :: binary(),
            Body :: term()) -> wok_msg:wok_msg().
reply(Msg, Topic, From, To, Body) ->
  wok_msg:set_response(Msg, Topic, From, To, Body).

-spec encode_reply(Msg :: wok_msg:wok_msg(),
                   Topic :: binary()
                   | {Topic :: binary(), Partition :: integer()}
                   | {Topic :: binary(), Key :: binary()},
                   From :: binary(),
                   To :: binary(),
                   Body :: term()) -> {ok, binary(), integer(), opaque_message_transfert()}
                                      | {error, term()}.
encode_reply(Msg, {Topic, Key}, From, To, Body) when is_binary(Key) ->
  encode_reply(Msg, {Topic, kafe:default_key_to_partition(Topic, Key)}, From, To, Body);
encode_reply(Msg, Topic, From, To, Body) when is_binary(Topic) ->
  encode_reply(Msg, {Topic, kafe_rr:next(Topic)}, From, To, Body);
encode_reply(Msg, {Topic, Partition}, From, To, Body) when is_integer(Partition) ->
  {ok, Topic, Partition, base64:encode(term_to_binary(reply(Msg, Topic, From, To, Body)))}.

-spec encode_reply(Msg :: wok_msg:wok_msg(),
                   Topic :: binary()
                   | {Topic :: binary(), Partition :: integer()}
                   | {Topic :: binary(), Key :: binary()},
                   To :: binary(),
                   Body :: term()) -> {ok, binary(), integer(), opaque_message_transfert()}
                                      | {error, term()}.
encode_reply(Msg, Topic, To, Body) ->
  encode_reply(Msg, Topic, undefined, To, Body).

-spec async_reply(wok_msg:wok_msg()) -> wok_msg:wok_msg().
async_reply(Msg) ->
  wok_msg:set_noreply(Msg).

-spec encode_message(Topic :: binary()
                     | {Topic :: binary(), Partition :: integer()}
                     | {Topic :: binary(), Key :: binary()},
                     From :: binary(),
                     To :: binary(),
                     Body :: term()) -> {ok, binary(), integer(), opaque_message_transfert()}
                                        | {error, term()}.
encode_message(Topic, From, To, Body) ->
  encode_reply(wok_msg:new(), Topic, From, To, Body).

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

