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
         , global_state/1
         , local_state/1
         , custom_data/1
         , custom_data/2
         , noreply/1
         , reply/4
         , reply/5
         , provide/2
         , provide/4
         , provide/5
        ]).

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
  wok_message_handler:uuid(content(Msg)).

-spec from(wok_msg:wok_msg() | wok_message_handler:message()) -> binary().
from(Msg) ->
  wok_message_handler:from(content(Msg)).

-spec to(wok_msg:wok_msg() | wok_message_handler:message()) -> binary().
to(Msg) ->
  wok_message_handler:to(content(Msg)).

-spec headers(wok_msg:wok_msg() | wok_message_handler:message()) -> binary().
headers(Msg) ->
  wok_message_handler:headers(content(Msg)).

-spec body(wok_msg:wok_msg() | wok_message_handler:message()) -> binary().
body(Msg) ->
  wok_message_handler:body(content(Msg)).

-spec global_state(wok_msg:wok_msg()) -> any().
global_state(Msg) ->
  wok_msg:get_global_state(Msg).

-spec local_state(wok_msg:wok_msg()) -> any().
local_state(Msg) ->
  wok_msg:get_local_state(Msg).

-spec custom_data(wok_msg:wok_msg()) -> any().
custom_data(Msg) ->
  wok_msg:set_custom_data(Msg).

-spec custom_data(wok_msg:wok_msg(), any()) -> wok_msg:wok_msg().
custom_data(Msg, Data) ->
  wok_msg:set_custom_data(Msg, Data).

-spec noreply(wok_msg:wok_msg()) -> wok_msg:wok_msg().
noreply(Msg) ->
  wok_msg:set_noreply(Msg).

-spec reply(wok_msg:wok_msg(), binary() | {binary(), integer()}, binary(), binary()) -> wok_msg:wok_msg().
reply(Msg, Topic, To, Body) ->
  wok_msg:set_response(Msg, Topic, undefined, To, Body).

-spec reply(wok_msg:wok_msg(), binary() | {binary(), integer()}, binary(), binary(), binary()) -> wok_msg:wok_msg().
reply(Msg, Topic, From, To, Body) ->
  wok_msg:set_response(Msg, Topic, From, To, Body).

% @doc
% Send a message
% @end
-spec provide(Topic :: binary() | list() | atom() | {binary() | list() | atom(), integer()},
              From :: binary(),
              To :: binary(),
              Body :: term()) -> {ok, term()} | {error, term()}.
provide(Topic, From, To, Body) ->
  wok_producer:provide(Topic, From, To, Body).

% @doc
% Send a message
% @end
-spec provide(Topic :: binary() | list() | atom() | {binary() | list() | atom(), integer()},
              From :: binary(),
              To :: binary(),
              Body :: term(),
              Options :: map()) -> {ok, term()} | {error, term()}.
provide(Topic, From, To, Body, Options) ->
  wok_producer:provide(Topic,  From, To, Body, Options).

% @doc
% Send a message
% @end
-spec provide(Topic :: binary() | list() | atom() | {binary() | list() | atom(), integer()},
              Message :: binary()) -> {ok, term()} | {error, term()}.
provide(Topic, Message) ->
  wok_producer:provide(Topic, Message).

