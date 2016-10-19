-module(wok_message_handler).
-include_lib("../include/wok_message_handler.hrl").

-export([
         get_uuid/1
         , get_to/1
         , get_from/1
         , get_headers/1
         , get_body/1
         , get_params/1
        ]).

-export_type([
              message/0
              , message_handler_option/0
             ]).

-type message() :: #msg{}.
-type message_handler_option() :: {wok_version, integer()} | {headers, map()} | {atom(), term()}.

-callback create(From :: binary(),
                 To :: binary() | [binary()],
                 Body :: term()) -> binary().

-callback create(From :: binary(),
                 To :: binary() | [binary()],
                 Body :: term(),
                 Options :: [message_handler_option()]) -> binary().

-callback parse(binary()) -> {ok,  message(), binary()} | {error, term()}.

% @hidden
get_uuid(#msg{uuid = UUID}) -> UUID.
% @hidden
get_to(#msg{to = TO}) -> TO.
% @hidden
get_from(#msg{from = From}) -> From.
% @hidden
get_headers(#msg{headers = Headers}) -> Headers.
% @hidden
get_body(#msg{body = Body}) -> Body.
% @hidden
get_params(#msg{params = Params}) -> Params.

