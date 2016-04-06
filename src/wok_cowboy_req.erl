% @hidden
-module(wok_cowboy_req).
-behaviour(wok_req).

-export([
  reply/1
  , set_cookie/4
  , get_cookies/1
  , client_ip/1
  , client_port/1
  , body/1
  , has_body/1
  , body_length/1
  , method/1
  , path/1
  , header/3
  , headers/1
  , post_values/1
  , get_values/1
  , binding_values/1
  , get_file/1
  , get_file/2
  , get_file/3
]).

-spec reply(wok_req:wok_req()) -> term().
reply(Req) ->
  cowboy_req:reply(wok_req:get_response_code(Req),
                   wok_req:get_response_headers(Req),
                   wok_req:get_response_body(Req),
                   wok_req:get_http_req(Req)).

-spec set_cookie(wok_req:wok_req(),
                 iodata(),
                 iodata(),
                 [{max_age, non_neg_integer()}
                  | {domain, binary()}
                  | {path, binary()}
                  | {secure, boolean()}
                  | {http_only, boolean()}]) -> wok_req:wok_req().
set_cookie(Req, Name, Value, Options) ->
  wok_req:set_http_req(Req,
                       cowboy_req:set_resp_cookie(Name, Value, Options,
                                                  wok_req:get_http_req(Req))).

-spec get_cookies(wok_req:wok_req()) -> [{binary(), binary()}].
get_cookies(Req) ->
  cowboy_req:parse_cookies(wok_req:get_http_req(Req)).

-spec client_ip(wok_req:wok_req()) -> inet:ip_address().
client_ip(Req) ->
  {IP, _} = cowboy_req:peer(wok_req:get_http_req(Req)),
  IP.

-spec client_port(wok_req:wok_req()) -> inet:port_number().
client_port(Req) ->
  {_, Port} = cowboy_req:peer(wok_req:get_http_req(Req)),
  Port.

-spec body(wok_req:wok_req()) -> {ok | more, binary(), wok_req:wok_req()}.
body(Req) ->
  {Type, Data, CowboyReq} = cowboy_req:body(wok_req:get_http_req(Req)),
  {Type, Data, wok_req:set_http_req(Req, CowboyReq)}.

-spec has_body(wok_req:wok_req()) -> boolean().
has_body(Req) ->
  cowboy_req:has_body(wok_req:get_http_req(Req)).

-spec body_length(wok_req:wok_req()) -> integer().
body_length(Req) ->
  cowboy_req:body_length(wok_req:get_http_req(Req)).

-spec method(wok_req:wok_req()) -> term().
method(Req) ->
  cowboy_req:method(wok_req:get_http_req(Req)).

-spec path(wok_req:wok_req()) -> term().
path(Req) ->
  cowboy_req:path(wok_req:get_http_req(Req)).

-spec header(wok_req:wok_req(), binary(), any()) -> binary() | any() | undefined.
header(Req, Name, Default) ->
  cowboy_req:header(Name, wok_req:get_http_req(Req), Default).

-spec headers(wok_req:wok_req()) -> [{binary(), iodata()}].
headers(Req) ->
  cowboy_req:headers(wok_req:get_http_req(Req)).

-spec post_values(wok_req:wok_req()) -> {ok, [{binary(), binary() | true}], wok_req:wok_req()}
                                        | {error, wok_req:wok_req()}.
post_values(Req) ->
  case cowboy_req:body_qs(wok_req:get_http_req(Req)) of
    {ok, List, CowboyReq} -> {ok, List, wok_req:set_http_req(Req, CowboyReq)};
    {_, CowboyReq} -> {error, wok_req:set_http_req(Req, CowboyReq)}
  end.

-spec get_values(wok_req:wok_req()) -> {ok, [{binary(), binary() | true}], wok_req:wok_req()}
                                       | {error, wok_req:wok_req()}.
get_values(Req) ->
  {ok, cowboy_req:parse_qs(wok_req:get_http_req(Req)), Req}.

-spec binding_values(wok_req:wok_req()) -> {ok, [{binary(), binary() | true}], wok_req:wok_req()}
                                           | {error, wok_req:wok_req()}.
binding_values(Req) ->
  {ok, cowboy_req:bindings(wok_req:get_http_req(Req)), Req}.

-spec get_file(wok_req:wok_req()) -> {ok, binary(), binary(), binary(), wok_req:wok_req()}
                                     | {no_file, wok_req:wok_req()}.
get_file(Req) ->
  get_file(Req, fun(_, _, Data, Acc) -> <<Acc/binary, Data/binary>> end, <<>>).

%-spec get_file(wok_req:wok_req(), binary() | list() | pid() ) -> {:ok}

get_file(Req, FileName) when is_list(FileName) ->
  {ok, FilePid} = file:open(FileName, [append]),
  Return = case get_file(Req,FilePid) of
    {ok, FilePid, Req2} -> {ok, FileName, Req2};
    Error -> Error
  end,
  file:close(FilePid),
  Return;
get_file(Req, FilePid) when is_pid(FilePid) ->
  AppendToFileFn = fun(_, _, Data, _) ->
    file:write(FilePid, Data)
  end,
  case get_file(Req, AppendToFileFn, <<>>) of
    {ok, _, _, ok, Req2} -> {ok, FilePid, Req2};
    Error -> Error
  end.

-type get_file_callback() :: fun((binary(), binary(), binary(), term()) ->{ok, term()} | {error, term(), term()}).
-spec get_file(wok_req:wok_req(), get_file_callback(), any()) -> {ok, binary(), binary(), binary(), wok_req:wok_req()}
                                                          | {no_file, wok_req:wok_req()}.
get_file(Req, Function, Acc) ->
  case cowboy_req:part(wok_req:get_http_req(Req)) of
    {ok, Headers, CowboyReq2} ->
      case cow_multipart:form_data(Headers) of
        {file, _, Filename, ContentType, _} ->
          {Data, CowboyReq3} = get_file_data(CowboyReq2, Filename, ContentType, Function, Acc),
          {ok, Filename, ContentType, Data, wok_req:set_http_req(Req, CowboyReq3)};
        _ ->
          {no_file, wok_req:set_http_req(Req, CowboyReq2)}
      end;
    {done, CowboyReq2} ->
      {no_file, wok_req:set_http_req(Req, CowboyReq2)}
  end.

get_file_data(CowboyReq, Filename, ContentType, Function, Acc) ->
  case cowboy_req:part_body(CowboyReq) of
    {ok, Data, CowboyReq2} ->
      {Function(Filename, ContentType, Data, Acc), CowboyReq2};
    {more, Data, CowboyReq2} ->
      get_file_data(CowboyReq2, Filename, ContentType, Function, Function(Filename, ContentType, Data, Acc))
  end.
