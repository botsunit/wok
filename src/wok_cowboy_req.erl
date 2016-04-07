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
  get_file(Req, fun(_, _, Data, Acc) -> {ok, <<Acc/binary, Data/binary>>} end, <<>>).

-spec get_file(wok_req:wok_req(), wok_request:get_file_callback()
                                  | list()
                                  | pid()) -> {ok, file:filename_all() | pid(), wok_req:wok_req()}
                                              | {ok, wok_req:wok_req()}
                                              | {error, term(), file:filename_all() | pid(), woq_req:wok_req()}
                                              | {error, term(), woq_req:wok_req()}
                                              | {no_file, file:filename_all() | pid(), wok_req:wok_req()}
                                              | {no_file, wok_req:wok_req()}.
get_file(Req, FileName) when is_list(FileName) orelse is_binary(FileName) ->
  case file:open(FileName, [append]) of
    {ok, FilePid} ->
      case get_file(Req, FilePid) of
        {ok, FilePid2, Req2} ->
          case file:close(FilePid2) of
            ok ->
              {ok, FileName, Req2};
            {error, Reason} ->
              {error, Reason, FileName, Req2}
          end;
        {error, Reason, FilePid2, Req2} ->
          _ = file:close(FilePid2),
          {error, Reason, FileName, Req2};
        {no_file, FilePid2, Req2} ->
          _ = file:close(FilePid2),
          {no_file, FileName, Req2}
      end;
    {error, Reason} ->
      {error, Reason, FileName, Req}
  end;
get_file(Req, FilePid) when is_pid(FilePid) ->
  AppendToFileFn = fun(_, _, Data, Acc) ->
    case file:write(Acc, Data) of
      ok ->
        {ok, Acc};
      {error, Reason} ->
        {error, Reason, Acc}
    end
  end,
  case get_file(Req, AppendToFileFn, FilePid) of
    {ok, _, _, FilePid2, Req2} ->
      {ok, FilePid2, Req2};
    Other ->
      Other
  end;
get_file(Req, Function) when is_function(Function) ->
  case get_file(Req, Function, <<>>) of
    {ok, _, _, _, Req2} ->
      {ok, Req2};
    {error, Reason, _, Req2} ->
      {error, Reason, Req2};
    {no_file, _, Req2} ->
      {no_file, Req2}
  end.

-spec get_file(wok_req:wok_req(), wok_request:get_file_callback(), any()) ->
      {ok, binary(), binary(), any(), wok_req:wok_req()}
      | {error, term(), any(), wok_req:wok_req()}
      | {no_file, any(), wok_req:wok_req()}.

get_file(Req, Function, Acc) ->
  case cowboy_req:part(wok_req:get_http_req(Req)) of
    {ok, Headers, CowboyReq2} ->
      case cow_multipart:form_data(Headers) of
        {file, _, Filename, ContentType, _} ->
          case get_file_data(CowboyReq2, Filename, ContentType, Function, Acc) of
            {ok, Acc2, CowboyReq3} ->
              {ok, Filename, ContentType, Acc2, wok_req:set_http_req(Req, CowboyReq3)};
            {error, Reason, Acc2, CowboyReq3} ->
              {error, Reason, Acc2, wok_req:set_http_req(Req, CowboyReq3)}
          end;
        _ ->
          {no_file, Acc, wok_req:set_http_req(Req, CowboyReq2)}
      end;
    {done, CowboyReq2} ->
      {no_file, Acc, wok_req:set_http_req(Req, CowboyReq2)}
  end.

get_file_data(CowboyReq, Filename, ContentType, Function, Acc) ->
  {Status, Data, CowboyReq2} = cowboy_req:part_body(CowboyReq),
  case Function(Filename, ContentType, Data, Acc) of
    {ok, Acc2} when Status =:= ok -> {ok, Acc2, CowboyReq2};
    {ok, Acc2} when Status =:= more -> get_file_data(CowboyReq2, Filename, ContentType, Function, Acc2);
    {error, Reason, Acc2} -> {error, Reason, Acc2, CowboyReq2}
  end.
