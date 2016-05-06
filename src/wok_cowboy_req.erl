% @hidden
-module(wok_cowboy_req).
-behaviour(wok_req).

% API
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
        ]).

% internal use
-export([
         post_values/1
         , get_values/1
         , binding_values/1
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

% internal use

-spec post_values(cowboy_req:req()) -> {ok,
                                        [{binary(), binary() | true}], % DATA
                                        [{binary(), binary(), binary()}], % FILES
                                        binary() | undefined,
                                        cowboy_req:req()}
                                       | {error, cowboy_req:req()}.
post_values(CowboyReq) ->
  case header(CowboyReq, <<"content-type">>, undefined) of
    <<"multipart/form-data", _/binary>> ->
      {CowboyReq2, Datas, Files, TempFilePath} = get_request_parts(cowboy_req:part(CowboyReq), [], [], undefined),
      {ok, Datas, Files, TempFilePath, CowboyReq2};
    <<"application/x-www-form-urlencoded">> ->
      case cowboy_req:body_qs(CowboyReq) of
        {ok, Datas, CowboyReq2} -> {ok, Datas, [], undefined, CowboyReq2};
        {_, CowboyReq2} -> {error, CowboyReq2}
      end;
    _ ->
      {ok, [], [], undefined, CowboyReq}
  end.

-spec get_values(cowboy_req:req()) -> {ok, [{binary(), binary() | true}], cowboy_req:req()}
                                      | {error, cowboy_req:req()}.
get_values(CowboyReq) ->
  {ok, cowboy_req:parse_qs(CowboyReq), CowboyReq}.

-spec binding_values(cowboy_req:req()) -> {ok, [{binary(), binary() | true}], cowboy_req:req()}
                                          | {error, cowboy_req:req()}.
binding_values(CowboyReq) ->
  {ok, cowboy_req:bindings(CowboyReq), CowboyReq}.

get_request_parts({done, CowboyReq}, Datas, Files, TempFilePath) ->
  {CowboyReq, Datas, Files, TempFilePath};
get_request_parts({ok, Headers, CowboyReq}, Datas, Files, TempFilePath) ->
  case cow_multipart:form_data(Headers) of
    {data, FieldName} ->
      {Value, CowboyReq2} = get_part_body(CowboyReq, <<>>),
      get_request_parts(cowboy_req:part(CowboyReq2), [{FieldName, Value}|Datas], Files, TempFilePath);
    {file, FieldName, Filename, Type, _} ->
      {File, TempFilePath2, CowboyReq2} = get_part_file(CowboyReq, Filename, TempFilePath),
      get_request_parts(cowboy_req:part(CowboyReq2), Datas, [{FieldName, Type, File}|Files], TempFilePath2)
  end.

get_part_body(CowboyReq, Acc) when is_binary(Acc) ->
  case cowboy_req:part_body(CowboyReq) of
    {ok, Data, CowboyReq2} ->
      {<<Acc/binary, Data/binary>>, CowboyReq2};
    {more, Data, CowboyReq2} ->
      get_part_body(CowboyReq2, <<Acc/binary, Data/binary>>)
  end;
get_part_body(CowboyReq, IO) when is_pid(IO) ->
  {Status, Data, CowboyReq2} = cowboy_req:part_body(CowboyReq),
  ok = file:write(IO, Data), % TODO: return error
  if
    Status == more ->
      get_part_body(CowboyReq2, IO);
    true ->
      CowboyReq2
  end.

get_part_file(CowboyReq, Filename, TempFilePath) ->
  TempFilePath2 = case TempFilePath of
                    undefined -> bucs:to_binary(tempdir:name());
                    _ -> TempFilePath
                  end,
  TempFile = <<TempFilePath2/binary, Filename/binary>>,
  ok = filelib:ensure_dir(TempFile), % TODO: return error
  {ok, IO} = file:open(TempFile, [write, binary]), % TODO: return error
  CowboyReq2 = get_part_body(CowboyReq, IO),
  ok = file:close(IO), % TODO: return error
  {TempFile, TempFilePath2, CowboyReq2}.

