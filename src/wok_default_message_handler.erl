% @hidden
-module(wok_default_message_handler).
-behaviour(wok_message_handler).
-include("../include/wok_message_handler.hrl").
-include("../include/wok_default_message_handler.hrl").

-export([
         create/3,
         create/4,
         parse/1
        ]).

% @equiv create(From, To, Body, [{headers, #{}}, {wok_version, 1}])
-spec create(binary(), list() | binary(), binary()) -> binary().
create(From, To, Body) ->
  create(From, To, Body, [{wok_version, ?WOK_VERSION},
                          {headers, ?DEFAULT_HEADERS}]).

-spec create(binary(), list() | binary(), binary(), list()) -> binary().
create(From, To, Body, []) when is_binary(From),
                                is_binary(To) ->
  create(From, To, Body);
create(From, To, Body, Options) when is_binary(From),
                                     is_binary(To),
                                     is_list(Options) ->
  create(From, [To], Body, Options);
create(From, To, Body, Options) when is_binary(From),
                                     is_list(To),
                                     is_list(Options) ->
  {Version, Headers} = options(Options),
  {Headers1, Body1} = case is_binary(Body) of
                        true -> {Headers#{binary_body => true}, Body};
                        false -> {Headers#{binary_body => false}, term_to_binary(Body)}
                      end,
  {Headers2, CompressedBody} = case maps:get(compress, Headers1, ?DEFAULT_HEADERS_COMPRESS) of
                                 false -> {Headers1#{compress => false}, Body1};
                                 true -> {Headers1#{compress => true}, zlib:compress(Body1)}
                               end,
  UUID = bucs:to_binary(uuid:to_string(uuid:uuid4())),
  <<Version:8/unsigned,
    (encode(To))/binary,
    (encode(From))/binary,
    (encode(UUID))/binary,
    (encode(Headers2))/binary,
    (crc(Version, To, From, UUID, Headers2)):32/unsigned,
    (crc(Body1)):32/unsigned,
    (encode(CompressedBody))/binary>>.

-spec parse(binary()) -> {ok, wok_message_handler:message(), binary()} | {error, term()}.
parse(<<Version:8/unsigned, Message/binary>>) when Version == ?WOK_VERSION ->
  case decode(Message) of
    {ok, {To, Message1}} ->
      case decode(Message1) of
        {ok, {From, Message2}} ->
          case decode(Message2) of
            {ok, {UUID, Message3}} ->
              case decode(Message3) of
                {ok, {Headers, Message4}} ->
                  case Message4 of
                    <<CRCHeader:32/unsigned, CRCBody:32/unsigned, Message5/binary>> ->
                      case decode(Message5) of
                        {ok, {CBody, Rest}} ->
                          Body = case Headers of
                                   #{compress := true} -> zlib:uncompress(CBody);
                                   _ -> CBody
                                 end,
                          case {crc(Version, To, From, UUID, Headers), crc(Body)} of
                            {CRCHeader, CRCBody} ->
                              {ok, #msg{
                                      uuid = UUID,
                                      from = From,
                                      to = To,
                                      headers = Headers,
                                      body = case Headers of
                                               #{binary_body := true} -> Body;
                                               _ -> binary_to_term(Body)
                                             end
                                     }, Rest};
                            {CRCHeader, _} ->
                              {error, invalid_crc_body};
                            {_, CRCBody} ->
                              {error, invalid_crc_header}
                          end;
                        _ ->
                          {error, invalid_body}
                      end;
                    _ ->
                      {error, invalid_crc}
                  end;
                _ ->
                  {error, invalid_content_type}
              end;
            _ ->
              {error, invalid_uuid}
          end;
        _ ->
          {error, invalid_to}
      end;
    _ ->
      {error, invalid_from}
  end;
parse(_) ->
  {error, invalid_version}.

% private

options(Options) ->
  {buclists:keyfind(wok_version, 1, Options, ?WOK_VERSION),
   buclists:keyfind(headers, 1, Options, ?DEFAULT_HEADERS)}.

decode(<<?TYPE_STRING:8/unsigned, Rest/binary>>) ->
  case decode(Rest) of
    {ok, {Size, Rest1}} when is_integer(Size) ->
      <<Data:Size/binary, Rest2/binary>> = Rest1,
      {ok, {Data, Rest2}};
    _ ->
      {error, invalid_string}
  end;
decode(<<?TYPE_ATOM:8/unsigned, Rest/binary>>) ->
  case decode(Rest) of
    {ok, {Size, Rest1}} when is_integer(Size) ->
      <<Data:Size/binary, Rest2/binary>> = Rest1,
      {ok, {bucs:to_atom(Data), Rest2}};
    _ ->
      {error, invalid_string}
  end;
decode(<<?TYPE_INT:8/unsigned, Size:8/unsigned, Data:Size/signed, Rest/binary>>) ->
  {ok, {Data, Rest}};
decode(<<?TYPE_UINT:8/unsigned, Size:8/unsigned, Data:Size/unsigned, Rest/binary>>) ->
  {ok, {Data, Rest}};
decode(<<?TYPE_FLOAT:8/unsigned, Data/float, Rest/binary>>) ->
  {ok, {Data, Rest}};
decode(<<?TYPE_LIST:8/unsigned, Rest/binary>>) ->
  case decode(Rest) of
    {ok, {Size, Rest1}} when is_integer(Size) ->
      decode_array(Size, Rest1, []);
    _ ->
      {error, invalid_array}
  end;
decode(<<?TYPE_MAP:8/unsigned, Rest/binary>>) ->
  case decode(Rest) of
    {ok, {Size, Rest1}} when is_integer(Size) ->
      decode_map(Size, Rest1, #{});
    _ ->
      {error, invalid_map}
  end;
decode(<<?TYPE_BOOLEAN_TRUE:8/unsigned, 1, Rest/binary>>) ->
  {ok, {true, Rest}};
decode(<<?TYPE_BOOLEAN_FALSE:8/unsigned, 0, Rest/binary>>) ->
  {ok, {false, Rest}};


decode(<<Size:32/unsigned, Data:Size/binary, Rest/binary>>) ->
  {ok, {Data, Rest}};
decode(<<>>) ->
  no_data;
decode(_) ->
  error.

decode_array(0, Rest, Acc) -> {ok, {lists:reverse(Acc), Rest}};
decode_array(N, Data, Acc) ->
  case decode(Data) of
    {ok, {Element, Rest}} ->
      decode_array(N - 1, Rest, [Element|Acc]);
    E ->
      E
  end.

decode_map(0, Rest, Acc) -> {ok, {Acc, Rest}};
decode_map(N, Data, Acc) ->
  case decode(Data) of
    {ok, {Key, Rest1}} ->
      case decode(Rest1) of
        {ok, {Value, Rest2}} ->
          decode_map(N - 1, Rest2, maps:put(Key, Value, Acc));
        EV ->
          EV
      end;
    EK ->
      EK
  end.

encode(Data) when is_binary(Data) ->
  <<?TYPE_STRING:8/unsigned, (encode(size(Data)))/binary, Data/binary>>;
encode(Data) when is_integer(Data), Data >= 0 ->
  encode_unsigned_integer(Data);
encode(Data) when is_integer(Data), Data < 0 ->
  encode_signed_integer(Data);
encode(Data) when is_float(Data) ->
  <<?TYPE_FLOAT:8/unsigned, Data/float>>;
encode(Data) when is_list(Data) ->
  encode_list(Data, <<?TYPE_LIST:8/unsigned, (encode(length(Data)))/binary>>);
encode(Data) when is_map(Data) ->
  encode_map(Data, <<?TYPE_MAP:8/unsigned, (encode(maps:size(Data)))/binary>>);
encode(true) ->
  <<?TYPE_BOOLEAN_TRUE:8/unsigned, 1>>;
encode(false) ->
  <<?TYPE_BOOLEAN_FALSE:8/unsigned, 0>>;
encode(Data) when is_atom(Data) ->
  Data1 = to_binary(Data),
  <<?TYPE_ATOM:8/unsigned, (encode(size(Data1)))/binary, Data1/binary>>.

encode_unsigned_integer(Data) ->
  encode_integer(Data, 8, ?TYPE_UINT).
encode_signed_integer(Data) ->
  encode_integer(Data, 8, ?TYPE_INT).
encode_integer(Data, Size, ?TYPE_UINT) ->
  encode_integer(Data, Size, {?INT_UMIN(Size), ?INT_UMAX(Size)}, ?TYPE_UINT);
encode_integer(Data, Size, ?TYPE_INT) ->
  encode_integer(Data, Size, {?INT_MIN(Size), ?INT_MAX(Size)}, ?TYPE_INT).
encode_integer(Data, Size, {Min, Max}, ?TYPE_INT) when Data >= Min, Data =< Max ->
  <<?TYPE_INT:8/unsigned, Size:8/unsigned, Data:Size/signed>>;
encode_integer(Data, Size, {Min, Max}, ?TYPE_UINT) when Data >= Min, Data =< Max ->
  <<?TYPE_UINT:8/unsigned, Size:8/unsigned, Data:Size/signed>>;
encode_integer(Data, Size, _, Type) ->
  encode_integer(Data, Size + Size, Type).

encode_list([], Acc) -> Acc;
encode_list([Element|Rest], Acc) ->
  encode_list(Rest, <<Acc/binary, (encode(Element))/binary>>).

encode_map(Data, Init) ->
  maps:fold(fun(Key, Value, Acc) ->
               <<Acc/binary, (encode(Key))/binary, (encode(Value))/binary>>
           end, Init, Data).

crc(Version, To, From, UUID, Headers) ->
  crc(<<Version:8/unsigned,
        (to_binary(To))/binary,
        (to_binary(From))/binary,
        (to_binary(UUID))/binary,
        (to_binary(Headers))/binary>>).
crc(Binary) when is_binary(Binary) ->
  Z = zlib:open(),
  Value = zlib:crc32(Z, Binary),
  zlib:close(Z),
  Value.

to_binary(Data) when is_list(Data); is_map(Data) ->
  term_to_binary(Data);
to_binary(Data) ->
  bucs:to_binary(Data).


