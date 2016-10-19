% @hidden
-module(wok_message_path).

-export([get_message_path_handlers/1, get_message_handlers/2]).

get_message_path_handlers(ControlersDef) ->
  maps:from_list(ControlersDef).

get_message_handlers(Path, CompiledMessagePaths) when is_binary(Path) ->
  get_message_handlers([Path], CompiledMessagePaths);
get_message_handlers(Paths, CompiledMessagePaths) when is_list(Paths) ->
  lists:foldl(fun(Path, Acc) ->
                  lists:umerge(lists:sort(Acc),
                               lists:sort(
                                 get_message_handlers(
                                   binary:split(Path, <<"/">>, [global]),
                                   maps:keys(CompiledMessagePaths), [])))
              end, [], Paths).

get_message_handlers(_, [], Result) ->
  Result;
get_message_handlers(Path, [Service|Services], Result) ->
  get_message_handlers(
    Path, Services,
    message_path_match(
      Path, binary:split(bucs:to_binary(Service), <<"/">>, [global]),
      Service, #{}, Result)).

message_path_match([], [], ServiceName, Params, Result) ->
  [{ServiceName, Params}|Result];
message_path_match(P, [], _, _, Result) when P =/= [] ->
  Result;
message_path_match([], S, _, _, Result) when S =/= [] ->
  Result;
message_path_match([P|Path], [<<":", S/binary>>|Service], ServiceName, Params, Result) ->
  message_path_match(Path, Service, ServiceName, maps:put(S, P, Params), Result);
message_path_match([P|_], [S|_], _, _, Result) when P =/= S,
                                                 P =/= <<"*">>,
                                                 S =/= <<"*">> ->
  Result;
message_path_match([P|Path], [S|Service], ServiceName, Params, Result) when P == S;
                                                                    P == <<"*">>;
                                                                    S == <<"*">> ->
  message_path_match(Path, Service, ServiceName, Params, Result).

