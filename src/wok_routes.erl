-module(wok_routes).

-export([static/0, static/1]).
-export([paths/2, path/2, path/3, path/4]).

% @doc
% Return the static route
% @end
-spec static() -> string().
static() ->
  bucs:to_string(wok_state:static(route)).

% @doc
% Return the static route
% @end
-spec static(Path :: string() | binary()) -> string().
static(Path) ->
  Static = static(),
  case lists:reverse(Static) of
    "/" ++ _ ->
      Static ++ bucs:to_string(Path);
    "" ->
      bucs:to_string(Path);
    _ ->
      Static ++ "/" ++ bucs:to_string(Path)
  end.

% @equiv path('GET', Handler, Function)
path(Handler, Function) when is_atom(Handler),
                             is_atom(Function) ->
  path('GET', Handler, Function).
% @doc
% Return the path for the given <tt>Verb</tt>, <tt>Handler</tt> and <tt>Function</tt>
%
% Or
%
% equiv to path('GET', Verb, Handler, Function, Args).
% @end
path(Verb, Handler, Function) when (Verb == 'GET' orelse
                                    Verb == 'HEAD' orelse
                                    Verb == 'POST' orelse
                                    Verb == 'PUT' orelse
                                    Verb == 'DELETE' orelse
                                    Verb == 'TRACE' orelse
                                    Verb == 'OPTIONS' orelse
                                    Verb == 'CONNECT' orelse
                                    Verb == 'PATCH'),
                                   is_atom(Handler),
                                   is_atom(Function) ->
  case paths(Handler, Function) of
    [] -> undefined;
    Paths -> buclists:keyfind(Verb, 1, Paths, undefined)
  end;
path(Handler, Function, Args) when is_atom(Handler),
                                   is_atom(Function),
                                   is_map(Args) ->
  path('GET', Handler, Function, Args).

% @doc
% Return the path for the given <tt>Verb</tt>, <tt>Handler</tt> and <tt>Function</tt>
% and replace the bindings values by the ones in the given map.
% @end
path(Verb, Handler, Function, Args) when (Verb == 'GET' orelse
                                          Verb == 'HEAD' orelse
                                          Verb == 'POST' orelse
                                          Verb == 'PUT' orelse
                                          Verb == 'DELETE' orelse
                                          Verb == 'TRACE' orelse
                                          Verb == 'OPTIONS' orelse
                                          Verb == 'CONNECT' orelse
                                          Verb == 'PATCH'),
                                         is_atom(Handler),
                                         is_atom(Function),
                                         is_map(Args) ->
  case path(Verb, Handler, Function) of
    undefined -> undefined;
    Path ->
      maps:fold(fun(K, V, Acc) ->
                    K1 = "/:" ++ bucs:to_string(K) ++ "(/|\\Z)",
                    V1 = "/" ++ bucs:to_string(V) ++ "\\1",
                    re:replace(Acc, K1, V1, [{return,list}, global])
                end, Path, Args)
  end.

% @doc
% @end
-spec paths(Handler :: atom(), Function :: atom()) -> string().
paths(Handler, Function) when is_atom(Handler),
                              is_atom(Function) ->
  {Routes, _} = wok_http_handler:routes(),
  lists:foldl(fun
                ({Path, Handlers}, Acc) when is_list(Handlers) ->
                  Acc ++ lists:foldl(fun
                                       ({Verb, {Handler1, Function1}}, Acc2)
                                         when Handler1 == Handler,
                                              Function1 == Function -> [{Verb, Path}|Acc2];
                                       ({Verb, {Handler1, Function1}, _}, Acc2)
                                         when Handler1 == Handler,
                                              Function1 == Function -> [{Verb, Path}|Acc2];
                                       (_, Acc2) -> Acc2
                                     end, [], Handlers);
                (_, Acc) -> % Because of static handler
                  Acc
              end, [], Routes).

