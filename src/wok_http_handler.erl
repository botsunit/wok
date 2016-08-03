% @hidden
-module(wok_http_handler).
-compile([{parse_transform, lager_transform}]).

-export([routes/0, routes/1]).
-export([add_access_control_allow_origin/1, add_access_control_allow_credentials/1, cors_headers/1]).

-define(ALLOWED_METHODS, ['GET', 'HEAD', 'POST', 'PUT', 'DELETE', 'TRACE', 'OPTIONS', 'CONNECT', 'PATCH']).

routes() ->
  routes(wok_middlewares:routes() ++ doteki:get_env([wok, rest, routes], [])).

routes(Routes) ->
  routes(
    lists:sort(fun
                 ({_, A, _, _}, {_, B, _, _}) ->
                   A > B;
                 ({_, A, _}, {_, B, _, _}) ->
                   A > B;
                 ({_, A, _, _}, {_, B, _}) ->
                   A > B;
                 ({_, A, _}, {_, B, _}) ->
                   A > B
               end,
               Routes),
    {[], #{static_path => "",
           static_route => ""}}).

cors_headers(Path) ->
  [
   {<<"Access-Control-Allow-Methods">>,
    bucbinary:join(doteki:get_env([wok, rest, cors, 'Access-Control-Allow-Methods'], allow(Path)), <<", ">>)},
   {<<"Access-Control-Max-Age">>,
    bucs:to_binary(doteki:get_env([wok, rest, cors, 'Access-Control-Max-Age'], 1728000))},
   {<<"Access-Control-Allow-Headers">>,
    bucbinary:join(doteki:get_env([wok, rest, cors, 'Access-Control-Allow-Headers'],
                                   [<<"Access-Control-Allow-Origin">>,
                                    <<"Authorization">>,
                                    <<"Origin">>,
                                    <<"x-requested-with">>,
                                    <<"Content-Type">>,
                                    <<"Content-Range">>,
                                    <<"Content-Disposition">>,
                                    <<"Content-Description">>]), <<", ">>)}
  ] ++
  case doteki:get_env([wok, rest, cors, 'Access-Control-Expose-Headers'], undefined) of
    undefined -> [];
    H -> [{<<"Access-Control-Expose-Headers">>, bucbinary:join(H, <<", ">>)}]
  end ++
  case doteki:get_env([wok, rest, cors, 'Access-Control-Allow-Credentials'], undefined) of
    undefined -> [];
    C -> [{<<"Access-Control-Allow-Credentials">>, bucs:to_binary(C)}]
  end.

add_access_control_allow_origin(Headers) ->
  case lists:keyfind(<<"Access-Control-Allow-Origin">>, 1, Headers) of
    false ->
      [{<<"Access-Control-Allow-Origin">>,
        doteki:get_env([wok, rest, cors, 'Access-Control-Allow-Origin'], <<"*">>)}|Headers];
    _ ->
      Headers
  end.

add_access_control_allow_credentials(Headers) ->
  case lists:keyfind(<<"Access-Control-Allow-Credentials">>, 1, Headers) of
    false ->
      [{<<"Access-Control-Allow-Credentials">>,
        doteki:get_as_binary([wok, rest, cors, 'Access-Control-Allow-Credentials'], <<"false">>)}|Headers];
    _ ->
      Headers
  end.

routes([], {Routes, Static}) ->
  lager:debug("routes => ~p", [Routes]),
  {lists:reverse(Routes), Static};
routes([{namespace, NS, Routes}|Rest], Acc) ->
  routes(
    lists:map(fun
                ({Path, Handler}) ->
                  {NS ++ Path, Handler};
                ({resources, Name, Handler}) ->
                  {resources, Name, Handler, NS};
                ({resources, Name, Handler, Namespace}) ->
                  {resources, Name, Handler, NS ++ Namespace};
                ({Verb, Path, Handler}) ->
                  {Verb, NS ++ Path, Handler};
                (Route) ->
                  Route
              end, Routes) ++ Rest, Acc);
routes([{resources, Name, Handler}|Rest], Acc) ->
  routes([{resources, Name, Handler, ""}|Rest], Acc);
routes([{resources, Name, Handler, NS}|Rest], Acc) ->
  BasePath = NS ++ "/" ++ bucs:to_string(Name),
  routes(
    lists:foldl(fun({Verb, Method, CmplPath}, NewRoutes) ->
                    case bucs:function_exists(Handler, Method, 1) of
                      true ->
                        [{Verb, BasePath ++ CmplPath, {Handler, Method}}|NewRoutes];
                      false ->
                        NewRoutes
                    end
                end, [], [{'GET', index, ""},
                          {'GET', new, "/new"},
                          {'POST', create, ""},
                          {'GET', show, "/:id"},
                          {'GET', edit, "/:id/edit"},
                          {'PATCH', update, "/:id"},
                          {'PUT', update, "/:id"},
                          {'DELETE', destroy, "/:id"}]) ++ Rest, Acc);
routes([{Path, Handler}|Rest], Acc) ->
  routes(Rest, add_route(Path, 'GET', Handler, Acc));
routes([{Verb, Path, Handler}|Rest], Acc) ->
  routes(Rest, add_route(Path, Verb, Handler, Acc));
routes([{Verb, Path, Handler, Middleware}|Rest], Acc) ->
  routes(Rest, add_route(Path, Verb, Handler, Middleware, Acc)).
add_route(Path, static, Filepath, {Routes, _}) ->
  StaticPath = static_path(Filepath),
  StaticRoute = bucuri:join(Path, "[...]"),
  {case lists:keyfind(StaticRoute, 1, Routes) of
    {StaticRoute, _} ->
      lists:keyreplace(StaticRoute, 1, Routes, {StaticRoute,
                                                cowboy_static,
                                                {dir, StaticPath,
                                                 [{mimetypes, cow_mimetypes, all},
                                                  {default_file, "index.html"}]}});
    false ->
      [{StaticRoute,
        cowboy_static,
        {dir, StaticPath, [{mimetypes, cow_mimetypes, all},
                           {default_file, "index.html"}]}}|Routes]
  end, #{static_path => StaticPath, static_route => Path}};
add_route(Path, Verb, Handler, {Routes, Static}) ->
  {case lists:keyfind(Path, 1, Routes) of
     {Path, Data} ->
       lists:keyreplace(Path, 1, Routes, {Path, lists:reverse([{Verb, Handler}|Data])});
     false ->
       [{Path, [{Verb, Handler}]}|Routes]
   end, Static}.
add_route(Path, Verb, Handler, Middleware, {Routes, Static}) ->
  {case lists:keyfind(Path, 1, Routes) of
     {Path, Data} ->
       lists:keyreplace(Path, 1, Routes, {Path, lists:reverse([{Verb, Handler, Middleware}|Data])});
     false ->
       [{Path, [{Verb, Handler, Middleware}]}|Routes]
   end, Static}.

static_path({priv_dir, App}) ->
  buccode:priv_dir(App);
static_path({priv_dir, App, Extra}) ->
  filename:join(buccode:priv_dir(App), Extra);
static_path({dir, Dir}) ->
  Dir.

allow(Ressource) ->
  list_to_binary(
    string:join(
      lists:foldl(
        fun({Verb, Path, _}, Acc) ->
            case {bucs:to_binary(Ressource),
                  bucs:to_binary(Path)} of
              {R, P} when R =:= <<"*">> orelse R =:= P ->
                case lists:member(Verb, ?ALLOWED_METHODS) of
                  true -> [atom_to_list(Verb)|Acc];
                  false -> Acc
                end;
              _ ->
                Acc
            end
        end, ["OPTIONS"], doteki:get_env([wok, rest, routes], [])), ", ")).

