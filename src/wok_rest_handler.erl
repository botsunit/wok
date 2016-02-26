% @hidden
-module(wok_rest_handler).
-compile([{parse_transform, lager_transform}]).

-export([routes/1]).
-export([init/2]).
-export([websocket_handle/3, websocket_info/3]).
-export([add_access_control_allow_origin/1, cors_headers/1]).
-define(ALLOWED_METHODS, ['GET', 'HEAD', 'POST', 'PUT', 'DELETE', 'TRACE', 'OPTIONS', 'CONNECT', 'PATCH']).

-include("../include/wok.hrl").

routes(Routes) ->
  lager:debug("Routes : ~p", [Routes]),
  {Routes1, Static} = routes(Routes, {[], #{static_path => "",
                                            static_route => ""}}),
  {cowboy_router:compile([{'_', Routes1}]), Static}.

init(Req, Opts) ->
  Path = cowboy_req:path(Req),
  case list_to_atom(
         string:to_upper(
           binary_to_list(
             cowboy_req:method(Req)))) of

    'OPTIONS' ->
      {ok, cowboy_req:reply(200, add_access_control_allow_origin(cors_headers(Path)), <<>>, Req), Opts};

    Action ->
      try
        WokReq = wok_req:set_cowboy_req(wok_req:new(), Req),
        WokReq1 = wok_req:set_global_state(WokReq, wok_state:state()),
        case lists:keyfind(Action, 1, Opts) of
          {Action, {Module, Function}} ->
            WokReq5 = case wok_middlewares:incoming_http(WokReq1) of
                          {continue, WokReq2} ->
                            WokReq3 = erlang:apply(Module, Function, [WokReq2]),
                            WokReq4 = wok_middlewares:outgoing_http(WokReq3),
                            _ = wok_state:state(wok_req:get_global_state(WokReq4)),
                            WokReq4;
                          WokReq2 -> WokReq2
                        end,
            {ok, wok_req:reply(WokReq5), Opts};

          {Action, {Module, Function}, Middleware} ->
            WokReq2 = erlang:apply(Module, Function, [WokReq1])s,
            _ = wok_middlewares:state(Middleware, WokReq2),
            Headers = wok_req:get_response_headers(WokReq2),
            Headers2 = add_access_control_allow_origin(Headers),
            WokReq3 = wok_req:set_response_headers(Headers2, WokReq2)
            {ok, wok_req:reply(WokReq3), Opts};
          false ->
            case lists:keyfind('WS', 1, Opts) of
              {'WS', Module} ->
                {ok, Req2, WokState} = erlang:apply(Module, ws_init, [Req, self(), wok_state:state()]),
                _ = wok_state:state(WokState),
                {cowboy_websocket, Req2, Module};
              {'WS', Module, Middleware} ->
                {ok, Req2, MidState} = erlang:apply(Module, ws_init, [Req, self(), wok_middlewares:state(Middleware)]),
                _ = wok_middlewares:state(Middleware, MidState),
                {cowboy_websocket, Req2, {Module, Middleware}};
              false ->
                {ok, cowboy_req:reply(404, add_access_control_allow_origin([]), <<>>, Req), Opts}
            end
        end
      catch
        Class:Error ->
          lager:error("Internal server error ~p:~p : ~p", [Class, Error, erlang:get_stacktrace()]),
          {ok, cowboy_req:reply(500, add_access_control_allow_origin([]), <<>>, Req), Opts}
      end
  end.

websocket_handle(Data, Req, {Module, Middleware} = Opts) ->
  case websocket_handle(Module, Data, Req, wok_middlewares:state(Middleware)) of
    {ok, Req2, State} ->
      _ = wok_middlewares:state(Middleware, State),
      {ok, Req2, Opts};
    {reply, Response, Req2, State} ->
      _ = wok_middlewares:state(Middleware, State),
      {reply, Response, Req2, Opts}
  end;
websocket_handle(Data, Req, Module) ->
  case websocket_handle(Module, Data, Req, wok_state:state()) of
    {ok, Req2, State} ->
      _ = wok_state:state(State),
      {ok, Req2, Module};
    {reply, Response, Req2, State} ->
      _ = wok_state:state(State),
      {reply, Response, Req2, Module}
  end.
websocket_handle(Module, Data, Req, State) ->
  erlang:apply(Module, ws_handle, [Data, Req, self(), State]).

websocket_info(Data, Req, {Module, Middleware} = Opts) ->
  case websocket_info(Module, Data, Req, wok_middlewares:state(Middleware)) of
    {ok, Req2, State} ->
      _ = wok_middlewares:state(Middleware, State),
      {ok, Req2, Opts};
    {reply, Response, Req2, State} ->
      _ = wok_middlewares:state(Middleware, State),
      {reply, Response, Req2, Opts}
  end;
websocket_info(Data, Req, Module) ->
  case websocket_info(Module, Data, Req, wok_state:state()) of
    {ok, Req2, State} ->
      _ = wok_state:state(State),
      {ok, Req2, Module};
    {reply, Response, Req2, State} ->
      _ = wok_state:state(State),
      {reply, Response, Req2, Module}
  end.
websocket_info(Module, Data, Req, State) ->
  erlang:apply(Module, ws_info, [Data, Req, self(), State]).

% private

routes([], {Routes, Static}) ->
  lager:debug("routes => ~p", [Routes]),
  {compile(lists:reverse(Routes)), Static};
routes([{Path, Handler}|Rest], Acc) ->
  routes(Rest, add_route(Path, 'GET', Handler, Acc));
routes([{Verb, Path, Handler}|Rest], Acc) ->
  routes(Rest, add_route(Path, Verb, Handler, Acc));
routes([{Verb, Path, Handler, Middleware}|Rest], Acc) ->
  routes(Rest, add_route(Path, Verb, Handler, Middleware, Acc)).
add_route(Path, static, Filepath, {Routes, _}) ->
  StaticPath = static_path(Filepath),
  StaticRoute = bucuri:join(Path, "[...]"),
  {case lists:keyfind(StaticRoute,1, Routes) of
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

compile(Routes) ->
  lists:map(fun
              ({Path, Opts}) ->
                {Path, ?MODULE, Opts};
              (Other) ->
                Other
            end, Routes).

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
                                    <<"Content-Description">>]),<<", ">>)}
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
