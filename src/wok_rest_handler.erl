% @hidden
-module(wok_rest_handler).
-compile([{parse_transform, lager_transform}]).

-export([routes/1]).
-export([init/2]).
-export([websocket_handle/3, websocket_info/3]).

routes(Routes) ->
  lager:debug("Routes : ~p", [Routes]),
  cowboy_router:compile([{'_', routes(Routes, [])}]).

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
        case lists:keyfind(Action, 1, Opts) of
          {Action, {Module, Function}} ->
            {C, H, B, WokState} = erlang:apply(Module,
                                               Function,
                                               [Req, wok_state:state()]),
            _ = wok_state:state(WokState),
            {ok, cowboy_req:reply(C, H, B, Req), Opts};
          {Action, {Module, Function}, Middleware} ->
            {C, H, B, MidState} = erlang:apply(Module,
                                               Function,
                                               [Req,
                                                wok_middlewares:state(Middleware)]),
            _ = wok_middlewares:state(Middleware, MidState),
            {ok, cowboy_req:reply(C, add_access_control_allow_origin(H), B, Req), Opts};
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

routes([], Routes) ->
  compile(lists:reverse(Routes));
routes([{Path, Handler}|Rest], Acc) ->
  routes(Rest, add_route(Path, 'GET', Handler, Acc));
routes([{Verb, Path, Handler}|Rest], Acc) ->
  routes(Rest, add_route(Path, Verb, Handler, Acc));
routes([{Verb, Path, Handler, Middleware}|Rest], Acc) ->
  routes(Rest, add_route(Path, Verb, Handler, Middleware, Acc)).

add_route(Path, Verb, Handler, Acc) ->
  case lists:keyfind(Path, 1, Acc) of
    {Path, Data} ->
      lists:keyreplace(Path, 1, Acc, {Path, lists:reverse([{Verb, Handler}|Data])});
    false ->
      [{Path, [{Verb, Handler}]}|Acc]
  end.
add_route(Path, Verb, Handler, Middleware, Acc) ->
  case lists:keyfind(Path, 1, Acc) of
    {Path, Data} ->
      lists:keyreplace(Path, 1, Acc, {Path, lists:reverse([{Verb, Handler, Middleware}|Data])});
    false ->
      [{Path, [{Verb, Handler, Middleware}]}|Acc]
  end.

compile(Routes) ->
  lists:map(fun({Path, Opts}) ->
                {Path, ?MODULE, Opts}
            end, Routes).

allow(Ressource) ->
  list_to_binary(
    string:join(
      lists:foldl(
        fun({Verb, Path, _}, Acc) ->
            case {eutils:to_binary(Ressource),
                  eutils:to_binary(Path)} of
              {R, P} when R == <<"*">> orelse R == P ->
                case lists:member(atom_to_list(Verb), Path) of
                  true -> Acc;
                  false -> [atom_to_list(Verb)|Acc]
                end;
              _ ->
                Acc
            end
        end, ["OPTIONS"], wok_config:conf([wok, rest, routes], [])), ",")).

cors_headers(Path) ->
  [
   {<<"Access-Control-Allow-Methods">>,
    ebinary:join(wok_config:conf([wok, rest, cors, 'Access-Control-Allow-Methods'], allow(Path)), <<", ">>)},
   {<<"Access-Control-Max-Age">>,
    eutils:to_binary(wok_config:conf([wok, rest, cors, 'Access-Control-Max-Age'], 1728000))},
   {<<"Access-Control-Allow-Headers">>,
    ebinary:join(wok_config:conf([wok, rest, cors, 'Access-Control-Allow-Headers'],
                                 [<<"Access-Control-Allow-Origin">>,
                                  <<"Authorization">>,
                                  <<"Origin">>,
                                  <<"x-requested-with">>,
                                  <<"Content-Type">>,
                                  <<"Content-Range">>,
                                  <<"Content-Disposition">>,
                                  <<"Content-Description">>]),<<", ">>)}
  ] ++
  case wok_config:conf([wok, rest, cors, 'Access-Control-Expose-Headers'], undefined) of
    undefined -> [];
    H -> [{<<"Access-Control-Expose-Headers">>, ebinary:join(H)}]
  end ++
  case wok_config:conf([wok, rest, cors, 'Access-Control-Allow-Credentials'], undefined) of
    undefined -> [];
    C -> [{<<"Access-Control-Allow-Credentials">>, eutils:to_binary(C)}]
  end.

add_access_control_allow_origin(Headers) ->
  case lists:keyfind(<<"Access-Control-Allow-Origin">>, 1, Headers) of
    false ->
      [{<<"Access-Control-Allow-Origin">>,
        wok_config:conf([wok, rest, cors, 'Access-Control-Allow-Origin'], <<"*">>)}|Headers];
    _ ->
      Headers
  end.

