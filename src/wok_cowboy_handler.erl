% @hidden
-module(wok_cowboy_handler).
-compile([{parse_transform, lager_transform}]).

-export([routes/0, routes/1]).
-export([init/2]).
-export([websocket_handle/3, websocket_info/3]).

routes() ->
  routes(doteki:get_env([wok, rest, routes], []) ++ wok_middlewares:routes()).

routes(Routes) ->
  lager:debug("Routes : ~p", [Routes]),
  {Routes1, Static} = wok_http_handler:routes(Routes),
  {cowboy_router:compile([{'_', compile(Routes1)}]), Static}.

init(Req, Opts) ->
  Path = cowboy_req:path(Req),
  case list_to_atom(
         string:to_upper(
           binary_to_list(
             cowboy_req:method(Req)))) of

    'OPTIONS' ->
      {ok, cowboy_req:reply(200,
                            wok_http_handler:add_access_control_allow_origin(
                              wok_http_handler:cors_headers(Path)), <<>>, Req), Opts};

    Action ->
      try
        WokReq1 = init_req(Req),
        case lists:keyfind(Action, 1, Opts) of
          {Action, {Module, Function}} ->
            WokReq5 = case wok_middlewares:incoming_http(WokReq1) of
                          {continue, WokReq2} ->
                            WokReq3 = erlang:apply(Module, Function, [WokReq2]),
                            WokReq4 = wok_middlewares:outgoing_http(WokReq3),
                            terminate_req(WokReq4);
                          WokReq2 -> WokReq2
                        end,
            WokReq6 = set_response_headers(WokReq5),
            {ok, wok_req:reply(WokReq6), Opts};
          {Action, {Module, Function}, Middleware} ->
            WokReq2 = wok_req:set_local_state(WokReq1, wok_middlewares:state(Middleware)),
            WokReq3 = erlang:apply(Module, Function, [WokReq2]),
            WokReq4 = terminate_req(WokReq3, Middleware),
            WokReq5 = set_response_headers(WokReq4),
            {ok, wok_req:reply(WokReq5), Opts};
          false ->
            case lists:keyfind('WS', 1, Opts) of
              {'WS', Module} ->
                {ok, WokReq2} = erlang:apply(Module, ws_init, [WokReq1]),
                WokReq3 = terminate_req(WokReq2),
                {cowboy_websocket, wok_req:get_http_req(WokReq3), Module};
              {'WS', Module, Middleware} ->
                WokReq2 = wok_req:set_local_state(WokReq1, wok_middlewares:state(Middleware)),
                {ok, WokReq3} = erlang:apply(Module, ws_init, [WokReq2]),
                WokReq4 = terminate_req(WokReq3, Middleware),
                {cowboy_websocket, wok_req:get_http_req(WokReq4), {Module, Middleware}};
              false ->
                {ok, cowboy_req:reply(404, wok_http_handler:add_access_control_allow_origin([]), <<>>, Req), Opts}
            end
        end
      catch
        Class:Error ->
          lager:error("Internal server error ~p:~p : ~p", [Class, Error, erlang:get_stacktrace()]),
          {ok, cowboy_req:reply(500, wok_http_handler:add_access_control_allow_origin([]), <<>>, Req), Opts}
      end
  end.

websocket_handle(Data, Req, {Module, Middleware} = Opts) ->
  WokReq1 = init_req(Req),
  WokReq2 = wok_req:set_local_state(WokReq1, wok_middlewares:state(Middleware)),
  case erlang:apply(Module, ws_handle, [Data, WokReq2]) of
    {R, WokReq3} when R == ok; R == stop ->
      WokReq4 = terminate_req(WokReq3, Middleware),
      {R, wok_req:get_http_req(WokReq4), Opts};
    {R, WokReq3, hibernate} when R == ok; R == stop ->
      WokReq4 = terminate_req(WokReq3, Middleware),
      {R, wok_req:get_http_req(WokReq4), Opts, hibernate};
    {reply, Response, WokReq3} ->
      WokReq4 = terminate_req(WokReq3, Middleware),
      {reply, Response, wok_req:get_http_req(WokReq4), Opts}
  end;
websocket_handle(Data, Req, Module) ->
  WokReq1 = init_req(Req),
  case erlang:apply(Module, ws_handle, [Data, WokReq1]) of
    {R, WokReq2} when R == ok; R == stop ->
      WokReq3 = terminate_req(WokReq2),
      {R, wok_req:get_http_req(WokReq3), Module};
    {R, WokReq2, hibernate} when R == ok; R == stop ->
      WokReq3 = terminate_req(WokReq2),
      {R, wok_req:get_http_req(WokReq3), Module, hibernate};
    {reply, Response, WokReq2} ->
      WokReq3 = terminate_req(WokReq2),
      {reply, Response, wok_req:get_http_req(WokReq3), Module}
  end.

websocket_info(Data, Req, {Module, Middleware} = Opts) ->
  WokReq1 = init_req(Req),
  WokReq2 = wok_req:set_local_state(WokReq1, wok_middlewares:state(Middleware)),
  case erlang:apply(Module, ws_info, [Data, WokReq2]) of
    {R, WokReq3} when R == ok; R == stop ->
      WokReq4 = terminate_req(WokReq3, Middleware),
      {R, wok_req:get_http_req(WokReq4), Opts};
    {R, WokReq3, hibernate} when R == ok; R == stop ->
      WokReq4 = terminate_req(WokReq3, Middleware),
      {R, wok_req:get_http_req(WokReq4), Opts, hibernate};
    {reply, Response, WokReq3} ->
      WokReq4 = terminate_req(WokReq3, Middleware),
      {reply, Response, wok_req:get_http_req(WokReq4), Opts}
  end;
websocket_info(Data, Req, Module) ->
  WokReq1 = init_req(Req),
  case erlang:apply(Module, ws_info, [Data, WokReq1]) of
    {R, WokReq2} when R == ok; R == stop ->
      WokReq3 = terminate_req(WokReq2),
      {R, wok_req:get_http_req(WokReq3), Module};
    {R, WokReq2, hibernate} when R == ok; R == stop ->
      WokReq3 = terminate_req(WokReq2),
      {R, wok_req:get_http_req(WokReq3), Module, hibernate};
    {reply, Response, WokReq2} ->
      WokReq3 = terminate_req(WokReq2),
      {reply, Response, wok_req:get_http_req(WokReq3), Module}
  end.

% private

compile(Routes) ->
  lists:map(fun
              ({Path, Opts}) ->
                {Path, ?MODULE, Opts};
              (Other) ->
                Other
            end, Routes).

init_req(Req) ->
  WokReq = wok_req:set_http_req(wok_req:new(wok_cowboy_req), Req),
  WokReq0 = wok_req:set_handler(WokReq, self()),
  wok_req:set_global_state(WokReq0, wok_state:state()).

terminate_req(WokReq) ->
  _ = wok_state:state(wok_req:get_global_state(WokReq)),
  WokReq.

terminate_req(WokReq, Middleware) ->
  WokReq1 = terminate_req(WokReq),
  _ = wok_middlewares:state(Middleware, wok_req:get_local_state(WokReq1)),
  wok_req:set_local_state(WokReq1, undefined).

set_response_headers(WokReq) ->
  Headers = wok_req:get_response_headers(WokReq),
  Headers2 = wok_http_handler:add_access_control_allow_origin(Headers),
  Headers3 = wok_http_handler:add_access_control_allow_credentials(Headers2),
  wok_req:set_response_headers(WokReq, Headers3).

