% @hidden
-module(wok_rest_handler).

-export([routes/1]).
-export([init/2]).

routes(Routes) ->
  lager:debug("Routes : ~p", [Routes]),
  cowboy_router:compile([{'_', routes(Routes, [])}]).

init(Req, Opts) ->
  Path = cowboy_req:path(Req),
  {Code, Headers, Body} = case list_to_atom(
                                            string:to_upper(
                                              binary_to_list(
                                                cowboy_req:method(Req)))) of
                                       'OPTIONS' ->
                                         {200, [{<<"Allow">>, allow(Path)}], <<>>};
                                       Action ->
                                         try
                                           case lists:keyfind(Action, 1, Opts) of
                                             {Action, {Module, Function}} ->
                                               {C, H, B, WokState} = erlang:apply(Module, 
                                                                                  Function, 
                                                                                  [Req, wok_state:state()]),
                                               _ = wok_state:state(WokState),
                                               {C, H, B};
                                             {Action, {Module, Function}, Middleware} ->
                                               {C, H, B, MidState} = erlang:apply(Module, 
                                                                                  Function, 
                                                                                  [Req, 
                                                                                   wok_middlewares:state(Middleware)]),
                                               _ = wok_middlewares:state(Middleware, MidState),
                                               {C, H, B};
                                             false ->
                                               {404, [], <<>>}
                                           end
                                         catch
                                           _:_ ->
                                             {500, [], <<>>}
                                         end
                                     end,
  {ok, cowboy_req:reply(Code, Headers, Body, Req), Opts}.

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

