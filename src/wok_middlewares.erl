% @hidden
-module(wok_middlewares).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-export([routes/0]).

-export([start_link/0]).
-export([state/1, state/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

routes() ->
  lists:foldl(fun({Name, Opts}, Acc) ->
                Acc ++ update_routes(erlang:apply(Name, routes, []), Opts, Name, [])
            end, [], wok_config:conf([wok, middlewares], [])).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

state(Middleware) ->
  gen_server:call(?SERVER, {state, Middleware}).

state(Middleware, State) ->
  gen_server:cast(?SERVER, {state, Middleware, State}).

init(_) ->
  {Middlewares, Confs} = lists:foldl(fun({Name, Opts}, {MiddlewaresAcc, ConfsAcc}) ->
                                         case case lists:keyfind(init, 1, Opts) of
                                                false ->
                                                  {ok, nostate};
                                                {init, Init} ->
                                                  erlang:apply(Name, init, [Init])
                                              end of
                                           {ok, Args} ->
                                             {[Name|MiddlewaresAcc], maps:put(Name, Args, ConfsAcc)};
                                           {stop, Reason} ->
                                             lager:info("Middleware ~p stop: ~p", [Name, Reason]),
                                             {MiddlewaresAcc, ConfsAcc}
                                         end
                                     end, {[], #{}}, wok_config:conf([wok, middlewares], [])),
  {ok, #{middlewares => lists:reverse(Middlewares),
         confs => Confs}}.

handle_call({state, Middleware}, _From, #{confs := MStates} = State) ->
  {reply, maps:get(Middleware, MStates, nostate), State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({state, Middleware, MState}, #{confs := MStates} = State) ->
  {noreply, State#{confs => maps:put(Middleware, MState, MStates)}};
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

update_routes([], _, _, Result) ->
  Result;
update_routes([{Verb, Route, Action}|Rest], Opts, Name, Result) ->
  Prefix = case lists:keyfind(route_prefix, 1, Opts) of
             {route_prefix, RoutePrefix} ->
               RoutePrefix;
             _ ->
               case lists:member(no_route_prefix, Opts) of
                 true -> 
                   "";
                 false -> 
                   io_lib:format("/~p", [Name])
               end
           end,
  Route1 = case get_route(Opts, Route) of
             [$/|_] = R -> R;
             R -> "/" ++ R
           end,
  update_routes(Rest, Opts, Name, [{Verb, Prefix ++ Route1, Action, Name}|Result]).

get_route([], Route) ->
  Route;
get_route([{route, Route, NewRoute}|_], Route) ->
  NewRoute;
get_route([_|Rest], Route) ->
  get_route(Rest, Route).

