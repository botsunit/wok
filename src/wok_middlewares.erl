% @hidden
-module(wok_middlewares).
-compile([{parse_transform, lager_transform}]).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-export([routes/0]).

-export([start_link/0, stop/0]).
-export([state/1, state/2]).
-export([incoming_message/1, outgoing_message/1]).
-export([incoming_http/1, outgoing_http/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

routes() ->
  lists:foldl(fun({Name, Opts}, Acc) ->
                  Acc ++ update_routes(erlang:apply(Name, routes, []), Opts, Name, [])
              end, [], doteki:get_env([wok, middlewares], [])).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
  gen_server:call(?SERVER, stop).

state(Middleware) ->
  gen_server:call(?SERVER, {state, Middleware}).

state(Middleware, State) ->
  gen_server:cast(?SERVER, {state, Middleware, State}).

incoming_message(Message) ->
  gen_server:call(?SERVER, {incoming_message, Message}).

outgoing_message(Message) ->
  gen_server:call(?SERVER, {outgoing_message, Message}).

incoming_http(Req) ->
  gen_server:call(?SERVER, {incoming_http, Req}).

outgoing_http(Resp) ->
  gen_server:call(?SERVER, {outgoing_http, Resp}).

init(_) ->
  {Middlewares, Confs} = lists:foldl(fun({Name, Opts}, {MiddlewaresAcc, ConfsAcc}) ->
                                         case case lists:keyfind(init, 1, Opts) of
                                                false ->
                                                  {ok, nostate};
                                                {init, Init} ->
                                                  erlang:apply(Name, init, [Init])
                                              end of
                                           {ok, Args} ->
                                             lager:debug("Middleware ~p started", [Name]),
                                             {[Name|MiddlewaresAcc], maps:put(Name, Args, ConfsAcc)};
                                           {stop, Reason} ->
                                             lager:debug("Middleware ~p stop: ~p", [Name, Reason]),
                                             {MiddlewaresAcc, ConfsAcc}
                                         end
                                    end, {[], #{}}, doteki:get_env([wok, middlewares], [])),
  {ok, #{middlewares => lists:reverse(Middlewares),
         confs => Confs}}.

handle_call(stop, _From, State) ->
  {stop, normal, ok, State};
handle_call({state, Middleware}, _From, #{confs := MStates} = State) ->
  {reply, maps:get(Middleware, MStates, nostate), State};
handle_call({incoming_message, Message}, _From, #{middlewares := Middlewares,
                                                  confs := MStates} = State) ->
  {Result, MStates2} = middlewares_message(incoming_message, Middlewares, MStates, Message),
  {reply, Result, State#{confs => MStates2}};
handle_call({outgoing_message, Message}, _From, #{middlewares := Middlewares,
                                                  confs := MStates} = State) ->
  {Result, MStates2} = middlewares_message(outgoing_message, lists:reverse(Middlewares), MStates, Message),
  {reply, Result, State#{confs => MStates2}};
handle_call({incoming_http, Req}, _From, #{middlewares := Middlewares,
                                                  confs := Mstates} = State) ->
  {Result, MStates2} = middlewares_incoming_http(Middlewares, Mstates, Req),
  {reply, Result, State#{confs => MStates2}};
handle_call({outgoing_http, Resp}, _From, #{middlewares := Middlewares,
                                                  confs := Mstates} = State) ->
  {Result, MStates2} = middlewares_outgoing_http(lists:reverse(Middlewares), Mstates, Resp),
  {reply, Result, State#{confs => MStates2}};
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
                   lists:flatten(io_lib:format("/~w", [Name]))
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

middlewares_message(Direction, Middlewares, MStates, Message) ->
  middlewares_message(Direction, Middlewares, {{ok, Message}, MStates}).

middlewares_message(Direction, [Middleware|Middlewares], {{ok, Message}, MStates}) ->
  case erlang:apply(Middleware, Direction, [Message, maps:get(Middleware, MStates, nostate)]) of
    {ok, Message1, MState} ->
      middlewares_message(Direction, Middlewares, {{ok, Message1}, maps:put(Middleware, MState, MStates)});
    {stop, Reason, MState} ->
      {{stop, Middleware, Reason}, maps:put(Middleware, MState, MStates)}
  end;
middlewares_message(_, _, Result) ->
  Result.

middlewares_incoming_http(Middlewares, MStates, Req) ->
  middlewares_incoming_http2(Middlewares, Req, {{continue, Req}, MStates}).

middlewares_incoming_http2([Middleware|Middlewares], Req, {{continue, Req}, MStates}) ->
  case erlang:apply(Middleware, incoming_http, [Req, maps:get(Middleware, MStates, nostate)]) of
    {continue, Req2, MState} ->
      middlewares_incoming_http2(Middlewares, Req2, {{continue, Req2}, maps:put(Middleware, MState, MStates)});
    {C, H, B, MState} ->
      {{C, H, B}, maps:put(Middleware, MState, MStates)}
  end;
middlewares_incoming_http2(_, _, Result) ->
  Result.

middlewares_outgoing_http([], MStates, Result) ->
  {Result, MStates};
middlewares_outgoing_http([Middleware|Middlewares], MStates, Resp) ->
  {C, H, B, MState} = erlang:apply(Middleware, outgoing_http, [Resp, maps:get(Middleware, MStates, nostate)]),
  middlewares_outgoing_http(Middlewares, maps:put(Middleware, MState, MStates), {C, H, B}).

