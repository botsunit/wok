% @hidden
-module(wok_sup).

-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

-define(CHILD(I, Args, Type, Shutdown), {I, {I, start_link, Args}, permanent, Shutdown, Type, [I]}).

start_link(Static) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, [Static]).

init([Static]) ->
  Childs = [?CHILD(wok_state, [Static], worker, 5000),
            ?CHILD(wok_middlewares, [], worker, 5000),
            ?CHILD(wok_plugins, [], worker, 5000)] ++
            case doteki:get_env([wok, messages]) of
              undefined ->
                [];
              _ ->
                [
                 % ?CHILD(wok_messages_sup, [], supervisor, infinity)
                 ?CHILD(wok_consumer_groups, [], worker, 5000)
                ]
            end ++
            case doteki:get_env([wok, producer]) of
              undefined ->
                [];
              _ ->
                [
                 % TODO REWRITE ?CHILD(wok_producer_sup, [], supervisor, infinity)
                ]
            end ++ custom_servers() ++ middlewares_servers(),
  {ok, {
     {one_for_one, 5, 10},
     Childs
    }
  }.

custom_servers() ->
  servers(doteki:get_env([wok, start], [])).

middlewares_servers() ->
  lists:foldl(fun({_, Options}, Acc) ->
                  case lists:keyfind(start, 1, Options) of
                    false ->
                      Acc;
                    {start, Servers} ->
                      Acc ++ servers(Servers)
                  end
            end, [], doteki:get_env([wok, middlewares], [])).

servers(Servers) ->
  lists:map(fun
              ({Module, Args, worker}) ->
                ?CHILD(Module, Args, worker, 5000);
              ({Module, Args, supervisor}) ->
                ?CHILD(Module, Args, supervisor, infinity);
              ({Module, Args}) ->
                ?CHILD(Module, Args, worker, 5000)
            end, Servers).

