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
               [?CHILD(wok_messages_sup, [], supervisor, infinity)]
           end ++
           case doteki:get_env([wok, rest]) of
             undefined ->
               [];
             _ ->
               [?CHILD(wok_rest_sup, [], supervisor, infinity)]
           end,
  {ok, {
     {one_for_one, 5, 10},
     Childs
    }
  }.

