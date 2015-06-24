% @hidden
-module(wok_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(CHILD(I, Type, Shutdown), {I, {I, start_link, []}, permanent, Shutdown, Type, [I]}).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  Childs = case wok_config:conf([wok, messages]) of
             undefined ->
               [];
             _ ->
               [?CHILD(wok_messages_sup, supervisor, infinity)]
           end ++ case wok_config:conf([wok, rest]) of
                    undefined ->
                      [];
                    _ ->
                      [?CHILD(wok_rest_sup, supervisor, infinity)]
                  end,
  {ok, {
     {one_for_one, 5, 10},
     Childs
    }
  }.

