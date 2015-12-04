% @hidden
-module(wok_topics_sup).
-compile([{parse_transform, lager_transform}]).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(CHILD(ID, I, Type, Args), {ID, {I, start_link, Args}, permanent, 2000, Type, [I]}).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  Childs = case doteki:get_env([wok, messages, topics]) of
             undefined ->
               lager:debug("No topic declared in config!"),
               [];
             Topics ->
               build_childs(Topics)
           end,
  {ok, {
     {one_for_one, 5, 10},
     Childs
    }
  }.

build_childs(Topics) ->
  build_childs(Topics, []).

build_childs([], Childs) ->
  Childs;
build_childs([{Name, Options}|Rest], Childs) ->
  build_childs(Rest,
               [?CHILD(bucs:to_atom(Name), wok_topic, worker, [Name, Options])|
                Childs]).

