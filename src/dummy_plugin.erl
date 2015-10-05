% @hidden
-module(dummy_plugin).
-compile([{parse_transform, lager_transform}]).

-export([init/1]).
-export([foo/1, bar/1, baz/1]).

init(Args) ->
  lager:info("Initialize dummy_plugin ~p", [?MODULE]),
  {ok, Args}. % {stop, Reason}

foo(State) ->
  {ok, State}.

bar([Topic, Data]) ->
  {send, Topic, {<<"dummy_plugin/bar">>, <<"service/controler/action">>, <<"Message body: ", (eutils:to_binary(Data))/binary>>}, [Topic, Data + 1]}.

baz(State) ->
  {stop, State}.
