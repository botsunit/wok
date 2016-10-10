% @hidden
-module(wok_utils).

-export([
         consumer_group/0,
         local_consumer_group/0
        ]).

is_registered(What) ->
  case ets:info(wok_configuration) of
    undefined ->
      ets:new(wok_configuration, [public, named_table]),
      false;
    _ ->
      case ets:lookup(wok_configuration, What) of
        [Tuple] ->
          Tuple;
        _ ->
          false
      end
  end.

consumer_group() ->
  cg(consumer_group, undefined).

local_consumer_group() ->
  cg(local_consumer_group, consumer_group()).

cg(Type, Default) ->
  case is_registered(Type) of
    false ->
      case doteki:get_env([wok, messages, Type], Default) of
        random ->
          store(Type, generate_random_cg({random, []}));
        {random} ->
          store(Type, generate_random_cg({random, []}));
        Tuple when is_tuple(Tuple) ->
          store(Type, generate_random_cg(Tuple));
        Other when is_list(Other);
                   is_binary(Other) ->
          store(Type, bucs:to_binary(Other));
        _ ->
          store(Type, undefined)
      end;
    {Type, CG} ->
      CG
  end.

generate_random_cg({random, Options}) ->
  generate_random_cg(Options);
generate_random_cg({Options, random}) ->
  generate_random_cg(Options);
generate_random_cg(Options) when is_list(Options) ->
  <<(case lists:keyfind(prefix, 1, Options) of
       {prefix, Prefix} ->
         <<(bucs:to_binary(Prefix))/binary, "_">>;
       _ ->
         <<>>
     end)/binary,
    (bucs:to_binary(uuid:to_string(uuid:uuid4())))/binary>>;
generate_random_cg(_) ->
  undefined.

store(Key, Value) ->
  ets:insert(wok_configuration, {Key, Value}), Value.
