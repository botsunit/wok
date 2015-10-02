% @hidden
-module(wok_time).

-export([verify/1, next/1, next/2]).

verify({_, _, _, _, _, _} = Spec) ->
  Exp = expand([element(I, Spec) || I <- lists:seq(1, tuple_size(Spec))]),
  Check = lists:append([[{U, N} || N <- L] ||
                        {U, L} <- lists:zip(units(), Exp)]),
  validate(Check).

next(Spec) ->
  next(Spec, n()).
next({_Year, _Month, _Day, _Hour, _Minute, _Second}, _From) ->
  ok.

n() ->
  {{Y, M, D}, {H, Mo, S}} = calendar:local_time(),
  {Y, M, D, H, Mo, S}.

expand(Spec) ->
  lists:map(fun
              (L) when is_list(L) -> lists:usort(L);
              (N) -> [N]
            end, Spec).

units() -> [year, month, day, hour, minute, second].

validate([]) -> ok;
validate([{year, Y}|Rest]) when is_integer(Y), Y > 0 -> validate(Rest);
validate([{month, M}|Rest]) when is_integer(M), M >= 1, M =< 12 -> validate(Rest);
validate([{day, D}|Rest]) when is_integer(D), D >= 1, D =< 31 -> validate(Rest);
validate([{day, monday}|Rest]) -> validate(Rest);
validate([{day, tuesday}|Rest]) -> validate(Rest);
validate([{day, wednesday}|Rest]) -> validate(Rest);
validate([{day, thursday}|Rest]) -> validate(Rest);
validate([{day, friday}|Rest]) -> validate(Rest);
validate([{day, saturday}|Rest]) -> validate(Rest);
validate([{day, sunday}|Rest]) -> validate(Rest);
validate([{hour, H}|Rest]) when is_integer(H), H >= 0,  H =< 23 -> validate(Rest);
validate([{minute, M}|Rest]) when is_integer(M), M >= 0,  M =< 59 -> validate(Rest);
validate([{second, S}|Rest]) when is_integer(S), S >= 0,  S =< 59 -> validate(Rest);
validate([{Type, Value}|Rest]) when is_atom(Value) -> 
  case binary:split(eutils:to_binary(Value), <<"/">>) of
    [<<"*">>] -> validate(Rest);
    [<<"*">>, Data] ->
      try eutils:to_integer(Data) of
        _ -> validate(Rest)
      catch 
        _:_ -> {error, Type}
      end;
    _ -> {error, Type}
  end;
validate([{Type, _}|_]) -> {error, Type}.
