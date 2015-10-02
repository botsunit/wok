-module(wok_time_tests).

-include_lib("eunit/include/eunit.hrl").

wok_time_test_() ->
  {setup,
   fun setup/0,
   fun teardown/1,
   [
      ?_test(t_verify_year())
      , ?_test(t_verify_month())
      , ?_test(t_verify_day())
      , ?_test(t_verify_hour())
      , ?_test(t_verify_minute())
      , ?_test(t_verify_second())
   ]}.

setup() ->
  ok.

teardown(_) ->
  ok.

t_verify_year() ->
  ?assertMatch({error, year}, wok_time:verify({error, '*', '*', '*', '*', '*'})),
  ?assertMatch({error, year}, wok_time:verify({<<"error">>, '*', '*', '*', '*', '*'})),
  ?assertMatch({error, year}, wok_time:verify({-1, '*', '*', '*', '*', '*'})),
  ?assertMatch({error, year}, wok_time:verify({'*/x', '*', '*', '*', '*', '*'})),
  ?assertMatch({error, year}, wok_time:verify({'x', '*', '*', '*', '*', '*'})),
  ?assertMatch({error, year}, wok_time:verify({'x/*', '*', '*', '*', '*', '*'})),
  ?assertMatch(ok, wok_time:verify({2000, '*', '*', '*', '*', '*'})),
  ?assertMatch(ok, wok_time:verify({[2000, 2002], '*', '*', '*', '*', '*'})),
  ?assertMatch(ok, wok_time:verify({'*/2', '*', '*', '*', '*', '*'})),
  ?assertMatch(ok, wok_time:verify({'*', '*', '*', '*', '*', '*'})).

t_verify_month() ->
  ?assertMatch({error, month}, wok_time:verify({'*', error, '*', '*', '*', '*'})),
  ?assertMatch({error, month}, wok_time:verify({'*', <<"error">>, '*', '*', '*', '*'})),
  ?assertMatch({error, month}, wok_time:verify({'*', 0, '*', '*', '*', '*'})),
  ?assertMatch({error, month}, wok_time:verify({'*', 13, '*', '*', '*', '*'})),
  ?assertMatch({error, month}, wok_time:verify({'*', '*/x', '*', '*', '*', '*'})),
  ?assertMatch({error, month}, wok_time:verify({'*', 'x', '*', '*', '*', '*'})),
  ?assertMatch({error, month}, wok_time:verify({'*', 'x/*', '*', '*', '*', '*'})),
  ?assertMatch(ok, wok_time:verify({'*', [1, 3], '*', '*', '*', '*'})),
  ?assertMatch(ok, wok_time:verify({'*', 3, '*', '*', '*', '*'})),
  ?assertMatch(ok, wok_time:verify({'*', '*/2', '*', '*', '*', '*'})),
  ?assertMatch(ok, wok_time:verify({'*', '*', '*', '*', '*', '*'})).

t_verify_day() ->
  ?assertMatch({error, day}, wok_time:verify({'*', '*', error, '*', '*', '*'})),
  ?assertMatch({error, day}, wok_time:verify({'*', '*', <<"error">>, '*', '*', '*'})),
  ?assertMatch({error, day}, wok_time:verify({'*', '*', 0, '*', '*', '*'})),
  ?assertMatch({error, day}, wok_time:verify({'*', '*', 32, '*', '*', '*'})),
  ?assertMatch({error, day}, wok_time:verify({'*', '*', '*/x', '*', '*', '*'})),
  ?assertMatch({error, day}, wok_time:verify({'*', '*', 'x', '*', '*', '*'})),
  ?assertMatch({error, day}, wok_time:verify({'*', '*', 'x/*', '*', '*', '*'})),
  ?assertMatch(ok, wok_time:verify({'*', '*', monday, '*', '*', '*'})),
  ?assertMatch(ok, wok_time:verify({'*', '*', thursday, '*', '*', '*'})),
  ?assertMatch(ok, wok_time:verify({'*', '*', wednesday, '*', '*', '*'})),
  ?assertMatch(ok, wok_time:verify({'*', '*', thursday, '*', '*', '*'})),
  ?assertMatch(ok, wok_time:verify({'*', '*', friday, '*', '*', '*'})),
  ?assertMatch(ok, wok_time:verify({'*', '*', saturday, '*', '*', '*'})),
  ?assertMatch(ok, wok_time:verify({'*', '*', sunday, '*', '*', '*'})),
  ?assertMatch(ok, wok_time:verify({'*', '*', [monday, sunday], '*', '*', '*'})),
  ?assertMatch(ok, wok_time:verify({'*', '*', [3, 10], '*', '*', '*'})),
  ?assertMatch(ok, wok_time:verify({'*', '*', 3, '*', '*', '*'})),
  ?assertMatch(ok, wok_time:verify({'*', '*', '*/2', '*', '*', '*'})),
  ?assertMatch(ok, wok_time:verify({'*', '*', '*', '*', '*', '*'})).

t_verify_hour() ->
  ?assertMatch({error, hour}, wok_time:verify({'*', '*', '*', error, '*', '*'})),
  ?assertMatch({error, hour}, wok_time:verify({'*', '*', '*', <<"error">>, '*', '*'})),
  ?assertMatch({error, hour}, wok_time:verify({'*', '*', '*', -1, '*', '*'})),
  ?assertMatch({error, hour}, wok_time:verify({'*', '*', '*', 24, '*', '*'})),
  ?assertMatch({error, hour}, wok_time:verify({'*', '*', '*', '*/x', '*', '*'})),
  ?assertMatch({error, hour}, wok_time:verify({'*', '*', '*', 'x', '*', '*'})),
  ?assertMatch({error, hour}, wok_time:verify({'*', '*', '*', 'x/*', '*', '*'})),
  ?assertMatch(ok, wok_time:verify({'*', '*', '*', [3, 7], '*', '*'})),
  ?assertMatch(ok, wok_time:verify({'*', '*', '*', 2, '*', '*'})),
  ?assertMatch(ok, wok_time:verify({'*', '*', '*', '*/2', '*', '*'})),
  ?assertMatch(ok, wok_time:verify({'*', '*', '*', '*', '*', '*'})).

t_verify_minute() ->
  ?assertMatch({error, minute}, wok_time:verify({'*', '*', '*', '*', error, '*'})),
  ?assertMatch({error, minute}, wok_time:verify({'*', '*', '*', '*', <<"error">>, '*'})),
  ?assertMatch({error, minute}, wok_time:verify({'*', '*', '*', '*', -1, '*'})),
  ?assertMatch({error, minute}, wok_time:verify({'*', '*', '*', '*', 60, '*'})),
  ?assertMatch({error, minute}, wok_time:verify({'*', '*', '*', '*', '*/x', '*'})),
  ?assertMatch({error, minute}, wok_time:verify({'*', '*', '*', '*', 'x', '*'})),
  ?assertMatch({error, minute}, wok_time:verify({'*', '*', '*', '*', 'x/*', '*'})),
  ?assertMatch(ok, wok_time:verify({'*', '*', '*', '*', [3, 7], '*'})),
  ?assertMatch(ok, wok_time:verify({'*', '*', '*', '*', 2, '*'})),
  ?assertMatch(ok, wok_time:verify({'*', '*', '*', '*', '*/2', '*'})),
  ?assertMatch(ok, wok_time:verify({'*', '*', '*', '*', '*', '*'})).

t_verify_second() ->
  ?assertMatch({error, second}, wok_time:verify({'*', '*', '*', '*', '*', error})),
  ?assertMatch({error, second}, wok_time:verify({'*', '*', '*', '*', '*', <<"error">>})),
  ?assertMatch({error, second}, wok_time:verify({'*', '*', '*', '*', '*', -1})),
  ?assertMatch({error, second}, wok_time:verify({'*', '*', '*', '*', '*', 60})),
  ?assertMatch({error, second}, wok_time:verify({'*', '*', '*', '*', '*', '*/x'})),
  ?assertMatch({error, second}, wok_time:verify({'*', '*', '*', '*', '*', 'x'})),
  ?assertMatch({error, second}, wok_time:verify({'*', '*', '*', '*', '*', 'x/*'})),
  ?assertMatch(ok, wok_time:verify({'*', '*', '*', '*', '*', [3, 7]})),
  ?assertMatch(ok, wok_time:verify({'*', '*', '*', '*', '*', 2})),
  ?assertMatch(ok, wok_time:verify({'*', '*', '*', '*', '*', '*/2'})),
  ?assertMatch(ok, wok_time:verify({'*', '*', '*', '*', '*', '*'})).

