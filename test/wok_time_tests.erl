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
      , ?_test(t_next())
      , ?_test(t_next_stop())
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

t_next() ->
  ?assertMatch({ok, {{2000, 1, 1}, {0, 0, 1}}, 1}, 
               wok_time:next({2000, '*', '*', '*', '*', '*'},
                             {2000, 1, 1, 0, 0, 0})),
  ?assertMatch({ok, {{2000, 1, 3}, {0, 0, 0}}, 60*60*24*2}, 
               wok_time:next({2000, '*', monday, '*', '*', '*'},
                             {2000, 1, 1, 0, 0, 0})),
  ?assertMatch({ok, {{2000, 1, 1}, {0, 0, 1}}, 1}, 
               wok_time:next({2000, '*', [saturday, monday], '*', '*', '*'},
                             {2000, 1, 1, 0, 0, 0})),
  ?assertMatch({ok, {{2000, 1, 1}, {0, 0, 1}}, 1}, 
               wok_time:next({2000, 1, 1, 0, 0, 1},
                             {2000, 1, 1, 0, 0, 0})),
  ?assertMatch({ok, {{2000, 1, 1}, {0, 1, 0}}, 60}, 
               wok_time:next({2000, 1, 1, 0, 1, 0},
                             {2000, 1, 1, 0, 0, 0})),
  ?assertMatch({ok, {{2000, 1, 1}, {1, 0, 0}}, 60*60}, 
               wok_time:next({2000, 1, 1, 1, 0, 0},
                             {2000, 1, 1, 0, 0, 0})),
  ?assertMatch({ok, {{2000, 1, 2}, {0, 0, 0}}, 60*60*24}, 
               wok_time:next({2000, 1, 2, 0, 0, 0},
                             {2000, 1, 1, 0, 0, 0})),
  ?assertMatch({ok, {{2000, 2, 1}, {0, 0, 0}}, 60*60*24*31}, 
               wok_time:next({2000, 2, 1, 0, 0, 0},
                             {2000, 1, 1, 0, 0, 0})),
  ?assertMatch({ok, {{2000, 3, 1}, {0, 0, 0}}, 60*60*24*(31+29)}, 
               wok_time:next({2000, 3, 1, 0, 0, 0},
                             {2000, 1, 1, 0, 0, 0})),
  ?assertMatch({ok, {{2001, 3, 1}, {0, 0, 0}}, 60*60*24*(31+28)}, 
               wok_time:next({2001, 3, 1, 0, 0, 0},
                             {2001, 1, 1, 0, 0, 0})),
  ?assertMatch({ok, {{2000, 1, 1}, {0, 0, 5}}, 5}, 
               wok_time:next({2000, '*', '*', '*', '*', '*/5'},
                             {2000, 1, 1, 0, 0, 0})),
  ?assertMatch({ok, {{2000, 1, 1}, {0, 5, 0}}, 5*60}, 
               wok_time:next({2000, '*', '*', '*', '*/5', '*'},
                             {2000, 1, 1, 0, 0, 0})),
  ?assertMatch({ok, {{2000, 1, 1}, {5, 0, 0}}, 5*60*60}, 
               wok_time:next({2000, '*', '*', '*/5', '*', '*'},
                             {2000, 1, 1, 0, 0, 0})),
  ?assertMatch({ok, {{2000, 1, 6}, {0, 0, 0}}, 5*60*60*24}, 
               wok_time:next({2000, '*', '*/5', '*', '*', '*'},
                             {2000, 1, 1, 0, 0, 0})),
   ?assertMatch({ok, {{2000, 1, 17}, {0, 0, 0}}, 16*60*60*24}, 
                wok_time:next({2000, '*', '*/16', '*', '*', '*'},
                              {2000, 1, 1, 0, 0, 0})),
  ?assertMatch({ok, {{2000, 6, 1}, {0, 0, 0}}, (31+29+31+30+31)*60*60*24}, 
               wok_time:next({2000, '*/5', '*', '*', '*', '*'},
                             {2000, 1, 1, 0, 0, 0})),
  ?assertMatch({ok, {{2002, 1, 1}, {0, 0, 0}}, _}, 
               wok_time:next({'*/2', '*', '*', '*', '*', '*'},
                             {2000, 1, 1, 0, 0, 0})),
  ?assertMatch({ok, _, 1}, wok_time:next({'*', '*', '*', '*', '*', '*'})).

t_next_stop() ->
  ?assertMatch(stop, wok_time:next({2000, '*', '*', '*', '*', '*'})).

