-module(wok_cowboy_req_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("bucs/include/bucassert.hrl").
-include_lib("wok_http_adapter/include/wok_req.hrl").
-include("../include/wok.hrl").

wok_r() ->
  cowboy_req:new(
    undefined, % Socket
    dummy_transport, % Transport
    undefined, % Peer
    <<"GET">>, % Method
    <<"/test/path">>, % Path
    <<"?a=b&c=d">>, % Query
    'HTTP/1.1', % Version
    [{<<"X-Wok-Test">>, <<"true">>},
     {<<"content-type">>, <<"multipart/form-data; boundary=--aabbcc">>}], % Headers
    <<"botsunit.com">>, % Host
    8080, % Port
    <<>>, % Buffer
    true, % CanKeepalive
    false, % Compress
    undefined % OnResponse
   ).

meck_cowboy_req() ->
  meck:new(cowboy_req, [passthrough]),
  meck:expect(cowboy_req, part, 1, meck:seq([fun(R) -> {ok, [], R} end,
                                             fun(R) -> {done, R} end])),
  meck:expect(cowboy_req, part_body, 1, meck:seq([fun(R) -> {more, <<"hello ">>, R} end,
                                                  fun(R) -> {ok, <<"world">>, R} end])).

meck_cow_multipart() ->
  meck:new(cow_multipart, []),
  meck:expect(cow_multipart, form_data,
    fun(_Headers) -> {file, <<"file">>, <<"dummy_filename.txt">>, <<"text/plain">>, ""} end).

wok_file_upload_test_() ->
  {setup,
   fun() ->
    meck_cowboy_req(),
    meck_cow_multipart(),
    wok_r()
   end,
   fun(_) ->
    meck:unload()
   end,
   fun(R) ->
       {with, R,
        [
         fun(CR) ->
             ?assertContinueIfMatch(
                {ok, [], [{<<"file">>, <<"text/plain">>, File}], _, _},
                wok_cowboy_req:post_values(CR),
                File,
                fun(F) ->
                    ?assertEqual(
                       <<"dummy_filename.txt">>,
                       filename:basename(F)),
                    ?assertEqual(
                       {ok, <<"hello world">>},
                       file:read_file(F)),
                    _ = bucfile:remove_recursive(filename:dirname(F))
                end)
         end
        ]}
   end}.

