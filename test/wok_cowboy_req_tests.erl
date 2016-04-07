-module(wok_cowboy_req_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("bucs/include/bucassert.hrl").
-include_lib("wok_http_adapter/include/wok_req.hrl").
-include("../include/wok.hrl").

wok_r() ->
  #wok_req{
    adapter = wok_cowboy_req,
    request = cowboy_req:new(
      undefined, % Socket
      dummy_transport, % Transport
      undefined, % Peer
      <<"GET">>, % Method
      <<"/test/path">>, % Path
      <<"?a=b&c=d">>, % Query
      'HTTP/1.1', % Version
      [{<<"X-Wok-Test">>, <<"true">>}], % Headers
      <<"botsunit.com">>, % Host
      8080, % Port
      <<>>, % Buffer
      true, % CanKeepalive
      true, % Compress
      undefined % OnResponse
    )}.

meck_cowboy_req() ->
  meck:new(cowboy_req, [passthrough]),
  meck:expect(cowboy_req, part, fun(CowboyReq) -> {ok, [], CowboyReq} end),
  meck:sequence(cowboy_req, part_body, 1, [
                  {
                    more,
                    <<"hello ">>,
                    dummy_cowboy_req
                  },{
                    ok,
                    <<"world">>,
                    dummy_cowboy_req
                  }
                ]).

meck_cow_multipart() ->
  meck:new(cow_multipart, []),
  meck:expect(cow_multipart, form_data,
    fun(_Headers) -> {file, "", <<"dummy_filename">>, <<"text/plain">>, ""} end).

wok_file_upload_test_() ->
  {foreach,
   fun() ->
    meck_cowboy_req(),
    meck_cow_multipart(),
    wok_r()
   end,
   fun(_) ->
    meck:unload()
   end,
   [ {with, [T]} || T <- [
            fun(Req) ->
              ?assertMatch({ok, <<"dummy_filename">>, <<"text/plain">>, <<"hello world">>, _}, wok_cowboy_req:get_file(Req))
            end,
            fun(Req) ->
              Function = fun(_, _, Data, Acc) -> {ok, <<Acc/binary, Data/binary>>} end,
              ?assertMatch({ok, <<"dummy_filename">>, <<"text/plain">>, <<"YOLOhello world">>, _}, wok_cowboy_req:get_file(Req, Function, <<"YOLO">>))
            end,
            fun(Req) ->
              TempFileName = tempfile:name("prefix_"),
              {ok, <<"dummy_filename">>, <<"text/plain">>, TempFileName, _} = wok_cowboy_req:get_file(Req, TempFileName),
              {ok, FileData} = file:read_file(TempFileName),
              ?assertMatch(<<"hello world">>, FileData)
            end,
            fun(Req) ->
                TempFileName = tempfile:name("prefix_"),
                {ok, FilePid} = file:open(TempFileName, [append]),
                {ok, <<"dummy_filename">>, <<"text/plain">>, FilePid, _} = wok_cowboy_req:get_file(Req, FilePid),
                file:close(FilePid),
                {ok, FileData} = file:read_file(TempFileName),
                ?assertMatch(<<"hello world">>, FileData)
            end
                      ]
    ]
  }.

