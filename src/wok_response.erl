-module(wok_response).
-include("../include/wok.hrl").

-export([
         render/2,
         render/3,
         render/4,
         render/5,
         redirect/2,
         set_cookie/3,
         set_cookie/4,
         delete_cookie/2,
         set_response/2,
         set_headers/2,
         merge_headers/2
        ]).

-define(DEFAULT_HEADERS, [{<<"content-type">>, <<"text/html">>}]).
-define(DEFAULT_CODE, 200).

% @doc
% render(Req, 200, [{&lt;&lt;"content-type"&gt;&gt;, &lt;&lt;"text/html"&gt;&gt;}], View, [])
% @end
render(Req, View) when (is_atom(View) orelse is_list(View) orelse is_binary(View)) ->
  render(Req, ?DEFAULT_CODE, ?DEFAULT_HEADERS, View, []).

% @doc
% render(Req, Code, [{&lt;&lt;"content-type"&gt;&gt;, &lt;&lt;"text/html"&gt;&gt;}], View, [])
% render(Req, 200, Headers, View, [])
% render(Req, 200, [{&lt;&lt;"content-type"&gt;&gt;, &lt;&lt;"text/html"&gt;&gt;}], View, Data)
% @end
render(Req, Code, View) when is_integer(Code),
                             (is_atom(View) orelse is_list(View) orelse is_binary(View)) ->
  render(Req, Code, ?DEFAULT_HEADERS, View, []);
render(Req, Headers, View) when (is_list(Headers) orelse is_map(Headers)),
                                (is_atom(View) orelse is_list(View) orelse is_binary(View)) ->
  render(Req, ?DEFAULT_CODE, Headers, View, []);
render(Req, View, Data) when (is_atom(View) orelse is_list(View) orelse is_binary(View)),
                             (is_list(Data) orelse is_map(Data)) ->
  render(Req, ?DEFAULT_CODE, ?DEFAULT_HEADERS, View, Data).

% @doc
% render(Req, 200, Headers, View, Data)
% render(Req, Code, [{&lt;&lt;"content-type"&gt;&gt;, &lt;&lt;"text/html"&gt;&gt;}], View, Data)
% render(Req, Code, Headers, View, [])
% @end
render(Req, Headers, View, Data) when (is_list(Headers) orelse is_map(Headers)),
                                      (is_atom(View) orelse is_list(View) orelse is_binary(View)),
                                      (is_list(Data) orelse is_map(Data)) ->
  render(Req, ?DEFAULT_CODE, Headers, View, Data);
render(Req, Code, View, Data) when is_integer(Code),
                                   (is_atom(View) orelse is_list(View) orelse is_binary(View)),
                                   (is_list(Data) orelse is_map(Data)) ->
  render(Req, Code, ?DEFAULT_HEADERS, View, Data);
render(Req, Code, Headers, View) when is_integer(Code),
                                      (is_list(Headers) orelse is_map(Headers)),
                                      (is_atom(View) orelse is_list(View) orelse is_binary(View)) ->
  render(Req, Code, Headers, View, []).

% @doc
% Generate a reponse with the given view.
%
% Example :
%
% <code>
% wok_response:render(Req, "login.html.tmpl", #{error => "Wrong login or password"}).
% </code>
% @end
render(Req, Code, Headers, View, Data) when is_integer(Code),
                                            (is_list(Headers) orelse is_map(Headers)),
                                            (is_atom(View) orelse is_list(View) orelse is_binary(View)),
                                            (is_list(Data) orelse is_map(Data)) ->
  Result = case template_engine(View) of
             {ok, TemplateEngine} ->
               erlang:apply(TemplateEngine, render, [Code, Headers, View, Data]);
             error ->
               {500, [], <<"Template engine not found">>}
           end,
  set_response(Req, Result).

% @doc
% Redirect
%
% Example :
%
% <code>
% wok_response:redirect(Req, "/logout").
% wok_response:redirect(Req, {handler, fun}).
% wok_response:redirect(Req, {handler, fun, #{id => 1, name => &lt;&lt;"John"&gt;&gt;}}).
% </code>
% @end
redirect(Req, Path) when is_list(Path) orelse is_binary(Path) ->
  set_response(Req, {302, [{<<"Location">>, bucs:to_binary(Path)}], <<>>});
redirect(Req, {Handler, Function}) ->
  redirect(Req, wok_routes:path(Handler, Function));
redirect(Req, {Handler, Function, Args}) ->
  redirect(Req, wok_routes:path(Handler, Function, Args)).

% @equiv set_cookie(Req, Name, Value, [])
set_cookie(Req, Name, Value) ->
  set_cookie(Req, Name, Value, []).

% @doc
% Otions = [{max_age, non_neg_integer()} | {domain, binary()} | {path, binary()} | {secure, boolean()} | {http_only, boolean()}]
% @end
set_cookie(Req, Name, Value, Options) ->
  wok_req:set_cookie(Req, Name, Value, Options).

% @doc
% @end
delete_cookie(Req, Name) ->
  set_cookie(Req, Name, <<"">>, [{max_age, 0}]).

% @doc
% @end
set_response(Req, Response) ->
  wok_req:set_response(Req, Response).

set_headers(Req, Headers) ->
  wok_req:set_response_headers(Req, Headers).

merge_headers(Req, Headers) ->
  wok_req:set_response_headers(
    Req,
    buclists:merge_keylists(
      1,
      Headers,
      wok_req:get_response_headers(Req))).

% private

template_engine(View) when is_list(View) ->
  [Ext|_] = lists:reverse(string:tokens(View, "._")),
  Engine = bucs:to_atom("wok_" ++ Ext ++ "_engine"),
  case code:ensure_loaded(Engine) of
    {module, Engine} ->
      {ok, Engine};
    _ ->
      error
  end;
template_engine(View) ->
  template_engine(bucs:to_string(View)).

