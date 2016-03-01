% @hidden
-module(wok_views).

-export([render/2, render/3, render/4, render/5]).

-define(DEFAULT_HEADERS, [{<<"content-type">>, <<"text/html">>}]).
-define(DEFAULT_CODE, 200).

render(State, View) when (is_atom(View) orelse is_list(View) orelse is_binary(View)) ->
  render(State, ?DEFAULT_CODE, ?DEFAULT_HEADERS, View, []).

render(State, Code, View) when is_integer(Code),
                               (is_atom(View) orelse is_list(View) orelse is_binary(View)) ->
  render(State, Code, ?DEFAULT_HEADERS, View, []);
render(State, Headers, View) when (is_list(Headers) orelse is_map(Headers)),
                                  (is_atom(View) orelse is_list(View) orelse is_binary(View)) ->
  render(State, ?DEFAULT_CODE, Headers, View, []);
render(State, View, Data) when (is_atom(View) orelse is_list(View) orelse is_binary(View)),
                               (is_list(Data) orelse is_map(Data)) ->
  render(State, ?DEFAULT_CODE, ?DEFAULT_HEADERS, View, Data).

render(State, Headers, View, Data) when (is_list(Headers) orelse is_map(Headers)),
                                        (is_atom(View) orelse is_list(View) orelse is_binary(View)),
                                        (is_list(Data) orelse is_map(Data)) ->
  render(State, ?DEFAULT_CODE, Headers, View, Data);
render(State, Code, View, Data) when is_integer(Code),
                                     (is_atom(View) orelse is_list(View) orelse is_binary(View)),
                                     (is_list(Data) orelse is_map(Data)) ->
  render(State, Code, ?DEFAULT_HEADERS, View, Data);
render(State, Code, Headers, View) when is_integer(Code),
                                        (is_list(Headers) orelse is_map(Headers)),
                                        (is_atom(View) orelse is_list(View) orelse is_binary(View)) ->
  render(State, Code, Headers, View, []).

render(State, Code, Headers, View, Data) when is_integer(Code),
                                              (is_list(Headers) orelse is_map(Headers)),
                                              (is_atom(View) orelse is_list(View) orelse is_binary(View)),
                                              (is_list(Data) orelse is_map(Data)) ->
  View1 = template_module(View),
  case erlang_template(View1) of
    true ->
      wok_erlang_template_helpers:render(State, Code, Headers, View1, Data);
    Other -> % TODO : elixir template
      {500, [], bucs:to_binary(Other), State} % TODO
  end.

erlang_template(View) ->
  case code:ensure_loaded(wok_erlang_template_helpers) of
    {module, wok_erlang_template_helpers} ->
      case code:ensure_loaded(View) of
        {module, View} -> true;
        _ -> missing_template
      end;
    _ -> missing_template_helpers
  end.

template_module(Template) when is_atom(Template) ->
  Template;
template_module(Template) when is_list(Template);
                               is_binary(Template) ->
  bucs:pipecall([
                 {fun bucs:to_list/1, [Template]},
                 {fun re:replace/4, ["/", "_", [{return, list}, global]]},
                 {fun re:replace/4, ["\\.", "_", [{return, list}, global]]},
                 fun bucs:to_atom/1
                ]).
