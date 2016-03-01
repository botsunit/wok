# Wok Views helpers

## render/2

The `render` function does the heavy lifting of rendering your application's content for use by a browser. 

```erlang
wok_views:render(State :: term(), View :: atom()).
```

```elixir
Wok.Views.render(state :: term(), view :: atom()).
```

This function takes two parameters :

* The handler state.
* The template module name to render.

```erlang
-module(my_rest_handler).

-export([show/2]).

show(_WokReq, State) ->
  % Do some stuff...
  wok_views:render(State, show_tmpl).
```

```elixir
defmodule MyRestHandler do
  def show(wok_req, state) do
    # Do some stuff...
    Wok.Views.render(state, :show_tmpl)
  end
end
```

## render/3

The `render` function does the heavy lifting of rendering your application's content for use by a browser. 

```erlang
wok_views:render(State :: term(), Code :: integer(), View :: atom()).
wok_views:render(State :: term(), View :: atom(), Data :: map() | [tuple()]).
wok_views:render(State :: term(), Headers :: map() | [tuple()], View :: atom()).
```

```elixir
Wok.Views.render(state :: term(), code :: integer(), view :: atom()).
Wok.Views.render(state :: term(), view :: atom(), data :: map() | [tuple()]).
Wok.Views.render(state :: term(), headers :: map() | [tuple()], view :: atom()).
```

This function takes three parameters :

* The handler state.
* The response HTTP code.
* The template module name to render.

or:

* The handler state.
* The template module name to render.
* The datas used in the template.

or:

* The handler state.
* The response headers.
* The template module name to render.

```erlang
-module(my_rest_handler).

-export([show/2]).

show(_WokReq, State) ->
  % Do some stuff...
  wok_views:render(State, show_tmpl, #{firstname => "John",
                                       lastname => "Doe"}).
```

```elixir
defmodule MyRestHandler do
  def show(wok_req, state) do
    # Do some stuff...
    Wok.Views.render(state, :show_tmpl, %{firstname: "John",
                                          lastname: "Doe"))
  end
end
```

## render/4

The `render` function does the heavy lifting of rendering your application's content for use by a browser. 

```erlang
wok_views:render(State :: term(), Code :: integer(), View :: atom(), Data :: map() | [tuple()]).
wok_views:render(State :: term(), Headers :: map() | [tuple()], View :: atom(), Data :: map() | [tuple()]).
wok_views:render(State :: term(), Code :: integer(), Headers :: map() | [tuple()], View :: atom()).
```

```elixir
Wok.Views.render(State :: term(), Code :: integer(), View :: atom(), Data :: map() | [tuple()]).
Wok.Views.render(State :: term(), Headers :: map() | [tuple()], View :: atom(), Data :: map() | [tuple()]).
Wok.Views.render(State :: term(), Code :: integer(), Headers :: map() | [tuple()], View :: atom()).
```

This function takes four parameters :

* The handler state.
* The response HTTP code.
* The template module name to render.
* The datas used in the template.

or:

* The handler state.
* The response headers.
* The template module name to render.
* The datas used in the template.

or:

* The handler state.
* The response HTTP code.
* The response headers.
* The template module name to render.

```erlang
-module(my_rest_handler).

-export([show/2]).

show(_WokReq, State) ->
  % Do some stuff...
  wok_views:render(State, 
                   #{content-type => "text/html"},
                   show_tmpl, 
                   #{firstname => "John",
                     lastname => "Doe"}).
```

```elixir
defmodule MyRestHandler do
  def show(wok_req, state) do
    # Do some stuff...
    Wok.Views.render(state, 
                     %{"content-type": "text/html"},
                     :show_tmpl, 
                     %{firstname: "John",
                       lastname: "Doe"))
  end
end
```

## render/5

The `render` function does the heavy lifting of rendering your application's content for use by a browser. 

```erlang
wok_views:render(State :: term(), 
                 Code :: integer(), 
                 Headers :: map() | [tuple()], 
                 View :: atom(), 
                 Data :: map() | [tuple()]).
```

```elixir
Wok.Views.render(state :: term(), 
                 code :: integer(), 
                 headers :: map() | [tuple()], 
                 view :: atom(), 
                 data :: map() | [tuple()]).
```

This function takes four parameters :

* The handler state.
* The response HTTP code.
* The response headers.
* The template module name to render.
* The datas used in the template.

```erlang
-module(my_rest_handler).

-export([show/2]).

show(_WokReq, State) ->
  % Do some stuff...
  wok_views:render(State, 
                   200,
                   #{content-type => "text/html"},
                   show_tmpl, 
                   #{firstname => "John",
                     lastname => "Doe"}).
```

```elixir
defmodule MyRestHandler do
  def show(wok_req, state) do
    # Do some stuff...
    Wok.Views.render(state, 
                     200,
                     %{"content-type": "text/html"},
                     :show_tmpl, 
                     %{firstname: "John",
                       lastname: "Doe"))
  end
end
```

