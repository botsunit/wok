# APIs

## Message

### provide/2

### provide/4

### provide/5

## HTTP Request

### custom_data/1

### custom_data/2

### client_ip/1

### client_port/1

### body/1

### method/1

### param/2

### param/3

### params/1

### params/2

### path/1

### header/2

### header/3

### cookies/1

### cookie/2

### local_state/1

### local_state/2

### global_state/1

### global_state/2

## HTTP Response

### render/2

The `render` function does the heavy lifting of rendering your application's content for use by a browser. 

```erlang
wok_response:render(Req :: wok_req(), 
                    View :: atom() | string() | binary()).
```

```elixir
Wok.Response.render(req :: wok_req(), 
                    view :: atom() | string() | binary()).
```

This function takes two parameters :

* The opaque request
* The template module name to render.

```erlang
-module(my_rest_handler).

-export([show/2]).

show(_WokReq, State) ->
  % Do some stuff...
  wok_response:render(State, show_tmpl).
```

```elixir
defmodule MyRestHandler do
  def show(wok_req, state) do
    # Do some stuff...
    Wok.Response.render(state, :show_tmpl)
  end
end
```

### render/3

The `render` function does the heavy lifting of rendering your application's content for use by a browser. 

```erlang
wok_response:render(Req :: wok_req(), 
                    Code :: integer(), 
                    View :: atom() | string() | binary()).
wok_response:render(Req :: wok_req(), 
                    View :: atom() | string() | binary(), 
                    Data :: map() | [tuple()]).
wok_response:render(Req :: wok_req(), 
                    Headers :: map() | [tuple()], 
                    View :: atom() | string() | binary()).
```

```elixir
Wok.Response.render(req :: wok_req(), 
                    code :: integer(), 
                    view :: atom() | string() | binary()).
Wok.Response.render(req :: wok_req(), 
                    view :: atom() | string() | binary(), 
                    data :: map() | [tuple()]).
Wok.Response.render(req :: wok_req(), 
                    headers :: map() | [tuple()], 
                    view :: atom() | string() | binary()).
```

This function takes three parameters :

* The opaque request
* The response HTTP code.
* The template module name to render.

or:

* The opaque request
* The template module name to render.
* The datas used in the template.

or:

* The opaque request
* The response headers.
* The template module name to render.

```erlang
-module(my_rest_handler).

-export([show/2]).

show(_WokReq, State) ->
  % Do some stuff...
  wok_response:render(State, show_tmpl, #{firstname => "John",
                                          lastname => "Doe"}).
```

```elixir
defmodule MyRestHandler do
  def show(wok_req, state) do
    # Do some stuff...
    Wok.Response.render(state, :show_tmpl, %{firstname: "John",
                                             lastname: "Doe"))
  end
end
```

### render/4

The `render` function does the heavy lifting of rendering your application's content for use by a browser. 

```erlang
wok_response:render(Req :: wok_req(), 
                    Code :: integer(), 
                    View :: atom() | string() | binary(), 
                    Data :: map() | [tuple()]).
wok_response:render(Req :: wok_req(), 
                    Headers :: map() | [tuple()], 
                    View :: atom() | string() | binary(), 
                    Data :: map() | [tuple()]).
wok_response:render(Req :: wok_req(), 
                    Code :: integer(), 
                    Headers :: map() | [tuple()], 
                    View :: atom() | string() | binary()).
```

```elixir
Wok.Response.render(Req :: wok_req(), 
                    Code :: integer(), 
                    View :: atom() | string() | binary(), 
                    Data :: map() | [tuple()]).
Wok.Response.render(Req :: wok_req(), 
                    Headers :: map() | [tuple()], 
                    View :: atom() | string() | binary(), 
                    Data :: map() | [tuple()]).
Wok.Response.render(Req :: wok_req(), 
                    Code :: integer(), 
                    Headers :: map() | [tuple()], 
                    View :: atom() | string() | binary()).
```

This function takes four parameters :

* The opaque request
* The response HTTP code.
* The template module name to render.
* The datas used in the template.

or:

* The opaque request
* The response headers.
* The template module name to render.
* The datas used in the template.

or:

* The opaque request
* The response HTTP code.
* The response headers.
* The template module name to render.

```erlang
-module(my_rest_handler).

-export([show/2]).

show(_WokReq, State) ->
  % Do some stuff...
  wok_response:render(State, 
                      #{content-type => "text/html"},
                      show_tmpl, 
                      #{firstname => "John",
                        lastname => "Doe"}).
```

```elixir
defmodule MyRestHandler do
  def show(wok_req, state) do
    # Do some stuff...
    Wok.Response.render(state, 
                        %{"content-type": "text/html"},
                        :show_tmpl, 
                        %{firstname: "John",
                          lastname: "Doe"))
  end
end
```

### render/5

The `render` function does the heavy lifting of rendering your application's content for use by a browser. 

```erlang
wok_response:render(Req :: wok_req(), 
                    Code :: integer(), 
                    Headers :: map() | [tuple()], 
                    View :: atom() | string() | binary(), 
                    Data :: map() | [tuple()]).
```

```elixir
Wok.Response.render(req :: wok_req(), 
                    code :: integer(), 
                    headers :: map() | [tuple()], 
                    view :: atom() | string() | binary(), 
                    data :: map() | [tuple()]).
```

This function takes four parameters :

* The opaque request
* The response HTTP code.
* The response headers.
* The template module name to render.
* The datas used in the template.

```erlang
-module(my_rest_handler).

-export([show/2]).

show(_WokReq, State) ->
  % Do some stuff...
  wok_response:render(State, 
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
    Wok.Response.render(state, 
                        200,
                        %{"content-type": "text/html"},
                        :show_tmpl, 
                        %{firstname: "John",
                          lastname: "Doe"))
  end
end
```

### redirect/2

### set_cookie/3

### set_cookie/4

Otions = [{max_age, non_neg_integer()} | {domain, binary()} | {path, binary()} | {secure, boolean()} | {http_only, boolean()}]

### delete_cookie/2

### set_response/2

### set_headers/2

### merge_headers/2

