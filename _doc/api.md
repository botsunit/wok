# Messages API

## provide/2

## provide/4

## provide/5

# HTTP Request API

## custom_data/1

This function allow you to get your custom data from the wok request opaque structure.

```erlang
wok_request:custom_data(Req :: wok_req()) -> any().
```

```elixir
Wok.Request.custom_data(req :: wok_req()) -> any()
```

## custom_data/2

This function allow you to store your custom data in the wok request opaque structure.

```erlang
wok_request:custom_data(Req :: wok_req(), Data :: any()) -> wok_req().
```

```elixir
Wok.Request.custom_data(req :: wok_req(), data :: any()) -> wok_req()
```

## client_ip/1

Return the client's IP address.

```erlang
wok_request:client_ip(Req :: wok_req()) -> inet:ip_address().
```

```elixir
Wok.Request.client_ip(req :: wok_req()) -> inet::ip_address()
```

## client_port/1

Return the client's port number.

```erlang
wok_request:client_port(Req :: wok_req()) -> inet:port_number().
```

```elixir
Wok.Request.client_port(req :: wok_req()) -> inet::port_number()
```

## body/1

Read the request body.

This function will read a chunk of the request body. If there is more data to be read after this function call, then a `more` tuple is returned. Otherwise an `ok` tuple is returned.

```erlang
wok_request:body(Req :: wok_req()) -> {ok, binary(), wok_req()}
                                      | {more, binary(), wok_req()}.
```

```elixir
Wok.Request.body(req :: wok_req()) -> {:ok, binary(), wok_req()}
                                      | {:more, binary(), wok_req()}
```

## method/1

Return the method.

Methods are case sensitive. Standard methods are always uppercase.

```erlang
wok_request:method(Req :: wok_req()) -> binary().
```

```elixir
Wok.Request.method(req :: wok_req()) -> binary()
```

## param/2

Return the value for the given parameter. 
The parameter is sought in the query string, in the body and in the bindings.

```erlang
wok_request:param(Req :: wok_req(), Param :: term()) -> {ok, binary(), wok_req()}
                                                        | {undefined, wok_req()}
                                                        | {error, wok_req()}.
```

```elixir
Wok.Request.param(req :: wok_req(), param :: term()) -> {:ok, binary(), wok_req()}
                                                          | {:undefined, wok_req()}
                                                          | {:error, wok_req()}
```

## param/3


Return the value for the given parameter and seek-domain (get | post | bind).

```erlang
wok_request:param(Req :: wok_req(), 
                  From :: get | post | bind,
                  Param :: term()) -> {ok, binary(), wok_req()}
                                      | {undefined, wok_req()}
                                      | {error, wok_req()}.
```

```elixir
Wok.Request.param(req :: wok_req(), 
                  from :: :get | :post | :bind,
                  param :: term()) -> {:ok, binary(), wok_req()}
                                      | {:undefined, wok_req()}
                                      | {:error, wok_req()}
```

## param/3 (2nd flavour)

Return the value for the given parameter.
The parameter is sought in the query string, in the body and in the bindings.
The given 'Default' value is returned if the parameter is not found anywhere in the request.

```erlang
wok_request:param(Req :: wok_req(), 
                  Param :: term(),
                  Default :: term()) -> {ok, term(), wok_req()}
                                        | {undefined, wok_req()}
                                        | {error, wok_req()}.
```

```elixir
Wok.Request.param(req :: wok_req(), 
                  param :: term(),
                  default :: term()) -> {:ok, term(), wok_req()}
                                        | {:undefined, wok_req()}
                                        | {:error, wok_req()}
```

## param/4

Return the value for the given parameter and seek-domain (get | post | bind).
The given 'Default' value is returned if the parameter is not found anywhere in the request.

```erlang
wok_request:param(Req :: wok_req(), 
                  From :: get | post | bind,
                  Param :: term(),
                  Default :: term()) -> {ok, binary(), wok_req()}
                                        | {undefined, wok_req()}
                                        | {error, wok_req()}.
```

```elixir
Wok.Request.param(req :: wok_req(), 
                  from :: :get | :post | :bind,
                  param :: term()
                  default :: term()) -> {:ok, binary(), wok_req()}
                                        | {:undefined, wok_req()}
                                        | {:error, wok_req()}
```

## params/1

Return all parameters.

```erlang
wok_request:params(Req :: wok_req()) -> {ok, [{binary(), binary()}], wok_req()}
                                        | {error, wok_req()}.
```

```elixir
Wok.Request.params(req :: wok_req()) -> {:ok, [{binary(), binary()}], wok_req()}
                                        | {:error, wok_req()}
```

## params/2

Return all parameters for the given domain.

```erlang
wok_request:params(Req :: wok_req(), 
                   From :: get | post | bind) -> {ok, [{binary(), binary()}], wok_req()}
                                                 | {error, wok_req()}.
```

```elixir
Wok.Request.params(Req :: wok_req(), 
                   From :: :get | :post | :bind) -> {:ok, [{binary(), binary()}], wok_req()}
                                                    | {:error, wok_req()}.
```

## path/1

Return the requested path.

```erlang
wok_request:path(Req :: wok_req()) -> binary().
```

```elixir
Wok.Request.path(req :: wok_req()) -> binary().
```

## header/2

Equivalent to `headers/3` with `undefined` as third parameter.

```erlang
wok_request:header(Req :: wok_req(), Header :: binary()) -> binary() | any() | undefined.
```

```elixir
Wok.Request.header(req :: wok_req(), header :: binary()) -> binary() | any() | :undefined.
```

## header/3

Return the value for the given header.

While header names are case insensitive, this function expects the name to be a lowercase binary.

```erlang
wok_request:header(Req :: wok_req(), 
                   Header :: binary(),
                   Default :: any()) -> binary() | any() | undefined.
```

```elixir
Wok.Request.header(req :: wok_req(), 
                   header :: binary(),
                   default :: any()) -> binary() | any() | :undefined
```

## headers/1

Return all headers.

```erlang
wok_request:headers(Req :: wok_req()) -> [{binary(), iodata()}].
```

```elixir
Wok.Request.headers(req :: wok_req()) -> [{binary(), iodata()}]
```

## cookies/1

Parse and return all cookies.

Cookie names are case sensitive.

```erlang
wok_request:cookies(Req :: wok_req()) -> [{binary(), binary()}].
```

```elixir
Wok.Request.cookies(req :: wok_req()) -> [{binary(), binary()}].
```

## cookie/2

Return the value for the given cookie.

```erlang
wok_request:cookie(Req :: wok_req(), Name :: binary()) -> binary() | undefined.
```

```elixir
Wok.Request.cookie(req :: wok_req(), name :: binary()) -> binary() | :undefined
```

## local_state/1

In a middelware, return the middleware local state.

```erlang
wok_request:local_state(Req :: wok_req()) -> any().
```

```elixir
Wok.Request.local_state(req :: wok_req()) -> any()
```

## local_state/2

In a middelware, set the middleware local state.

```erlang
wok_request:local_state(Req :: wok_req(), State :: any()) -> wok_req().
```

```elixir
Wok.Request.local_state(req :: wok_req(), state :: any()) -> wok_req().
```

## global_state/1

Return the wok global state.

```erlang
wok_request:global_state(Req :: wok_req()) -> any().
```

```elixir
Wok.Request.global_state(req :: wok_req()) -> any()
```

## global_state/2

Set the wok global state.

```erlang
wok_request:global_state(Req :: wok_req(), State :: any()) -> wok_req().
```

```elixir
Wok.Request.global_state(req :: wok_req(), state :: any()) -> wok_req().
```

# HTTP Response API

## render/2

The `render` function does the heavy lifting of rendering your application's content for use by a browser. 

```erlang
wok_response:render(Req :: wok_req(), 
                    View :: atom() | string() | binary()).
```

```elixir
Wok.Response.render(req :: wok_req(), 
                    view :: atom() | string() | binary())
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

## render/3

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
                    view :: atom() | string() | binary())
Wok.Response.render(req :: wok_req(), 
                    view :: atom() | string() | binary(), 
                    data :: map() | [tuple()])
Wok.Response.render(req :: wok_req(), 
                    headers :: map() | [tuple()], 
                    view :: atom() | string() | binary())
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

## render/4

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
                    Data :: map() | [tuple()])
Wok.Response.render(Req :: wok_req(), 
                    Headers :: map() | [tuple()], 
                    View :: atom() | string() | binary(), 
                    Data :: map() | [tuple()])
Wok.Response.render(Req :: wok_req(), 
                    Code :: integer(), 
                    Headers :: map() | [tuple()], 
                    View :: atom() | string() | binary())
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

## render/5

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
                    data :: map() | [tuple()])
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

## redirect/2

## set_cookie/3

## set_cookie/4

Otions = [{max_age, non_neg_integer()} | {domain, binary()} | {path, binary()} | {secure, boolean()} | {http_only, boolean()}]

## delete_cookie/2

## set_response/2

## set_headers/2

## merge_headers/2

