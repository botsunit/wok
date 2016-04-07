# Messages API

## content/1

```erlang
wok_message:content(wok_msg:wok_msg()) -> wok_message_handler:message().
```
```elixir
Wok.Message.content(wok_msg:wok_msg()) -> wok_message_handler:message()
```

## content_has_map/1

```erlang
wok_message:content_has_map(wok_msg:wok_msg()) -> map().
```
```elixir
Wok.Message.content_has_map(wok_msg:wok_msg()) -> map()
```

## content/2

```erlang
wok_message:content(wok_msg:wok_msg(), wok_message_handler:message()) -> wok_msg:wok_msg().
```
```elixir
Wok.Message.content(wok_msg:wok_msg(), wok_message_handler:message()) -> wok_msg:wok_msg()
```

## uuid/1

```erlang
wok_message:uuid(wok_msg:wok_msg() | wok_message_handler:message()) -> binary().
```
```elixir
Wok.Message.uuid(wok_msg:wok_msg() | wok_message_handler:message()) -> binary()
```

## from/1

```erlang
wok_message:from(wok_msg:wok_msg() | wok_message_handler:message()) -> binary().
```
```elixir
Wok.Message.from(wok_msg:wok_msg() | wok_message_handler:message()) -> binary()
```

## to/1

```erlang
wok_message:to(wok_msg:wok_msg() | wok_message_handler:message()) -> binary().
```
```elixir
Wok.Message.to(wok_msg:wok_msg() | wok_message_handler:message()) -> binary()
```

## headers/1

```erlang
wok_message:headers(wok_msg:wok_msg() | wok_message_handler:message()) -> binary().
```
```elixir
Wok.Message.headers(wok_msg:wok_msg() | wok_message_handler:message()) -> binary()
```

## body/1

```erlang
wok_message:body(wok_msg:wok_msg() | wok_message_handler:message()) -> binary().
```
```elixir
Wok.Message.body(wok_msg:wok_msg() | wok_message_handler:message()) -> binary()
```

## global_state/1

```erlang
wok_message:global_state(wok_msg:wok_msg()) -> any().
```
```elixir
Wok.Message.global_state(wok_msg:wok_msg()) -> any()
```

## local_state/1

```erlang
wok_message:local_state(wok_msg:wok_msg()) -> any().
```
```elixir
Wok.Message.local_state(wok_msg:wok_msg()) -> any()
```

## custom_data/1

```erlang
wok_message:custom_data(wok_msg:wok_msg()) -> map().
```
```elixir
Wok.Message.custom_data(wok_msg:wok_msg()) -> map()
```

## custom_data/2

```erlang
wok_message:custom_data(wok_msg:wok_msg(), atom()) -> any().
```
```elixir
Wok.Message.custom_data(wok_msg:wok_msg(), atom()) -> any()
```

## custom_data/3

```erlang
wok_message:custom_data(wok_msg:wok_msg(), atom(), any()) -> {ok, wok_req:wok_req()}
                                                             | {ok, any(), wok_req:wok_req()}.
```
```elixir
Wok.Message.custom_data(wok_msg:wok_msg(), atom(), any()) -> {ok, wok_req:wok_req()}
                                                             | {ok, any(), wok_req:wok_req()}
```

## noreply/1

```erlang
wok_message:noreply(wok_msg:wok_msg()) -> wok_msg:wok_msg().
```
```elixir
Wok.Message.noreply(wok_msg:wok_msg()) -> wok_msg:wok_msg()
```

## reply/4

```erlang
wok_message:reply(wok_msg:wok_msg(), binary() | {binary(), integer()}, binary(), binary()) -> wok_msg:wok_msg().
```
```elixir
Wok.Message.reply(wok_msg:wok_msg(), binary() | {binary(), integer()}, binary(), binary()) -> wok_msg:wok_msg()
```

## reply/5

```erlang
wok_message:reply(wok_msg:wok_msg(), binary() | {binary(), integer()}, binary(), binary(), binary()) -> wok_msg:wok_msg().
```
```elixir
Wok.Message.reply(wok_msg:wok_msg(), binary() | {binary(), integer()}, binary(), binary(), binary()) -> wok_msg:wok_msg()
```

## provide/2

```erlang
wok_message:provide(Topic :: binary() | list() | atom() | {binary() | list() | atom(), integer()},
                    Message :: binary()) -> {ok, term()} | {error, term()}.

% DEPRECATED
wok:provide(Topic :: binary() | list() | atom() | {binary() | list() | atom(), integer()},
            Message :: binary()) -> {ok, term()} | {error, term()}.
```
```elixir
Wok.Message.provide(topic :: binary() | list() | atom() | {binary() | list() | atom(), integer()},
                    message :: binary()) -> {:ok, term()} | {:error, term()}

# DEPRECATED
Wok.provide(topic :: binary() | list() | atom() | {binary() | list() | atom(), integer()},
            message :: binary()) -> {:ok, term()} | {:error, term()}
```

## provide/4

```erlang
wok_message:provide(Topic :: binary() | list() | atom() | {binary() | list() | atom(), integer()},
                    From :: binary(),
                    To :: binary(),
                    Body :: term()) -> {ok, term()} | {error, term()}.

% DEPRECATED
wok:provide(Topic :: binary() | list() | atom() | {binary() | list() | atom(), integer()},
            From :: binary(),
            To :: binary(),
            Body :: term()) -> {ok, term()} | {error, term()}.
```
```elixir
Wok.Message.provide(topic :: binary() | list() | atom() | {binary() | list() | atom(), integer()},
                    from :: binary(),
                    to :: binary(),
                    body :: term()) -> {:ok, term()} | {:error, term()}

# DEPRECATED
Wok.provide(topic :: binary() | list() | atom() | {binary() | list() | atom(), integer()},
            from :: binary(),
            to :: binary(),
            body :: term()) -> {:ok, term()} | {:error, term()}
```

## provide/5

```erlang
wok_message:provide(Topic :: binary() | list() | atom() | {binary() | list() | atom(), integer()},
                    From :: binary(),
                    To :: binary(),
                    Body :: term(),
                    Options :: map()) -> {ok, term()} | {error, term()}.

% DEPRECATED
wok:provide(Topic :: binary() | list() | atom() | {binary() | list() | atom(), integer()},
            From :: binary(),
            To :: binary(),
            Body :: term(),
            Options :: map()) -> {ok, term()} | {error, term()}.
```
```elixir
Wok.Message.provide(topic :: binary() | list() | atom() | {binary() | list() | atom(), integer()},
                    from :: binary(),
                    to :: binary(),
                    body :: term(),
                    options :: map()) -> {:ok, term()} | {:error, term()}

# DEPRECATED
Wok.provide(topic :: binary() | list() | atom() | {binary() | list() | atom(), integer()},
            from :: binary(),
            to :: binary(),
            body :: term(),
            options :: map()) -> {:ok, term()} | {:error, term()}
```

# HTTP Request API

## custom_data/1

This function allow you to get your custom data from the wok request opaque structure.

```erlang
wok_request:custom_data(Req :: wok_req:wok_req()) -> map().
```

```elixir
Wok.Request.custom_data(req :: wok_req:wok_req()) -> map()
```

## custom_data/2

```erlang
wok_request:custom_data(Req :: wok_req:wok_req(), Key :: atom()) -> any().
```
```elixir
Wok.Request.custom_data(Req :: wok_req:wok_req(), Key :: atom()) -> any()
```

## custom_data/3

```erlang
wok_request:custom_data(Req :: wok_req:wok_req(), Key :: atom(), Value :: any()) ->
  {ok, wok_req:wok_req()}
  | {ok, any(), wok_req:wok_req()}.
```
```elixir
Wok.Request.custom_data(Req :: wok_req:wok_req(), Key :: atom(), Value :: any()) ->
  {ok, wok_req:wok_req()}
  | {ok, any(), wok_req:wok_req()}
```

## client_ip/1

Return the client's IP address.

```erlang
wok_request:client_ip(Req :: wok_req:wok_req()) -> inet:ip_address().
```

```elixir
Wok.Request.client_ip(req :: wok_req:wok_req()) -> inet::ip_address()
```

## client_port/1

Return the client's port number.

```erlang
wok_request:client_port(Req :: wok_req:wok_req()) -> inet:port_number().
```

```elixir
Wok.Request.client_port(req :: wok_req:wok_req()) -> inet::port_number()
```

## body/1

Read the request body.

This function will read a chunk of the request body. If there is more data to be read after this function call, then a `more` tuple is returned. Otherwise an `ok` tuple is returned.

```erlang
wok_request:body(Req :: wok_req:wok_req()) -> {ok, binary(), wok_req:wok_req()}
                                              | {more, binary(), wok_req:wok_req()}.
```

```elixir
Wok.Request.body(req :: wok_req:wok_req()) -> {:ok, binary(), wok_req:wok_req()}
                                              | {:more, binary(), wok_req:wok_req()}
```

## has_body/1

Return whether the request has a body.

```erlang
wok_request:has_body(Req :: wok_req:wok_req()) -> boolean().
```

```elixir
Wok.Request.has_body(req :: wok_req:wok_req()) -> boolean()
```

## body_length/1

Return the length of the request body.

The length will only be returned if the request does not use any transfer-encoding and if the content-length header is present.

```erlang
wok_request:body_length(Req :: wok_req:wok_req()) -> non_neg_integer() | undefined.
```

```elixir
Wok.Request.body_length(req :: wok_req:wok_req()) -> non_neg_integer() | undefined
```

## method/1

Return the method.

Methods are case sensitive. Standard methods are always uppercase.

```erlang
wok_request:method(Req :: wok_req:wok_req()) -> binary().
```

```elixir
Wok.Request.method(req :: wok_req:wok_req()) -> binary()
```

## param/2

Return the value for the given parameter.
The parameter is sought in the query string, in the body and in the bindings.

```erlang
wok_request:param(Req :: wok_req:wok_req(), Param :: binary() | string() | atom()) ->
  {ok, binary(), wok_req:wok_req()}
  | {undefined, wok_req:wok_req()}
  | {error, wok_req:wok_req()}.
```

```elixir
Wok.Request.param(req :: wok_req:wok_req(), param :: binary() | string() | atom()) ->
  {:ok, binary(), wok_req:wok_req()}
  | {:undefined, wok_req:wok_req()}
  | {:error, wok_req:wok_req()}
```

## params/1

Return all parameters.

```erlang
wok_request:params(Req :: wok_req:wok_req()) -> {ok, [{binary(), binary()}], wok_req:wok_req()}
                                                | {error, wok_req:wok_req()}.
```

```elixir
Wok.Request.params(req :: wok_req:wok_req()) -> {:ok, [{binary(), binary()}], wok_req:wok_req()}
                                                | {:error, wok_req:wok_req()}
```

## params/2

Return all parameters for the given domain.

```erlang
wok_request:params(Req :: wok_req:wok_req(),
                   From :: get | post | bind) -> {ok, [{binary(), binary()}], wok_req:wok_req()}
                                                 | {error, wok_req:wok_req()}.
```

```elixir
Wok.Request.params(Req :: wok_req:wok_req(),
                   From :: :get | :post | :bind) -> {:ok, [{binary(), binary()}], wok_req:wok_req()}
                                                    | {:error, wok_req:wok_req()}.
```

## param/3

Return the value for the given parameter.
The parameter is sought in the query string, in the body and in the bindings.
The given 'Default' value is returned if the parameter is not found anywhere in the request.

```erlang
wok_request:param(Req :: wok_req:wok_req(),
                  From :: get | post | bind,
                  Param :: binary() | string() | atom()) -> {ok, binary(), wok_req:wok_req()}
                                                            | {undefined, wok_req:wok_req()}
                                                            | {error, wok_req:wok_req()}.
wok_request:param(Req :: wok_req:wok_req(),
                  Param :: binary() | string() | atom()
                  Default :: term()) -> {ok, binary() | term(), wok_req:wok_req()}
                                        | {undefined, wok_req:wok_req()}
                                        | {error, wok_req:wok_req()}.
```

```elixir
Wok.Request.param(req :: wok_req:wok_req(),
                  from :: :get | :post | :bind,
                  param :: binary() | string() | atom()) -> {:ok, binary(), wok_req:wok_req()}
                                                            | {:undefined, wok_req:wok_req()}
                                                            | {:error, wok_req:wok_req()}
Wok.Request.param(req :: wok_req:wok_req(),
                  param :: binary() | string() | atom()
                  Default :: term()) -> {:ok, binary(), wok_req:wok_req()}
                                        | {:undefined, wok_req:wok_req()}
                                        | {:error, wok_req:wok_req()}
```

## path/1

Return the requested path.

```erlang
wok_request:path(Req :: wok_req:wok_req()) -> binary().
```

```elixir
Wok.Request.path(req :: wok_req:wok_req()) -> binary().
```

## header/2

Equivalent to `headers/3` with `undefined` as third parameter.

```erlang
wok_request:header(Req :: wok_req:wok_req(), Header :: binary()) -> binary() | any() | undefined.
```

```elixir
Wok.Request.header(req :: wok_req:wok_req(), header :: binary()) -> binary() | any() | :undefined.
```

## header/3

Return the value for the given header.

While header names are case insensitive, this function expects the name to be a lowercase binary.

```erlang
wok_request:header(Req :: wok_req:wok_req(),
                   Header :: binary(),
                   Default :: any()) -> binary() | any() | undefined.
```

```elixir
Wok.Request.header(req :: wok_req:wok_req(),
                   header :: binary(),
                   default :: any()) -> binary() | any() | :undefined
```

## headers/1

Return all headers.

```erlang
wok_request:headers(Req :: wok_req:wok_req()) -> [{binary(), iodata()}].
```

```elixir
Wok.Request.headers(req :: wok_req:wok_req()) -> [{binary(), iodata()}]
```

## cookies/1

Parse and return all cookies.

Cookie names are case sensitive.

```erlang
wok_request:cookies(Req :: wok_req:wok_req()) -> [{binary(), binary()}].
```

```elixir
Wok.Request.cookies(req :: wok_req:wok_req()) -> [{binary(), binary()}].
```

## cookie/2

Return the value for the given cookie.

```erlang
wok_request:cookie(Req :: wok_req:wok_req(), Name :: binary()) -> binary() | undefined.
```

```elixir
Wok.Request.cookie(req :: wok_req:wok_req(), name :: binary()) -> binary() | :undefined
```

## local_state/1

In a middelware, return the middleware local state.

```erlang
wok_request:local_state(Req :: wok_req:wok_req()) -> any().
```

```elixir
Wok.Request.local_state(req :: wok_req:wok_req()) -> any()
```

## local_state/2

In a middelware, set the middleware local state.

```erlang
wok_request:local_state(Req :: wok_req:wok_req(), State :: any()) -> wok_req:wok_req().
```

```elixir
Wok.Request.local_state(req :: wok_req:wok_req(), state :: any()) -> wok_req:wok_req().
```

## global_state/1

Return the wok global state.

```erlang
wok_request:global_state(Req :: wok_req:wok_req()) -> any().
```

```elixir
Wok.Request.global_state(req :: wok_req:wok_req()) -> any()
```

## global_state/2

Set the wok global state.

```erlang
wok_request:global_state(Req :: wok_req:wok_req(), State :: any()) -> wok_req:wok_req().
```

```elixir
Wok.Request.global_state(req :: wok_req:wok_req(), state :: any()) -> wok_req:wok_req().
```

## file/1

Return uploaded file informations and data.

```erlang
wok_request:file(Req :: wok_req:wok_req()) -> {ok, Filename :: binary(),
                                                   ContentType :: binary(),
                                                   Data :: binary(),
                                                   wok_req:wok_req()}
                                              | {no_file, wok_req:wok_req()}.
```

```elixir
Wok.Request.file(req :: wok_req:wok_req()) -> {:ok, filename :: binary(),
                                                    content_type :: binary(),
                                                    data :: binary(),
                                                    wok_req:wok_req()}
                                              | {:no_file, wok_req:wok_req()}
```
## file/2

Return uploaded file informations and data.

```erlang

get_file_callback() = fun((Filename :: binary(), ContentType ::  binary(), Data :: binary(), Accumulator :: any()) ->
                        {ok, Accumulator :: any()} 
                        | {error, Reason :: term(), Accumulator :: any()}).

wok_request:file(wok_req:wok_req(), FilePidOrFun :: file:filename_all() | pid() | get_file_callback()) ->
  {ok, FileOrPid :: file:filename_all() | pid(), wok_req:wok_req()}
  | {ok, wok_req:wok_req()}
  | {error, Reason :: term(), FileOrPid :: file:filename_all() | pid(), wok_req:wok_req()}
  | {error, Reason :: term(), wok_req:wok_req()}
  | {no_file, FileOrPid :: file:filename_all() | pid(), wok_req:wok_req()}
  | {no_file, wok_req:wok_req()}.
```

```elixir
get_file_callback() = fn((filename :: binary(), content_type ::  binary(), data :: binary(), accumulator :: any()) ->
                        {ok, accumulator :: any()} 
                        | {error, reason :: term(), accumulator :: any()})

Wok.Request.file(wok_req:wok_req(), file_pid_or_fun :: file:filename_all() | pid() | get_file_callback()) ->
  {ok, file_or_pid :: file:filename_all() | pid(), wok_req:wok_req()}
  | {ok, wok_req:wok_req()}
  | {error, reason :: term(), file_or_pid :: file:filename_all() | pid(), wok_req:wok_req()}
  | {error, reason :: term(), wok_req:wok_req()}
  | {no_file, file_or_pid :: file:filename_all() | pid(), wok_req:wok_req()}
  | {no_file, wok_req:wok_req()}
```
## file/3

Return uploaded file informations and data.

```erlang
get_file_callback() = fun((Filename :: binary(), ContentType ::  binary(), Data :: binary(), Accumulator :: any()) ->
                        {ok, Accumulator :: any()} 
                        | {error, Reason :: term(), Accumulator :: any()})

wok_request:file(wok_req:wok_req(), get_file_callback(), Accumulator :: any()) -> 
  {ok, Accumulator :: any(), wok_req:wok_req()}
  | {error, Reason :: term(), Accumulator :: any(), wok_req:wok_req()}
  | {no_file, Accumulator :: any(), wok_req:wok_req()}.
```

```elixir
get_file_callback() = fn((filename :: binary(), content_type ::  binary(), data :: binary(), accumulator :: any()) ->
                        {ok, accumulator :: any()} 
                        | {error, reason :: term(), accumulator :: any()})

Wok.Request.file(wok_req:wok_req(), get_file_callback(), accumulator :: any()) -> 
  {ok, accumulator :: any(), wok_req:wok_req()}
  | {error, reason :: term(), accumulator :: any(), wok_req:wok_req()}
  | {no_file, accumulator :: any(), wok_req:wok_req()}.
```

# HTTP Response API

## render/2

The `render` function does the heavy lifting of rendering your application's content for use by a browser.

```erlang
wok_response:render(Req :: wok_req:wok_req(),
                    View :: atom() | string() | binary()).
```

```elixir
Wok.Response.render(req :: wok_req:wok_req(),
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
  wok_response:render(Req, show_tmpl).
```

```elixir
defmodule MyRestHandler do
  def show(wok_req, state) do
    # Do some stuff...
    Wok.Response.render(req, :show_tmpl)
  end
end
```

## render/3

The `render` function does the heavy lifting of rendering your application's content for use by a browser.

```erlang
wok_response:render(Req :: wok_req:wok_req(),
                    Code :: integer(),
                    View :: atom() | string() | binary()).
wok_response:render(Req :: wok_req:wok_req(),
                    View :: atom() | string() | binary(),
                    Data :: map() | [tuple()]).
wok_response:render(Req :: wok_req:wok_req(),
                    Headers :: map() | [tuple()],
                    View :: atom() | string() | binary()).
```

```elixir
Wok.Response.render(req :: wok_req:wok_req(),
                    code :: integer(),
                    view :: atom() | string() | binary())
Wok.Response.render(req :: wok_req:wok_req(),
                    view :: atom() | string() | binary(),
                    data :: map() | [tuple()])
Wok.Response.render(req :: wok_req:wok_req(),
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
  wok_response:render(Req, show_tmpl, #{firstname => "John",
                                          lastname => "Doe"}).
```

```elixir
defmodule MyRestHandler do
  def show(wok_req, state) do
    # Do some stuff...
    Wok.Response.render(req, :show_tmpl, %{firstname: "John",
                                             lastname: "Doe"))
  end
end
```

## render/4

The `render` function does the heavy lifting of rendering your application's content for use by a browser.

```erlang
wok_response:render(Req :: wok_req:wok_req(),
                    Code :: integer(),
                    View :: atom() | string() | binary(),
                    Data :: map() | [tuple()]).
wok_response:render(Req :: wok_req:wok_req(),
                    Headers :: map() | [tuple()],
                    View :: atom() | string() | binary(),
                    Data :: map() | [tuple()]).
wok_response:render(Req :: wok_req:wok_req(),
                    Code :: integer(),
                    Headers :: map() | [tuple()],
                    View :: atom() | string() | binary()).
```

```elixir
Wok.Response.render(Req :: wok_req:wok_req(),
                    Code :: integer(),
                    View :: atom() | string() | binary(),
                    Data :: map() | [tuple()])
Wok.Response.render(Req :: wok_req:wok_req(),
                    Headers :: map() | [tuple()],
                    View :: atom() | string() | binary(),
                    Data :: map() | [tuple()])
Wok.Response.render(Req :: wok_req:wok_req(),
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
  wok_response:render(Req,
                      #{content-type => "text/html"},
                      show_tmpl,
                      #{firstname => "John",
                        lastname => "Doe"}).
```

```elixir
defmodule MyRestHandler do
  def show(wok_req, state) do
    # Do some stuff...
    Wok.Response.render(req,
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
wok_response:render(Req :: wok_req:wok_req(),
                    Code :: integer(),
                    Headers :: map() | [tuple()],
                    View :: atom() | string() | binary(),
                    Data :: map() | [tuple()]).
```

```elixir
Wok.Response.render(req :: wok_req:wok_req(),
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
  wok_response:render(Req,
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
    Wok.Response.render(req,
                        200,
                        %{"content-type": "text/html"},
                        :show_tmpl,
                        %{firstname: "John",
                          lastname: "Doe"))
  end
end
```

## redirect/2

Redirects the browser to the specified target.

The target can be :

* A string.

```erlang
wok_response:redirect(Req, "/logout").
```
```elixir
Wok.Response.redirect(req, "/logout")
```
> will redirect to `/logout`.

* A tuple with a  handler and a function.

```erlang
wok_response:redirect(Req, {handler, fun}).
```
```elixir
Wok.Response.redirect(req, {:handler, :fun})
```
> If, in the configuration, you have `{'GET', "/my/route", {handler, get}}`, you will be redirected to `/my/route`.

* A tuple with a handler, a function and the bindings values.

```erlang
wok_response:redirect(Req, {handler, fun, #{id => 1, name => <<"John">>}}).
```
```elixir
Wok.Response.redirect(Req, {:handler, :fun, %{id => 1, name => <<"John">>}})
```
> If, in the configuration, you have `{'GET', "/chat/:id/user/:name", {handler, fun}}`,  you will be redirected to `/chat/1/user/John`

## set_cookie/3

Options :

* `max_age :: non_neg_integer()`
* `domain :: binary()`
* `path :: binary()`
* `secure :: boolean()`
* `http_only :: boolean()`

## set_cookie/4

Options :

* `max_age :: non_neg_integer()`
* `domain :: binary()`
* `path :: binary()`
* `secure :: boolean()`
* `http_only :: boolean()`


## delete_cookie/2

## set_response/2

## set_headers/2

## merge_headers/2

# HTTP routes API

## static/0

## static/1

## paths/2

## path/2

## path/3

## path/4

