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
wok_message:reply(wok_msg:wok_msg(), 
                  Topic :: binary() | {binary(), integer()} | {binary(), binary()}, 
                  To :: binary(), 
                  Body :: term()) -> wok_msg:wok_msg().
```
```elixir
Wok.Message.reply(wok_msg:wok_msg(), 
                  topic :: binary() | {binary(), integer()} | {binary(), binary()}, 
                  to :: binary(), 
                  body :: binary()) -> wok_msg:wok_msg()
```

## reply/5

```erlang
wok_message:reply(wok_msg:wok_msg(), 
                  Topic :: binary() | {binary(), integer()} | {binary(), binary()}, 
                  From :: binary(), 
                  To :: binary(), 
                  Body :: term()) -> wok_msg:wok_msg().
```
```elixir
Wok.Message.reply(wok_msg:wok_msg(), 
                  topic :: binary() | {binary(), integer()} | {binary(), binary()}, 
                  from :: binary(), 
                  to :: binary(), 
                  body :: term()) -> wok_msg:wok_msg()
```

## provide/2

Send a message in the queue.

```erlang
wok_message:provide(Topic :: binary() | {binary(), integer()} | {binary(), binary()},
                    Message :: binary()) -> {ok, term()} | {error, term()}.

```elixir
Wok.Message.provide(topic :: binary() | {binary(), integer()} | {binary(), binary()},
                    message :: term()) -> {:ok, term()} | {:error, term()}
```

## provide/4

Send a message in the queue.

**Types:**

* **Topic**: The topic used to send the message. This parameter can be a simple binary representing the topic name, a tuple where the first parameter is the topic name and the second is an integer specifying the partition number or a tuple where the first parameter is the tomic name and the second one is a key use to calculate the partition.
* **From**: The message sender.
* **To**: The message receiver.
* **Body**: The message body.

```erlang
wok_message:provide(Topic :: binary() | {binary(), integer()} | {binary(), binary()},
                    From :: binary(),
                    To :: binary(),
                    Body :: term()) -> {ok, term()} | {error, term()}.
```
```elixir
Wok.Message.provide(topic :: binary() | {binary(), integer()} | {binary(), binary()},
                    from :: binary(),
                    to :: binary(),
                    body :: term()) -> {:ok, term()} | {:error, term()}
```

## provide/5

Send a message in the queue.

**Types:**

* **Topic**: The topic used to send the message. This parameter can be a simple binary representing the topic name, a tuple where the first parameter is the topic name and the second is an integer specifying the partition number or a tuple where the first parameter is the tomic name and the second one is a key use to calculate the partition.
* **From**: The message sender.
* **To**: The message receiver.
* **Body**: The message body.
* **Options** : Options to pass to the message handler.

If you use the default message handler, the availables options are :

* `{wok_version, integer()}` : The handler version (optional, default: 1). **Do not change this value**.
* `{headers, map()}` : The message headers (optional).

```erlang
wok_message:provide(Topic :: binary() | {binary(), integer()} | {binary(), binary()},
                    From :: binary(),
                    To :: binary(),
                    Body :: term(),
                    Options :: list()) -> {ok, term()} | {error, term()}.
```
```elixir
Wok.Message.provide(topic :: binary() | {binary(), integer()} | {binary(), binary()},
                    from :: binary(),
                    to :: binary(),
                    body :: term(),
                    options :: list()) -> {:ok, term()} | {:error, term()}
```

## encode_reply/4

```erlang
wok_message:encode_reply(Msg :: wok_msg:wok_msg(),
                         Topic :: binary()
                         | {Topic :: binary(), Partition :: integer()}
                         | {Topic :: binary(), Key :: binary()},
                         To :: binary(),
                         Body :: term()) -> {ok, binary(), integer(), opaque_message_transfert()}
                                            | {error, term()}.
```

```elixir
Wok.Message.encode_reply(msg :: wok_msg:wok_msg(),
                         topic :: binary()
                         | {topic :: binary(), partition :: integer()}
                         | {topic :: binary(), key :: binary()},
                         to :: binary(),
                         body :: term()) -> {:ok, binary(), integer(), opaque_message_transfert()}
                                            | {:error, term()}
```

## encode_reply/5

```erlang
wok_message:encode_reply(Msg :: wok_msg:wok_msg(),
                         Topic :: binary()
                         | {Topic :: binary(), Partition :: integer()}
                         | {Topic :: binary(), Key :: binary()},
                         From :: binary(),
                         To :: binary(),
                         Body :: term()) -> {ok, binary(), integer(), opaque_message_transfert()}
                                            | {error, term()}.
```

```elixir
Wok.Message.encode_reply(msg :: wok_msg:wok_msg(),
                         topic :: binary()
                         | {topic :: binary(), partition :: integer()}
                         | {topic :: binary(), key :: binary()},
                         from :: binary(),
                         to :: binary(),
                         body :: term()) -> {:ok, binary(), integer(), opaque_message_transfert()}
                                            | {:error, term()}
```

## async_reply/1

```erlang
wok_message:async_reply(wok_msg:wok_msg()) -> wok_msg:wok_msg().
```

```elixir
Wok.Message.async_reply(wok_msg:wok_msg()) -> wok_msg:wok_msg()
```

## encode_message/4

```erlang
wok_message:encode_message(Topic :: binary()
                           | {Topic :: binary(), Partition :: integer()}
                           | {Topic :: binary(), Key :: binary()},
                           From :: binary(),
                           To :: binary(),
                           Body :: term()) -> {ok, binary(), integer(), opaque_message_transfert()}
                                              | {error, term()}.
```

```elixir
Wok.Message.encode_message(topic :: binary()
                           | {topic :: binary(), partition :: integer()}
                           | {topic :: binary(), key :: binary()},
                           from :: binary(),
                           to :: binary(),
                           body :: term()) -> {:ok, binary(), integer(), opaque_message_transfert()}
                                              | {:error, term()}
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

## files/1

Return all uploaded files informations and data.

```erlang
wok_request:files(Req :: wok_req:wok_req()) -> {ok, [{Field :: binary(),
                                                      ContentType :: binary(),
                                                      Filename :: file:filename_all()}],
                                                wok_req:wok_req()}
                                              | {no_file, wok_req:wok_req()}.
```

```elixir
Wok.Request.file(req :: wok_req:wok_req()) -> {:ok, [{field :: binary(),
                                                      content_type :: binary(),
                                                      filename :: file:filename_all()}],
                                                    wok_req:wok_req()}
                                              | {:no_file, wok_req:wok_req()}
```

## file/2

Return an uploaded file informations.

```erlang
wok_request:file(Req :: wok_req:wok_req(), Field :: binary()) ->
  {ok, ContentType :: binary(), Filename :: file:filename_all(), wok_req:wok_req()}
  | {no_file, wok_req:wok_req()}.
```

```elixir
Wok.Request.file(req :: wok_req:wok_req(), field :: binary()) ->
  {:ok, content_type :: binary(), filename :: file:filename_all(), wok_req:wok_req()}
  | {:no_file, wok_req:wok_req()}
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

