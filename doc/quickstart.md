# Quickstart

## Choose your language

You can use `wok` with [erlang](http://www.erlang.org/), [elixir](http://elixir-lang.org/) and (soon) with [Ruby](https://www.ruby-lang.org) or [Python](https://www.python.org/). 

## Create a new project

Create a new project with your favorite language and add `wok` in the dependencies.

```erlang
{wok, ".*", {git, "https://github.com/scalezen-developer/wok.git", {branch, "master"}}}
```

```elixir
{:wok, git: "git@gitlab.scalezen.com:msaas/wok.git", branch: :master}
```

## Create your consumer

A consumer is a simple module with (at least) one function. This function receive two parameters : 

* `message` which is the message received from the queue (Kafka).
* `state`, the wok's global state.

This function must return a tuple which can be :

* `{noreply, State ::term()}`
* `{reply, Topic :: binary(), {From :: binary(), To :: binary(), Body :: binary()}, State :: term()}`
* `{reply, Topic :: binary(), {To :: binary(), Body :: binary()}, State :: term()}`
* `{reply, Topic :: binary(), Message :: binary(), State :: term()}`

Where :

* `State` is the (new) wok's global state.
* `Topic` is the name of  the topic to use to send  the  response.
* `From` is the name of the sender.
* `To` is the name of the recipient.
* `Body` is the content body.

```erlang
-module(my_service).
-include_lib("wok_message_handler/include/wok_message_handler.hrl").

-export([my_action/2]).

my_action(Message, State) ->
  % do domething with Message and State
  {reply, <<"test">>, {<<"recipient/action">>, <<"Message response from my_action">>}, State}.
```

```elixir
defmodule MyService do
  require Record
  Record.defrecord :message, Record.extract(:message, from_lib: "wok_message_handler/include/wok_message_handler.hrl")

  def my_action(message, state) do
    % do domething with Message and State
    {:reply, "test", {"recipient/action", "Message response from my_action"}, state}
  end
end
```

## Add a REST API

To add a REST api, create a new handler for each route. A REST handler is a simple module with (at least) one function receiving two parameters :

* `Req` which is the HTTP Request.
* `State` the wok's global state.

This function must return a tuple containing :

* The HTTP return code.
* A list of HTTP headers.
* The response body.
* The (new) wok's  global state.

```erlang
-module(my_rest_handler).

-export([my_controler/2]).

my_controler(Req, State) ->
  % do domething with Req and State
  {200, [{<<"content-type">>, <<"text/plain">>}], <<"Hello World">>, State}.
```

```elixir
defmodule MyRestHandler do

  def my_controler(req, state) do
    % do domething with req and State
    {200, [{"content-type", "text/plain"}], "Hello World", state}
  end
end
```

## Configure your bot

<aside class="notice">
See <a href="#configuration">Wok configuration</a> for more information.
</aside>

In the configuration, you must register your consumer. To do so, add a tuple in the `controlers` section. This tuple contains the name of the controler and a tuple with the consumer's module and function names.

```erlang
{<<"my_service/my_action">>, {my_service, my_action}}
```

```elixir
{"my_service/my_action", {MyService, :my_action}}
```

So, all message sended *to* `my_service/my_action` (or `my_service/*`, `*/my_action` and `*`) will be passed to the consumer `my_service/my_action`.

To register your REST api, add a tuple in the `routes` subsection of the REST section. This tuple must contains :

* The type of request (HTTP verb) accepted by the route.
* The route. It is possible to extract segments of the path and to store the values in the request for later use. To do so, add a `:` character at the begining of a segment. This means that what follows until the end of the segment is the name of the binding in which the segment value will be stored.
* A tuple with the module and fonction names of the controler.

```erlang
{'GET', "/my/route", {my_rest_handler, my_controler}}
```

```elixir
{:GET, '/my/route', {MyRestHandler, :my_controler}}
```

<aside class="notice">
Wok use <a href="http://ninenines.eu/">Cowboy</a>, so for more information about routing, see <a href="http://ninenines.eu/docs/en/cowboy/HEAD/guide/routing/">Cowboy routing</a>.
</aside>

OK, your bot is ready to start...

