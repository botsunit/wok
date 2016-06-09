

# Module wok_producer #
* [Function Index](#index)
* [Function Details](#functions)

__This module defines the `wok_producer` behaviour.__<br /> Required callback functions: `messages/3`, `response/2`.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#provide-2">provide/2</a></td><td></td></tr><tr><td valign="top"><a href="#provide-4">provide/4</a></td><td></td></tr><tr><td valign="top"><a href="#provide-5">provide/5</a></td><td></td></tr><tr><td valign="top"><a href="#start-0">start/0</a></td><td></td></tr><tr><td valign="top"><a href="#start-1">start/1</a></td><td></td></tr><tr><td valign="top"><a href="#start-2">start/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="provide-2"></a>

### provide/2 ###

`provide(Topic, Message) -> any()`

<a name="provide-4"></a>

### provide/4 ###

`provide(Topic, From, To, Body) -> any()`

<a name="provide-5"></a>

### provide/5 ###

`provide(Topic, From, To, Body, Options) -> any()`

<a name="start-0"></a>

### start/0 ###

`start() -> any()`

<a name="start-1"></a>

### start/1 ###

`start(Topic) -> any()`

<a name="start-2"></a>

### start/2 ###

`start(Topic, Partition) -> any()`

