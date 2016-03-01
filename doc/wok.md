

# Module wok #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#provide-2">provide/2</a></td><td>
Send a message.</td></tr><tr><td valign="top"><a href="#provide-4">provide/4</a></td><td>
Send a message.</td></tr><tr><td valign="top"><a href="#provide-5">provide/5</a></td><td>
Send a message.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="provide-2"></a>

### provide/2 ###

<pre><code>
provide(Topic::binary() | list() | atom() | {binary() | list() | atom(), integer()}, Message::binary()) -&gt; {ok, term()} | {error, term()}
</code></pre>
<br />

Send a message

<a name="provide-4"></a>

### provide/4 ###

<pre><code>
provide(Topic::binary() | list() | atom() | {binary() | list() | atom(), integer()}, From::binary(), To::binary(), Body::term()) -&gt; {ok, term()} | {error, term()}
</code></pre>
<br />

Send a message

<a name="provide-5"></a>

### provide/5 ###

<pre><code>
provide(Topic::binary() | list() | atom() | {binary() | list() | atom(), integer()}, From::binary(), To::binary(), Body::term(), Options::#{}) -&gt; {ok, term()} | {error, term()}
</code></pre>
<br />

Send a message

