

# Module wok_routes #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#path-2">path/2</a></td><td>Equivalent to <a href="#path-3"><tt>path('GET', Handler, Function)</tt></a>.</td></tr><tr><td valign="top"><a href="#path-3">path/3</a></td><td>
Return the path for the given <tt>Verb</tt>, <tt>Handler</tt> and <tt>Function</tt></td></tr><tr><td valign="top"><a href="#path-4">path/4</a></td><td>
Return the path for the given <tt>Verb</tt>, <tt>Handler</tt> and <tt>Function</tt>
and replace the bindings values by the ones in the given map.</td></tr><tr><td valign="top"><a href="#paths-2">paths/2</a></td><td></td></tr><tr><td valign="top"><a href="#static-0">static/0</a></td><td>
Return the static route.</td></tr><tr><td valign="top"><a href="#static-1">static/1</a></td><td>
Return the static route.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="path-2"></a>

### path/2 ###

`path(Handler, Function) -> any()`

Equivalent to [`path('GET', Handler, Function)`](#path-3).

<a name="path-3"></a>

### path/3 ###

`path(Verb, Handler, Function) -> any()`

Return the path for the given `Verb`, `Handler` and `Function`

Or

equiv to path('GET', Verb, Handler, Function, Args).

<a name="path-4"></a>

### path/4 ###

`path(Verb, Handler, Function, Args) -> any()`

Return the path for the given `Verb`, `Handler` and `Function`
and replace the bindings values by the ones in the given map.

<a name="paths-2"></a>

### paths/2 ###

<pre><code>
paths(Handler::atom(), Function::atom()) -&gt; string()
</code></pre>
<br />

<a name="static-0"></a>

### static/0 ###

<pre><code>
static() -&gt; string()
</code></pre>
<br />

Return the static route

<a name="static-1"></a>

### static/1 ###

<pre><code>
static(Path::string() | binary()) -&gt; string()
</code></pre>
<br />

Return the static route

