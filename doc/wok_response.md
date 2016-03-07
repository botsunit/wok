

# Module wok_response #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#delete_cookie-2">delete_cookie/2</a></td><td></td></tr><tr><td valign="top"><a href="#merge_headers-2">merge_headers/2</a></td><td></td></tr><tr><td valign="top"><a href="#redirect-2">redirect/2</a></td><td> 
Redirect.</td></tr><tr><td valign="top"><a href="#render-2">render/2</a></td><td>
render(Req, 200, [{<<"content-type">>, <<"text/html">>}], View, []).</td></tr><tr><td valign="top"><a href="#render-3">render/3</a></td><td>
render(Req, Code, [{<<"content-type">>, <<"text/html">>}], View, [])
render(Req, 200, Headers, View, [])
render(Req, 200, [{<<"content-type">>, <<"text/html">>}], View, Data).</td></tr><tr><td valign="top"><a href="#render-4">render/4</a></td><td>
render(Req, 200, Headers, View, Data)
render(Req, Code, [{<<"content-type">>, <<"text/html">>}], View, Data)
render(Req, Code, Headers, View, []).</td></tr><tr><td valign="top"><a href="#render-5">render/5</a></td><td> 
Generate a reponse with the given view.</td></tr><tr><td valign="top"><a href="#set_cookie-3">set_cookie/3</a></td><td>Equivalent to <a href="#set_cookie-4"><tt>set_cookie(Req, Name, Value, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#set_cookie-4">set_cookie/4</a></td><td>
Otions = [{max_age, non_neg_integer()} | {domain, binary()} | {path, binary()} | {secure, boolean()} | {http_only, boolean()}].</td></tr><tr><td valign="top"><a href="#set_headers-2">set_headers/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_response-2">set_response/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="delete_cookie-2"></a>

### delete_cookie/2 ###

`delete_cookie(Req, Name) -> any()`

<a name="merge_headers-2"></a>

### merge_headers/2 ###

`merge_headers(Req, Headers) -> any()`

<a name="redirect-2"></a>

### redirect/2 ###

`redirect(Req, Path) -> any()`


Redirect

Example :

`
wok_response:redirect(Req, "/logout").
wok_response:redirect(Req, {handler, fun}).
wok_response:redirect(Req, {handler, fun, #{id => 1, name => <<"John">>}}).
`

<a name="render-2"></a>

### render/2 ###

`render(Req, View) -> any()`

render(Req, 200, [{<<"content-type">>, <<"text/html">>}], View, [])

<a name="render-3"></a>

### render/3 ###

`render(Req, Code, View) -> any()`

render(Req, Code, [{<<"content-type">>, <<"text/html">>}], View, [])
render(Req, 200, Headers, View, [])
render(Req, 200, [{<<"content-type">>, <<"text/html">>}], View, Data)

<a name="render-4"></a>

### render/4 ###

`render(Req, Headers, View, Data) -> any()`

render(Req, 200, Headers, View, Data)
render(Req, Code, [{<<"content-type">>, <<"text/html">>}], View, Data)
render(Req, Code, Headers, View, [])

<a name="render-5"></a>

### render/5 ###

`render(Req, Code, Headers, View, Data) -> any()`


Generate a reponse with the given view.

Example :

`
wok_response:render(Req, "login.html.tmpl", #{error => "Wrong login or password"}).
`

<a name="set_cookie-3"></a>

### set_cookie/3 ###

`set_cookie(Req, Name, Value) -> any()`

Equivalent to [`set_cookie(Req, Name, Value, [])`](#set_cookie-4).

<a name="set_cookie-4"></a>

### set_cookie/4 ###

`set_cookie(Req, Name, Value, Options) -> any()`

Otions = [{max_age, non_neg_integer()} | {domain, binary()} | {path, binary()} | {secure, boolean()} | {http_only, boolean()}]

<a name="set_headers-2"></a>

### set_headers/2 ###

`set_headers(Req, Headers) -> any()`

<a name="set_response-2"></a>

### set_response/2 ###

`set_response(Req, Response) -> any()`

