# Templates

To help you to create your views, *wok* offers a template engine. 

## Wok Erlang Template

This template engine, usable in Erlang and Elixir, is based on 
[ErlyDTL](https://github.com/erlydtl/erlydtl). It is based on the 
[Django Template Language](https://docs.djangoproject.com/es/1.9/ref/templates/).

When you create a new *bot* with *chopstick*, *Wok Erlang Templates* is 
installed by default. However, if you want to install it by yourself, see 
[installation](#installation)

## Configuration

You can configure the *Wok Erlang Template* engine by using the followinf options :

Makefile | mix.exs | Default | Description
---------|---------|---------|------------
`TMPL_PATH` | `erlydtl_options[:source]` | `templates` | Templates directory sources.
`TMPL_SUFFIX` | `erlydtl_options[:suffix]` | `_tmpl` | Suffix added to the compiled template module.
`TMPL_EXT` | `erlydtl_options[:ext]` | `tmpl` | Extension template sources.
`TMPL_OPTS` | `erlydtl_options[:compiler_options]` | `[]` | ErlyDTL compiler options (see [ErlyDTL documentation](https://github.com/erlydtl/erlydtl#template-compilation))

## Installation

```erlang
% Update Erlang Makefile :

DEPS += wok_erlang_templates
dep_wok_erlang_templates = git git@gitlab.botsunit.com:msaas/wok_erlang_templates.git master

DEP_PLUGINS = wok_erlang_templates

TMPL_OPTS = {libraries, [{wok_helpers, wok_helpers_dtl}]} \
            , {default_libraries, [wok_helpers]} \
            , report_warnings \
            , report_errors
```

```elixir
# Add wok_erlang_templates dependecy to mix.exs:

{:wok_erlang_templates, ~r/.*/, 
 git: "git@gitlab.botsunit.com:msaas/wok_erlang_templates.git", 
 branch: "master"}
```

```elixir
# Add erlydtl compiler in mix.exs

compilers: Mix.compilers ++ [:erlydtl]
```

```elixir
# Set ErlyDTL options in mix.exs

erlydtl_options: [
  source: "templates",
  ext: :tmpl,
  suffix: "_tmpl",
  compiler_options: [:debug_info]
]
```

In an Erlang project, with [erlang.mk](http://erlang.mk), you need to modify your `Makefile` by
adding `wok_erlang_templates` in your dependencies and set the template engine options 
(see [configuration](#configuration)).

In an Elixir project, 

* Add `wok_erlang_templates` in your dependencies.
* Add the `erlydtl` compiler to the compiler list.
* Set the template engine options (see [configuration](#configuration))

You can now create a directory `templates` in your project and place your templates in this directory.

