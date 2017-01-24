# File: Wok.Producer.ex
# This file was generated from wok_producer.beam
# Using rebar3_elixir (https://github.com/botsunit/rebar3_elixir)
# MODIFY IT AT YOUR OWN RISK AND ONLY IF YOU KNOW WHAT YOU ARE DOING!
defmodule Wok.Producer do
  @callback messages(any, any) :: any
  @callback response(any, any) :: any
  def unquote(:"handle")(arg1, arg2, arg3) do
    :erlang.apply(:"wok_producer", :"handle", [arg1, arg2, arg3])
  end
  def unquote(:"handle")(arg1, arg2, arg3, arg4) do
    :erlang.apply(:"wok_producer", :"handle", [arg1, arg2, arg3, arg4])
  end
  def unquote(:"provide")(arg1, arg2) do
    :erlang.apply(:"wok_producer", :"provide", [arg1, arg2])
  end
  def unquote(:"provide")(arg1, arg2, arg3, arg4, arg5) do
    :erlang.apply(:"wok_producer", :"provide", [arg1, arg2, arg3, arg4, arg5])
  end
  def unquote(:"provide")(arg1, arg2, arg3, arg4) do
    :erlang.apply(:"wok_producer", :"provide", [arg1, arg2, arg3, arg4])
  end
end
