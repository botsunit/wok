# File: lib/Wok.ex
# This file was generated from src/wok.erl
# Using mix.mk (https://github.com/botsunit/mix.mk)
# MODIFY IT AT YOUR OWN RISK AND ONLY IF YOU KNOW WHAT YOU ARE DOING!
defmodule Wok do
  def unquote(:"start")() do
    :erlang.apply(:"wok", :"start", [])
  end
  def unquote(:"provide")(arg1, arg2, arg3, arg4) do
    :erlang.apply(:"wok", :"provide", [arg1, arg2, arg3, arg4])
  end
  def unquote(:"provide")(arg1, arg2, arg3, arg4, arg5) do
    :erlang.apply(:"wok", :"provide", [arg1, arg2, arg3, arg4, arg5])
  end
  def unquote(:"provide")(arg1, arg2) do
    :erlang.apply(:"wok", :"provide", [arg1, arg2])
  end
  def unquote(:"state")() do
    :erlang.apply(:"wok", :"state", [])
  end
  def unquote(:"state")(arg1) do
    :erlang.apply(:"wok", :"state", [arg1])
  end
end
