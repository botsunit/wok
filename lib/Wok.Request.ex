# File: lib/Wok.Request.ex
# This file was generated from src/wok_request.erl
# Using mix.mk (https://github.com/botsunit/mix.mk)
# MODIFY IT AT YOUR OWN RISK AND ONLY IF YOU KNOW WHAT YOU ARE DOING!
defmodule Wok.Request do
  def unquote(:"custom_data")(arg1) do
    :erlang.apply(:"wok_request", :"custom_data", [arg1])
  end
  def unquote(:"custom_data")(arg1, arg2) do
    :erlang.apply(:"wok_request", :"custom_data", [arg1, arg2])
  end
  def unquote(:"client_ip")(arg1) do
    :erlang.apply(:"wok_request", :"client_ip", [arg1])
  end
  def unquote(:"client_port")(arg1) do
    :erlang.apply(:"wok_request", :"client_port", [arg1])
  end
  def unquote(:"body")(arg1) do
    :erlang.apply(:"wok_request", :"body", [arg1])
  end
  def unquote(:"param")(arg1, arg2, arg3) do
    :erlang.apply(:"wok_request", :"param", [arg1, arg2, arg3])
  end
  def unquote(:"param")(arg1, arg2) do
    :erlang.apply(:"wok_request", :"param", [arg1, arg2])
  end
  def unquote(:"params")(arg1, arg2) do
    :erlang.apply(:"wok_request", :"params", [arg1, arg2])
  end
  def unquote(:"params")(arg1) do
    :erlang.apply(:"wok_request", :"params", [arg1])
  end
end
