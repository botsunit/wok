# File: lib/Wok.Response.ex
# This file was generated from src/wok_response.erl
# Using mix.mk (https://github.com/botsunit/mix.mk)
# MODIFY IT AT YOUR OWN RISK AND ONLY IF YOU KNOW WHAT YOU ARE DOING!
defmodule Wok.Response do
  def unquote(:"render")(arg1, arg2) do
    :erlang.apply(:"wok_response", :"render", [arg1, arg2])
  end
  def unquote(:"render")(arg1, arg2, arg3) do
    :erlang.apply(:"wok_response", :"render", [arg1, arg2, arg3])
  end
  def unquote(:"render")(arg1, arg2, arg3, arg4) do
    :erlang.apply(:"wok_response", :"render", [arg1, arg2, arg3, arg4])
  end
  def unquote(:"render")(arg1, arg2, arg3, arg4, arg5) do
    :erlang.apply(:"wok_response", :"render", [arg1, arg2, arg3, arg4, arg5])
  end
  def unquote(:"redirect")(arg1, arg2) do
    :erlang.apply(:"wok_response", :"redirect", [arg1, arg2])
  end
  def unquote(:"set_cookie")(arg1, arg2, arg3) do
    :erlang.apply(:"wok_response", :"set_cookie", [arg1, arg2, arg3])
  end
  def unquote(:"set_cookie")(arg1, arg2, arg3, arg4) do
    :erlang.apply(:"wok_response", :"set_cookie", [arg1, arg2, arg3, arg4])
  end
  def unquote(:"delete_cookie")(arg1, arg2) do
    :erlang.apply(:"wok_response", :"delete_cookie", [arg1, arg2])
  end
  def unquote(:"set_response")(arg1, arg2) do
    :erlang.apply(:"wok_response", :"set_response", [arg1, arg2])
  end
end
