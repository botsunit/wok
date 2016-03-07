# File: lib/Wok.Routes.ex
# This file was generated from src/wok_routes.erl
# Using mix.mk (https://github.com/botsunit/mix.mk)
# MODIFY IT AT YOUR OWN RISK AND ONLY IF YOU KNOW WHAT YOU ARE DOING!
defmodule Wok.Routes do
	def unquote(:"static")() do
		:erlang.apply(:"wok_routes", :"static", [])
	end
	def unquote(:"static")(arg1) do
		:erlang.apply(:"wok_routes", :"static", [arg1])
	end
	def unquote(:"path")(arg1, arg2) do
		:erlang.apply(:"wok_routes", :"path", [arg1, arg2])
	end
	def unquote(:"path")(arg1, arg2, arg3) do
		:erlang.apply(:"wok_routes", :"path", [arg1, arg2, arg3])
	end
	def unquote(:"path")(arg1, arg2, arg3, arg4) do
		:erlang.apply(:"wok_routes", :"path", [arg1, arg2, arg3, arg4])
	end
	def unquote(:"paths")(arg1, arg2) do
		:erlang.apply(:"wok_routes", :"paths", [arg1, arg2])
	end
end
