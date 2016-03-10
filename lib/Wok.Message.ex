# File: lib/Wok.Message.ex
# This file was generated from src/wok_message.erl
# Using mix.mk (https://github.com/botsunit/mix.mk)
# MODIFY IT AT YOUR OWN RISK AND ONLY IF YOU KNOW WHAT YOU ARE DOING!
defmodule Wok.Message do
	def unquote(:"content")(arg1) do
		:erlang.apply(:"wok_message", :"content", [arg1])
	end
	def unquote(:"content_has_map")(arg1) do
		:erlang.apply(:"wok_message", :"content_has_map", [arg1])
	end
	def unquote(:"content")(arg1, arg2) do
		:erlang.apply(:"wok_message", :"content", [arg1, arg2])
	end
	def unquote(:"uuid")(arg1) do
		:erlang.apply(:"wok_message", :"uuid", [arg1])
	end
	def unquote(:"from")(arg1) do
		:erlang.apply(:"wok_message", :"from", [arg1])
	end
	def unquote(:"to")(arg1) do
		:erlang.apply(:"wok_message", :"to", [arg1])
	end
	def unquote(:"headers")(arg1) do
		:erlang.apply(:"wok_message", :"headers", [arg1])
	end
	def unquote(:"body")(arg1) do
		:erlang.apply(:"wok_message", :"body", [arg1])
	end
	def unquote(:"global_state")(arg1) do
		:erlang.apply(:"wok_message", :"global_state", [arg1])
	end
	def unquote(:"local_state")(arg1) do
		:erlang.apply(:"wok_message", :"local_state", [arg1])
	end
	def unquote(:"custom_data")(arg1) do
		:erlang.apply(:"wok_message", :"custom_data", [arg1])
	end
	def unquote(:"custom_data")(arg1, arg2) do
		:erlang.apply(:"wok_message", :"custom_data", [arg1, arg2])
	end
	def unquote(:"noreply")(arg1) do
		:erlang.apply(:"wok_message", :"noreply", [arg1])
	end
	def unquote(:"reply")(arg1, arg2, arg3, arg4) do
		:erlang.apply(:"wok_message", :"reply", [arg1, arg2, arg3, arg4])
	end
	def unquote(:"reply")(arg1, arg2, arg3, arg4, arg5) do
		:erlang.apply(:"wok_message", :"reply", [arg1, arg2, arg3, arg4, arg5])
	end
	def unquote(:"provide")(arg1, arg2, arg3, arg4) do
		:erlang.apply(:"wok_message", :"provide", [arg1, arg2, arg3, arg4])
	end
	def unquote(:"provide")(arg1, arg2, arg3, arg4, arg5) do
		:erlang.apply(:"wok_message", :"provide", [arg1, arg2, arg3, arg4, arg5])
	end
	def unquote(:"provide")(arg1, arg2) do
		:erlang.apply(:"wok_message", :"provide", [arg1, arg2])
	end
end
