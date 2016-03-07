defmodule Wok.Mixfile do
	use Mix.Project

	def project do
		[app: :wok,
		 version: "0.0.1",
		 elixir: "~> 1.2",
		 build_embedded: Mix.env == :prod,
		 start_permanent: Mix.env == :prod,
		 deps: deps]
	end

	def application do
		[applications: [:syntax_tools,:compiler,:crypto,:goldrush,:lager,:pipette], mod: {:wok_app, []}]
	end

	defp deps do
		[
			{:lager, ~r/.*/, git: "https://github.com/basho/lager.git", branch: "master"},
			{:wok_message, ~r/.*/, git: "git@gitlab.botsunit.com:msaas/wok_message.git", branch: "master"},
			{:wok_message_handler, ~r/.*/, git: "git@gitlab.botsunit.com:msaas/wok_message_handler.git", branch: "master"},
			{:wok_producer, ~r/.*/, git: "git@gitlab.botsunit.com:msaas/wok_producer.git", branch: "master"},
			{:pipette, ~r/.*/, git: "git@gitlab.botsunit.com:msaas/pipette.git", branch: "master"},
			{:kafe, ~r/.*/, git: "https://github.com/botsunit/kafe.git", branch: "master"},
			{:cowboy, ~r/.*/, git: "https://github.com/ninenines/cowboy.git", branch: "2.0.0-pre.3"},
			{:cowboy_default_static_file, ~r/.*/, git: "https://github.com/botsunit/cowboy_default_static_file.git", branch: "master"},
			{:bucs, ~r/.*/, git: "https://github.com/botsunit/bucs.git", branch: "master"},
			{:doteki, ~r/.*/, git: "https://github.com/botsunit/doteki.git", branch: "master"},
			{:uuid, ~r/.*/, git: "https://github.com/avtobiff/erlang-uuid.git", branch: "master"},
		]
	end
end
