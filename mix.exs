defmodule Wok.Mixfile do
  use Mix.Project

  def project do
    [
      app: :wok,
      version: "0.4.2",
      elixir: "~> 1.2",
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      deps: deps
    ]
  end

  def application do
    [
       applications: [:syntax_tools, :compiler, :crypto, :goldrush, :lager, :pipette],
       env: [],
       mod: {:wok_app, []}
    ]
  end

  defp deps do
    [
      {:lager, "~> 3.2"},
      {:lager_json_formatter, "~> 0.0.3"},
      {:wok_http_adapter, git: "git@gitlab.botsunit.com:msaas/wok_http_adapter.git", tag: "0.1.0"},
      {:wok_message_handler, git: "git@gitlab.botsunit.com:msaas/wok_message_handler.git", tag: "0.4.0"},
      {:pipette, git: "git@gitlab.botsunit.com:msaas/pipette.git", tag: "0.1.1"},
      {:kafe, "~> 1.5.0"},
      {:cowboy_default_static_file, git: "https://github.com/botsunit/cowboy_default_static_file.git", tag: "1.2.1"},
      {:cowboy, git: "https://github.com/ninenines/cowboy.git", tag: "2.0.0-pre.3"},
      {:bucs, "~> 0.1.0"},
      {:doteki, "~> 0.1.0"},
      {:uuid, git: "https://github.com/avtobiff/erlang-uuid.git", tag: "v0.5.0"},
      {:tempfile, git: "https://github.com/botsunit/tempfile.git", tag: "1.1.1"}    
    ]
  end
end