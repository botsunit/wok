defmodule Wok.Mixfile do
  use Mix.Project

  def project do
    [
      app: :wok,
      version: "0.4.0",
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
      {:lager, "~> 3.2.0"},
      {:wok_http_adapter, git: "git@gitlab.botsunit.com:msaas/wok_http_adapter.git", branch: "master"},
      {:wok_message_handler, git: "git@gitlab.botsunit.com:msaas/wok_message_handler.git", branch: "master"},
      {:pipette, git: "git@gitlab.botsunit.com:msaas/pipette.git", branch: "master"},
      {:kafe, git: "https://github.com/botsunit/kafe.git", branch: "master"},
      {:cowboy_default_static_file, git: "https://github.com/botsunit/cowboy_default_static_file.git", branch: "master"},
      {:cowboy, git: "https://github.com/ninenines/cowboy.git", tag: "2.0.0-pre.3"},
      {:bucs, git: "https://github.com/botsunit/bucs.git", branch: "master"},
      {:doteki, git: "https://github.com/botsunit/doteki.git", branch: "master"},
      {:uuid, git: "https://github.com/avtobiff/erlang-uuid.git", tag: "v0.5.0"},
      {:tempfile, git: "https://github.com/botsunit/tempfile.git", branch: "master"}    
    ]
  end
end