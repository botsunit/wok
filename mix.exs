defmodule Wok.Mixfile do
  use Mix.Project

  def project do
    [
      app: :wok,
      version: "0.4.5",
      elixir: "~> 1.2",
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      deps: deps,
      aliases: aliases
    ]
  end

  def application do
    [
       applications: [:syntax_tools, :compiler, :crypto, :goldrush, :lager],
       env: [],
       mod: {:wok_app, []}
    ]
  end

  defp deps do
    [
      {:lager, "~> 3.2"},
      {:lager_json_formatter, "~> 0.0.4"},
      {:pipette, git: "git@gitlab.botsunit.com:msaas/pipette.git", branch: "master"},
      {:kafe, "~> 1.6.0"},
      {:bucs, "~> 0.1.7"},
      {:doteki, "~> 0.1.10"},
      {:uuid, git: "https://github.com/botsunit/erlang-uuid.git", branch: "master"},
      {:wok_message_handler, git: "git@gitlab.botsunit.com:msaas/wok_message_handler.git", branch: "master"}    
    ]
  end

  defp aliases do
    [compile: [&pre_compile_hooks/1, "compile", &post_compile_hooks/1]]
  end

  defp pre_compile_hooks(_) do
    run_hook_cmd [
    ]
  end

  defp post_compile_hooks(_) do
    run_hook_cmd [
    ]
  end

  defp run_hook_cmd(commands) do
    {_, os} = :os.type
    for command <- commands, do: (fn
      ({regex, cmd}) ->
         if Regex.match?(Regex.compile!(regex), Atom.to_string(os)) do
           Mix.Shell.cmd cmd, [], fn(x) -> Mix.Shell.IO.info(String.strip(x)) end
         end
      (cmd) ->
        Mix.Shell.cmd cmd, [], fn(x) -> Mix.Shell.IO.info(String.strip(x)) end
      end).(command)
  end    
end