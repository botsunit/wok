defmodule Wok.Mixfile do
  use Mix.Project

  def project do
    [
      app: :wok,
      version: "0.7.2",
      elixir: "~> 1.2",
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      deps: deps(),
      aliases: aliases()
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
      {:lager, "~> 3.2.0"},
      {:lager_json_formatter, "~> 0.2.2"},
      {:kafe, "~> 2.2.1"},
      {:bucs, "~> 1.0.6"},
      {:doteki, "~> 1.0.5"},
      {:uuid, git: "https://github.com/botsunit/erlang-uuid.git", tag: "0.7.2"},
      {:metrics, "~> 2.2.0"}    
    ]
  end

  defp aliases do
    [compile: &compile_with_hooks/1]
  end

  defp compile_with_hooks(args) do
    pre_compile_hooks()
    result = Mix.Task.run("compile", args)
    post_compile_hooks()
    result
  end

  defp pre_compile_hooks() do
    run_hook_cmd [
    ]
  end

  defp post_compile_hooks() do
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