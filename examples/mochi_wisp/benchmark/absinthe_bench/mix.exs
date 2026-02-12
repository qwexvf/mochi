defmodule AbsintheBench.MixProject do
  use Mix.Project

  def project do
    [
      app: :absinthe_bench,
      version: "0.1.0",
      elixir: "~> 1.14",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger],
      mod: {AbsintheBench.Application, []}
    ]
  end

  defp deps do
    [
      {:plug_cowboy, "~> 2.6"},
      {:absinthe, "~> 1.7"},
      {:absinthe_plug, "~> 1.5"},
      {:jason, "~> 1.4"}
    ]
  end
end
