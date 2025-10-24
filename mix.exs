defmodule Iconverl.MixProject do
  use Mix.Project

  def project do
    [
      app: :iconverl,
      version: "3.0.21",
      elixir: ">= 1.12",
      start_permanent: Mix.env() == :prod,
      compilers: [:elixir_make] ++ Mix.compilers(),
      make_targets: ["priv/iconverl.so"],
      make_clean: ["clean"],
      erlc_paths: ["src"],
      deps: deps(),
      description: "Erlang NIF library for iconv with Elixir wrapper",
      package: [licenses: ["MIT"]]
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp deps do
    [
      {:elixir_make, "~> 0.8", runtime: false}
    ]
  end
end
