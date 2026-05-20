defmodule Charlex.Command do
  @callback usage() :: [String.t()]

  @callback description() :: String.t()

  @callback parse_args(args :: String.t()) :: any()

  @callback run(context :: Map.t(), args :: [String.t()] | any()) :: String.t() | any()

  @optional_callbacks usage: 0, description: 0, parse_args: 1

  defmacro __using__(_opts) do
    quote do
      @behaviour unquote(__MODULE__)

      @impl true
      def usage, do: "This command is without usage, will add it soon"

      @impl true
      def description, do: "This command is without description, will add it soon"

      @impl true
      def run(_context, _args), do: :ok

      @impl true
      def parse_args(args), do: args |> String.split() |> Kernel.tl()

      defoverridable parse_args: 1, description: 0, run: 2, usage: 0
    end
  end
end
