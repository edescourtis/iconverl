defmodule Iconv do
  @moduledoc """
  Elixir wrapper around the `iconverl` Erlang NIF.

  Provides simple conversion helpers and Stream-based chunked conversion.
  """

  @type cd :: reference()

  @doc """
  Open a conversion descriptor from `from` encoding to `to` encoding.
  Returns `{:ok, cd}` or `{:error, reason}`.
  """
  @spec open(String.t() | iodata(), String.t() | iodata()) :: {:ok, cd} | {:error, term}
  def open(to, from) do
    case :iconverl.open(to, from) do
      {:error, :unsupported} -> {:error, :einval}
      {:error, _} = err -> err
      cd -> {:ok, cd}
    end
  end

  @doc """
  Convert `data` using a conversion descriptor.
  """
  @spec conv(cd, iodata) :: {:ok, binary} | {:error, term}
  def conv(cd, data), do: :iconverl.conv(cd, data)

  @doc """
  Convert `data` from `from` to `to` encoding in one call.
  """
  @spec conv(String.t() | iodata(), String.t() | iodata(), iodata) :: {:ok, binary} | {:error, term}
  def conv(to, from, data), do: :iconverl.conv(to, from, data)

  @doc """
  Reset the conversion descriptor state.
  """
  @spec reset(cd) :: :ok | {:error, term}
  def reset(cd), do: :iconverl.reset(cd)

  @doc """
  Returns true if the converter is configured to ignore invalid sequences.
  """
  @spec ignores(cd) :: boolean
  def ignores(cd), do: :iconverl.ignores(cd) == :true

  @doc """
  Chunk-convert `data` returning either `{:done, iodata}` or `{:more, iodata}`.
  """
  @spec chunk(cd, iodata) :: {:done, iodata} | {:more, iodata} | {:error, atom} | {:ok, :eilseq, non_neg_integer, binary}
  def chunk(cd, data), do: :iconverl.chunk(cd, data)

  @doc """
  Create a Stream that converts chunks from `from` to `to` encoding.

  Options:
  - `:chunk_size` - size in bytes to accumulate before converting (default 4096)
  - `:finalize` - whether to finalize and flush at the end (default true)
  """
  @spec stream(String.t() | iodata, String.t() | iodata, Enumerable.t(), keyword) :: Enumerable.t()
  def stream(to, from, enumerable, opts \\ []) do
    finalize? = Keyword.get(opts, :finalize, true)

    Stream.transform(
      enumerable,
      fn ->
        {:ok, cd} = open(to, from)
        %{cd: cd, pending: <<>>, finalize?: finalize?}
      end,
      fn chunk, %{cd: cd, pending: pending} = state ->
        data = IO.iodata_to_binary([pending, chunk])

        case :iconverl.chunk(cd, data) do
          {:done, out} ->
            {[IO.iodata_to_binary(out)], %{state | pending: <<>>}}

          {:more, out} ->
            _ = out
            {[], %{state | pending: data}}

          {:ok, :eilseq, _off, _out} ->
            raise ArgumentError, "iconv invalid sequence (eilseq)"

          {:error, reason} ->
            raise "iconv error: #{inspect(reason)}"
        end
      end,
      fn %{cd: cd, pending: pending, finalize?: fin?} ->
        if fin? do
          if byte_size(pending) > 0 do
            raise ArgumentError, "iconv incomplete sequence (einval) at end of stream"
          else
            _ = reset(cd)
            []
          end
        else
          []
        end
      end
    )
  end

  defp convert_until_emit(%{cd: cd, buffer: buffer, chunk_size: chunk_size} = state, chunks) do
    {to_convert, rest} = take_bytes(buffer, chunks, chunk_size)
    case :iconverl.chunk(cd, to_convert) do
      {:done, out} -> {[IO.iodata_to_binary(out)], %{state | buffer: rest}}
      {:more, out} ->
        new_buf = IO.iodata_to_binary([out, rest])
        {[], %{state | buffer: new_buf}}
      {:ok, :eilseq, _off, out} -> {[IO.iodata_to_binary(out)], %{state | buffer: rest}}
      {:error, _} = err -> raise "iconv error: #{inspect(err)}"
    end
  end

  defp take_bytes(buffer, chunks, n) do
    bin = IO.iodata_to_binary([buffer | Enum.take(chunks, 1)])
    if byte_size(bin) >= n do
      <<to::binary-size(n), rest::binary>> = bin
      {to, rest}
    else
      {bin, <<>>}
    end
  end
end
