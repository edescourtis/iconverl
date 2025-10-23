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
    chunk_size = Keyword.get(opts, :chunk_size, 4096)
    finalize? = Keyword.get(opts, :finalize, true)

    Stream.resource(
      fn ->
        {:ok, cd} = open(to, from)
        %{cd: cd, buffer: <<>>, chunk_size: chunk_size, done: false}
      end,
      fn %{done: true} = state ->
        {:halt, state}
        
      state ->
        next_chunk =
          case Enum.fetch(enumerable, 0) do
            {:ok, _} -> enumerable
            :error -> []
          end

        case next_chunk do
          [] ->
            state =
              if finalize? do
                case reset(state.cd) do
                  :ok -> %{state | done: true}
                  {:error, _} -> %{state | done: true}
                end
              else
                %{state | done: true}
              end
            {[], state}

          _ ->
            {emitted, new_state} = convert_until_emit(state, next_chunk)
            {emitted, new_state}
        end
      end,
      fn _ -> :ok end
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
