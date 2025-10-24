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
  Create a Stream that converts elements from `enumerable` from `from` to `to`.

  Each element of `enumerable` should be iodata. The converter buffers input to
  handle multibyte sequences that cross chunk boundaries. Emission behavior is
  configurable via options:

  - `:emit` – `:chunks` (default) or `:lines`
  - `:chunk_size` – for `:chunks`, bytes per yielded chunk (default 4096)
  - `:separator` – for `:lines`, binary separator (default "\n")
  - `:include_separator` – for `:lines`, include the separator in emitted lines (default true)
  - `:finalize` – finalize on end-of-stream and flush remainder (default true)
  """
  @spec stream(String.t() | iodata, String.t() | iodata, Enumerable.t(), keyword) :: Enumerable.t()
  def stream(to, from, enumerable, opts \\ []) do
    finalize? = Keyword.get(opts, :finalize, true)
    emit_mode = Keyword.get(opts, :emit, :chunks)
    chunk_size = Keyword.get(opts, :chunk_size, 4096)
    sep = Keyword.get(opts, :separator, "\n")
    include_sep? = Keyword.get(opts, :include_separator, true)

    Stream.transform(
      enumerable,
      fn ->
        {:ok, cd} = open(to, from)
        %{
          cd: cd,
          pending_in: <<>>,  # pending unconverted input to retry next step
          out_buf: [],       # converted data waiting to be emitted per policy (iodata)
          finalize?: finalize?,
          emit_mode: emit_mode,
          chunk_size: chunk_size,
          sep: sep,
          include_sep?: include_sep?
        }
      end,
      fn chunk, state ->
        %{cd: cd, pending_in: pending_in, out_buf: out_buf} = state
        data = IO.iodata_to_binary([pending_in, chunk])

        {out_buf2, pending_in2} =
          case :iconverl.chunk(cd, data) do
            {:done, out} -> {[out_buf, out], <<>>}
            {:more, out} -> {[out_buf, out], data}
            {:ok, :eilseq, _off, _out} -> raise ArgumentError, "iconv invalid sequence (eilseq)"
            {:error, reason} -> raise "iconv error: #{inspect(reason)}"
          end

        bin = IO.iodata_to_binary(out_buf2)

        {emitted, rest_buf} =
          case state.emit_mode do
            :chunks -> emit_from_buffer_chunks(bin, state.chunk_size)
            :lines -> emit_from_buffer_lines(bin, normalize_sep(state.sep), state.include_sep?)
          end

        {emitted, %{state | out_buf: rest_buf, pending_in: pending_in2}}
      end,
      fn state ->
        %{cd: cd, pending_in: pending_in, out_buf: out_buf, finalize?: fin?} = state

        bin = IO.iodata_to_binary(out_buf)

        leftover =
          case state.emit_mode do
            :chunks -> if bin == <<>>, do: [], else: [bin]
            :lines ->
              {lines, rest} = emit_from_buffer_lines(bin, normalize_sep(state.sep), state.include_sep?)
              if rest != <<>>, do: lines ++ [rest], else: lines
          end

        if fin? do
          if byte_size(pending_in) > 0 do
            raise ArgumentError, "iconv incomplete sequence (einval) at end of stream"
          else
            _ = reset(cd)
            leftover
          end
        else
          leftover
        end
      end
    )
  end

  defp emit_from_buffer_chunks(out_buf, n) when is_integer(n) and n > 0 do
    size = byte_size(out_buf)
    if size < n do
      {[], out_buf}
    else
      {to_emit, rest} = split_fixed(out_buf, n, [])
      {Enum.reverse(to_emit), rest}
    end
  end

  defp split_fixed(<<>>, _n, acc), do: {acc, <<>>}
  defp split_fixed(bin, n, acc) when byte_size(bin) < n, do: {acc, bin}
  defp split_fixed(bin, n, acc) do
    <<chunk::binary-size(n), rest::binary>> = bin
    split_fixed(rest, n, [chunk | acc])
  end

  defp emit_from_buffer_lines(out_buf, sep, include_sep?) when is_binary(sep) do
    segments = :binary.split(out_buf, sep, [:global])
    case segments do
      [_only] -> {[], out_buf}
      _ ->
        rest = List.last(segments)
        lines_wo_sep = Enum.drop_last(segments, 1)
        lines = if include_sep?, do: Enum.map(lines_wo_sep, &(&1 <> sep)), else: lines_wo_sep
        {lines, rest}
    end
  end

  # Normalize special separators (:lf, :crlf, or explicit binary)
  defp normalize_sep(:lf), do: "\n"
  defp normalize_sep(:crlf), do: "\r\n"
  defp normalize_sep(bin) when is_binary(bin), do: bin
  defp normalize_sep(_), do: "\n"

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
