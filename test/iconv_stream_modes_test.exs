defmodule IconvStreamModesTest do
  use ExUnit.Case, async: true

  @utf8 IO.iodata_to_binary(["a", <<0xC3>>, <<0xA9>>, "b\n", "c", <<0xC3>>, <<0xA0>>, "d\nend"]) # aéb\ncàd\nend
  @latin1 IO.iodata_to_binary(["a", <<0xE9>>, "b\n", "c", <<0xE0>>, "d\nend"]) # aéb\ncàd\nend

  test "stream emits fixed-size chunks" do
    chunks = [binary_part(@utf8, 0, 2), binary_part(@utf8, 2, 2), binary_part(@utf8, 4, byte_size(@utf8) - 4)]
    stream = Iconv.stream("latin1", "utf-8", chunks, emit: :chunks, chunk_size: 3)
    out = Enum.to_list(stream)
    assert Enum.join(out) == @latin1
    # Expect most chunks of size 3 except the last
    assert Enum.drop(Enum.map(out, &byte_size/1), -1) |> Enum.all?(& &1 == 3)
  end

  test "stream emits lines with separator included" do
    stream = Iconv.stream("latin1", "utf-8", [@utf8], emit: :lines, separator: "\n", include_separator: true)
    out = Enum.to_list(stream)
    assert out == ["aéb\n", "càd\n", "end"]
  end

  test "stream emits lines without separator" do
    stream = Iconv.stream("latin1", "utf-8", [@utf8], emit: :lines, separator: "\n", include_separator: false)
    out = Enum.to_list(stream)
    assert out == ["aéb", "càd", "end"]
  end
end
