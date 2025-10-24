defmodule IconvStreamCrlfTest do
  use ExUnit.Case, async: true

  test "lines with CRLF separator included" do
    utf8 = IO.iodata_to_binary(["a", <<0xC3>>, <<0xA9>>, "b\r\n", "c", <<0xC3>>, <<0xA0>>, "d\r\nend"]) # aéb\r\ncàd\r\nend
    latin1 = IO.iodata_to_binary(["a", <<0xE9>>, "b\r\n", "c", <<0xE0>>, "d\r\nend"]) # aéb\r\ncàd\r\nend

    stream = Iconv.stream("latin1", "utf-8", [utf8], emit: :lines, separator: :crlf, include_separator: true)
    out = Enum.to_list(stream)
    assert out == ["aéb\r\n", "càd\r\n", "end"]
    assert Enum.join(out) == latin1
  end

  test "lines auto-detect CRLF and LF, include separators" do
    utf8 = IO.iodata_to_binary(["a\r\n", "b\n", "c\r\n", "d"]) # mixed endings
    stream = Iconv.stream("latin1", "utf-8", [utf8], emit: :lines, include_separator: true)
    out = Enum.to_list(stream)
    assert out == ["a\r\n", "b\n", "c\r\n", "d"]
  end
end
