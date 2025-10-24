defmodule IconvStreamTest do
  use ExUnit.Case, async: true

  setup_all do
    Application.ensure_all_started(:crypto)
    :ok
  end

  defp to_bin(chunks), do: IO.iodata_to_binary(chunks)

  test "stream handles cross-chunk UTF-8 sequence boundary" do
    utf8 = IO.iodata_to_binary(["abc", <<0xC3>>, <<0xA9>>, "def"]) # "abcédef"
    latin1 = Iconv.stream("latin1", "utf-8", ["abc", <<0xC3>>, <<0xA9>>, "def"]) |> Enum.join()
    assert latin1 == to_bin(["abc", <<0xE9>>, "def"]) # 0xE9 is é in latin1

    # Now force cross-chunk: first byte then second byte of é
    latin1_2 = Iconv.stream("latin1", "utf-8", ["abc", <<0xC3>>, <<0xA9, ?d, ?e, ?f>>]) |> Enum.join()
    assert latin1_2 == to_bin(["abc", <<0xE9>>, "def"])    
  end

  test "ignore and translit options behave" do
    # ASCII//ignore drops unsupported codepoints
    {:ok, bin} = Iconv.conv("ASCII//ignore", "UTF-8", <<97,226,152,160,98>>) # a U+2020 b
    assert bin == "ab"

    # ascii//translit//ignore transliterates and ignores
    {:ok, bin2} = Iconv.conv("ascii//translit//ignore", "utf-8", <<195, 168, 56, 195, 139>>) # "è8Ë"
    assert bin2 == "e8E"
  end

  test "random data with ignore/translit never grows" do
    input = :crypto.strong_rand_bytes(32768)
    {:ok, result} = Iconv.conv("latin1//translit//ignore", "utf-8", input)
    assert byte_size(result) <= byte_size(input)
  end

  test "chunk returns more/done patterns" do
    {:ok, utf8} = Iconv.conv("utf-8", "latin1", IO.iodata_to_binary(["test", <<0xE9>>, "test"]))
    first = binary_part(utf8, 0, 5)
    {:ok, cd} = Iconv.open("latin1", "utf-8")
    assert {:more, _} = Iconv.chunk(cd, first)
    assert {:done, _} = Iconv.chunk(cd, utf8)
  end

  test "stream raises on eilseq" do
    assert_raise ArgumentError, fn ->
      Iconv.stream("ucs-2be", "utf-8", [<<129,129>>]) |> Enum.to_list()
    end
  end

  test "one-shot ucs-2be conversion" do
    {:ok, <<0, ?t, 0, ?e, 0, ?s, 0, ?t>> = out} = Iconv.conv("ucs-2be", "utf-8", "test")
    assert is_binary(out)
  end
end
