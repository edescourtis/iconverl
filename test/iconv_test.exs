defmodule IconvTest do
  use ExUnit.Case, async: true

  @latin1 <<116, 101, 115, 116, 233, 116, 101, 115, 116>> # "test\xE9test" in latin1
  @utf8  "testétest"

  test "open/2 ok and error" do
    assert {:ok, _cd} = Iconv.open("utf-8", "latin1")
    assert {:error, :einval} = Iconv.open("utf-99", "ucs-34")
  end

  test "conv/3 one-shot success" do
    assert {:ok, bin} = Iconv.conv("utf-8", "latin1", @latin1)
    assert bin == @utf8
  end

  test "conv/3 one-shot error eilseq" do
    assert {:error, :eilseq} = Iconv.conv("ucs-2be", "utf-8", <<129,129>>)
  end

  test "conv/2 with descriptor" do
    {:ok, cd} = Iconv.open("ucs-2be", "utf-8")
    assert {:ok, <<0, ?t, 0, ?e, 0, ?s, 0, ?t>>} = Iconv.conv(cd, "test")
  end

  test "chunk/2 done and more" do
    {:ok, utf8} = Iconv.conv("utf-8", "latin1", @latin1)
    half = binary_part(utf8, 0, 5)
    {:ok, cd} = Iconv.open("latin1", "utf-8")
    assert {:more, _} = Iconv.chunk(cd, half)
    assert {:done, _} = Iconv.chunk(cd, utf8)
  end

  test "stream/4 converts list of chunks" do
    chunks = ["te", "st", <<195, 169>>, "te", "st"] # utf-8 for "testétest"
    stream = Iconv.stream("latin1", "utf-8", chunks)
    result = Enum.join(Enum.to_list(stream))
    assert result == @latin1
  end

  test "stream raises on eilseq" do
    chunks = [<<129,129>>]
    assert_raise ArgumentError, fn ->
      Iconv.stream("ucs-2be", "utf-8", chunks) |> Enum.to_list()
    end
  end
end
