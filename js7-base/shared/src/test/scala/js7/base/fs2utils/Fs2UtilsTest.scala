package js7.base.fs2utils

import fs2.{Pure, Stream, Chunk}
import js7.base.data.ByteArray
import js7.base.fs2utils.Fs2Utils.combineByteSeqs
import js7.base.test.OurTestSuite

final class Fs2UtilsTest extends OurTestSuite:

  "combineByteSeqs" - {
    "empty" in:
      val stream: Stream[Pure, ByteArray] =
        Stream.emits(Seq[Chunk[String]]())
          .unchunks
          .map(ByteArray(_))
          .through:
            combineByteSeqs(limit = 3)

      assert(stream.toList.isEmpty)

    "empty Chunks" in:
      val stream: Stream[Pure, ByteArray] =
        Stream.emits:
          Seq(
            Chunk(""),
            Chunk("", "", ""))
        .unchunks
        .map(ByteArray(_))
        .through:
          combineByteSeqs(limit = 3)

      assert(stream.toList.isEmpty)

    "empty Strings" in:
      val stream: Stream[Pure, ByteArray] =
        Stream.emits:
          Seq(
            Chunk(""),
            Chunk(""))
        .unchunks
        .map(ByteArray(_))
        .through:
          combineByteSeqs(limit = 3)

      assert(stream.toList.isEmpty)

    "lines" in:
      val stream: Stream[Pure, ByteArray] =
        Stream.emits:
          Seq(
            Chunk(line("1")),
            Chunk(line("23"), line(""), line("456")),
            Chunk(line("78")),
            Chunk(line("90"), line("abc"), line("defghik")),
            Chunk.empty,
            Chunk(line("m"), line("nopqrstuvwxyz")))
        .unchunks
        .through:
          combineByteSeqs(limit = 3)

      assert(stream.toList == List(
        "1\n",
        "23\n",
        "\n",
        "456\n",
        "78\n",
        "90\n",
        "abc\n",
        "defghik\n",
        "m\n",
        "nopqrstuvwxyz\n"
      ).map(ByteArray(_)))

    "combineByteString, total is shorter than limit" in:
      val stream: Stream[Pure, ByteArray] =
        Stream.emit(ByteArray("123"))
          .through:
            combineByteSeqs(limit = 100)

      assert(stream.toList == List("123").map(ByteArray(_)))

    def line(string: String) =
      ByteArray(string + "\n")
  }
