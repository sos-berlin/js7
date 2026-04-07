package js7.base.fs2utils

import cats.effect.IO
import fs2.{Chunk, Pure, Stream}
import java.nio.charset.StandardCharsets.UTF_8
import js7.base.data.ByteArray
import js7.base.data.ByteSequence.nonInheritedOps.toByteSequenceOps
import js7.base.fs2utils.Fs2ChunkByteSequence.implicitByteSequence
import js7.base.fs2utils.Fs2Utils.{combineByteSeqs, toPosAndLines, unfoldEvalWeighted}
import js7.base.test.OurAsyncTestSuite
import js7.base.utils.ScalaUtils.syntax.*

final class Fs2UtilsTest extends OurAsyncTestSuite:

  "toPosAndLines" in:
    val result = Stream("ett\ntvå\ntre", "\nfyra\nfem").map: string =>
      fs2.Chunk.from(string.getBytes(UTF_8))
    .through:
      toPosAndLines(firstPosition = 100)
    .map: (pos, line) =>
      pos -> line.utf8String
    .toList

    assert(result == List(
      100 -> "ett\n",
      104 -> "två\n",
      109 -> "tre\n",
      113 -> "fyra\n",
      118 -> "fem"))

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

  "unfoldEvalWeighted" - {
    "Empty" in:
      val iterator = Iterator.empty[String]
      unfoldEvalWeighted[String](10, _.length)[IO]:
        iterator.hasNext ? iterator.next()
      .compile.toList.map: list =>
        assert(list.isEmpty)

    "Big Strings" in:
      val iterator = Iterator("", "abcdefghijk", "123456789ABC", "abcdefghijk")
      unfoldEvalWeighted[String](10, _.length)[IO]:
        iterator.hasNext ? iterator.next()
      .compile.toList.map: chunks =>
        assert(chunks == List(
          fs2.Chunk("", "abcdefghijk"),
          fs2.Chunk("123456789ABC"),
          fs2.Chunk("abcdefghijk")))

    "Standard" in:
      val iterator =
        Iterator("", "a", "bc", "def", "ghij", "klmnop", "", "xy", "", "123456789ABC", "Z")
      unfoldEvalWeighted[String](10, _.length)[IO]:
        iterator.hasNext ? iterator.next()
      .compile.toList.map: chunks =>
        assert(chunks == List(
          fs2.Chunk("", "a", "bc", "def", "ghij"),
          fs2.Chunk("klmnop", "", "xy", ""),
          fs2.Chunk("123456789ABC"),
          fs2.Chunk("Z")))
  }
