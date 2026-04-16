package js7.base.fs2utils

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import fs2.{Chunk, Stream}
import java.nio.charset.StandardCharsets.UTF_8
import js7.base.data.ByteSequence.ops.*
import js7.base.fs2utils.ByteChunksLineSplitter.{BreakLinesLongerThan, byteChunksToLines}
import js7.base.fs2utils.ByteChunksLineSplitterTest.*
import js7.base.fs2utils.Fs2ChunkByteSequence.implicitByteSequence
import js7.base.log.AnsiEscapeCodes.bold
import js7.base.log.Logger
import js7.base.test.OurAsyncTestSuite
import js7.base.thread.CatsBlocking.syntax.await
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.itemsPerSecondString
import js7.base.utils.ScalaUtils.syntax.foldMap
import js7.base.utils.Tests.isIntelliJIdea

final class ByteChunksLineSplitterTest extends OurAsyncTestSuite:

  private given IORuntime = ioRuntime

  "byteChunksToLines" - {
    "byteChunksToLines" in:
      val lines =
        (Stream("hello\nworld\nanother ") ++ Stream("one\nlast one"))
          .map: string =>
            Chunk.array(string.getBytes(UTF_8))
          .through:
            byteChunksToLines(breakLinesLongerThan = None)
          .map(_.utf8String)
          .map((x: String) => x)
          .toList
      assert(lines == List("hello\n", "world\n", "another one\n", "last one"))

    "Break long lines to avoid memory exhaustion" in:
      val chunks: List[Chunk[Byte]] =
        List(
          """First
            |1  very long łıne
            |2  very long łıne0
            |3  very long łıne01
            |4  very long _ASCII01
            |5  very long łıne012
            |6  very long łıne0123
            |7  very long łıne012⏰
            |8  very long łıne012⏰6
            |9  very long łıne012⏰ä89abcdefghijk
            |10 very long łıne012⏰ä89abcdefghijkl
            |11 very long łıne012⏰ä89abcdefghijklm
            |12 very long łıne012⏰ä89abcdefghijklmn
            |13 very long łıne012⏰ä89abcdefghijklmno
            |""".stripMargin,
         """|Last """.stripMargin,
         """|Finish""".stripMargin
        ).map: string =>
          Chunk.array(string.getBytes(UTF_8))

      Stream.iterable(chunks)
        .covary[IO]
        .through:
          byteChunksToLines(breakLinesLongerThan = Some(20))
        .compile.toList
        .map: lines =>
          assert(lines.map(_.utf8String) == List(
            "First\n",
            "1  very long łıne\n",
            "2  very long ł?↲\n", "\n",
            "3  very long ł?↲\n", ".\n",
            "4  very long _AS↲\n", ".\n",   // only ASCII
            "5  very long ł?↲\n", "..\n",
            "6  very long ł?↲\n", "...\n",
            "7  very long ł?↲\n", "...~~\n",  // "~~" is the remainder of '⏰'
            "8  very long ł?↲\n", "...~~6\n",
            "9  very long ł?↲\n", "...~~ä89abcdefg↲\n", "\n",
            "10 very long ł?↲\n", "...~~ä89abcdefg↲\n", ".\n",
            "11 very long ł?↲\n", "...~~ä89abcdefg↲\n", "..\n",
            "12 very long ł?↲\n", "...~~ä89abcdefg↲\n", "...\n",
            "13 very long ł?↲\n", "...~~ä89abcdefg↲\n", "...o\n",
            "Last Finish"))
          val a = chunks.toArray.flatMap(_.toArray)
          val b = lines.toArray.flatMap(_.toArray)
          assert(a.length == b.length)

          def checkSamePosition(substring: String) =
            val subbytes = substring.getBytes(UTF_8)
            var index = 0
            while !a.slice(index, index + subbytes.length).sameElements(subbytes) do
              index += 1
              if index > a.length then fail(s"Substring '$substring' not found")
            val end = index + substring.getBytes(UTF_8).length
            val a1 = a.slice(index, end)
            val b1 = b.slice(index, end)
            assert(new String(a1, UTF_8) == substring)
            assert(new String(a1, UTF_8) == new String(b1, UTF_8))
            assert(a1 sameElements b1)

          checkSamePosition("2  very long ł")
          checkSamePosition("3  very long ł")
          checkSamePosition("4  very long _AS")
          checkSamePosition("5  very long ł")
          checkSamePosition("6  very long ł")
          checkSamePosition("7  very long ł")
          checkSamePosition("7  very long ł")
          checkSamePosition("8  very long ł")
          checkSamePosition("9  very long ł")
          checkSamePosition("10 very long ł")
          checkSamePosition("11 very long ł")
          checkSamePosition("12 very long ł")
          checkSamePosition("13 very long ł")

    "Speed" in:
      if !isIntelliJIdea then
        IO.pure(succeed)
      else
        val n = 500
        val m = 1000
        val lineCount = n * m
        val testChunks: Seq[Chunk[Byte]] =
          Stream.range(1, n).map: i =>
            Chunk.array(("+" * i + "\n").getBytes(UTF_8))
          .repeatN(m)
          .unchunks
          .chunkN(200)
          .evalMap: chunk =>
            IO.pure(chunk) // Separate chunks
          .compile.toVector
          .await(99.s)

        (1 to 10).foldMap: _ =>
          Stream.fromBlockingIterator[IO](testChunks.iterator, chunkSize = 1)
            .through:
              byteChunksToLines(breakLinesLongerThan = Some(BreakLinesLongerThan))
            .compile.drain
            .timed.map: (duration, _) =>
              logger.info(bold:
                s"byteChunksToLines ${itemsPerSecondString(duration, lineCount, "lines")}")
              succeed
  }


object ByteChunksLineSplitterTest:
  private val logger = Logger[this.type]
