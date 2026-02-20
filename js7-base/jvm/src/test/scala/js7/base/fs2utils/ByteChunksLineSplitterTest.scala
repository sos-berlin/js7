package js7.base.fs2utils

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import fs2.{Chunk, Stream}
import java.nio.charset.StandardCharsets.UTF_8
import js7.base.data.ByteSequence.ops.*
import js7.base.fs2utils.ByteChunksLineSplitter.byteChunksToLines
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
      (Stream("hello\nworld\nanother ").covary[IO] ++ Stream("one\nlast one").covary[IO])
        .map: string =>
          Chunk.array(string.getBytes(UTF_8))
      .through:
        byteChunksToLines
      .map(_.utf8String)
      .map((x: String) => x)
      .compile.toList
      .map: lines =>
        lines.map(_.replace('\n', '|'))
      .map: lines =>
        assert(lines == List("hello|", "world|", "another one|", "last one"))

    "Speed" in:
      if !isIntelliJIdea then
        IO.pure(succeed)
      else
        val testChunks =
          Stream.range(1, 500).map: i =>
            Chunk.array(("+" * i + "\n").getBytes(UTF_8))
          .repeatN(100)
          .unchunks
          .chunkN(200)
          .evalMap: chunk =>
            IO.pure(chunk) // Separate chunks
          .compile.toVector
          .await(99.s)

        (1 to 10).foldMap: _ =>
          Stream.fromBlockingIterator[IO](testChunks.iterator, chunkSize = 1)
            .through:
              byteChunksToLines
            .compile.drain
            .timed.map: (duration, _) =>
              logger.info(bold:
                s"byteChunksToLines ${itemsPerSecondString(duration, testChunks.size, "chunks")}")
              succeed
  }


object ByteChunksLineSplitterTest:
  private val logger = Logger[this.type]
