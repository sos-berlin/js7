package js7.base.io.file

import cats.effect.IO
import java.nio.channels.FileChannel
import java.nio.file.StandardOpenOption.READ
import java.nio.file.{Files, Paths}
import js7.base.data.{ByteArray, ByteSequence}
import js7.base.fs2utils.Fs2ChunkByteSequence.implicitByteSequence
import js7.base.io.file.ByteSeqFileReaderTest.*
import js7.base.io.file.FileUtils.*
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.LogFileReader.UniqueHeaderSize
import js7.base.log.Logger
import js7.base.test.OurAsyncTestSuite
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.itemsPerSecondString
import js7.base.utils.Tests.isIntelliJIdea
import scala.concurrent.duration.Deadline
import scala.util.Random

final class ByteSeqFileReaderTest extends OurAsyncTestSuite:

  "ByteSeqFileReader[ByteArray] stream" in:
    testStream[ByteArray]

  "ByteSeqFileReader[fs2.Chunk[Byte] stream" in:
    testStream[fs2.Chunk[Byte]]

  //"ByteSeqFileReader[ByteString]" in:
  //  // Not used. Just for completeness. And test of Pekko's ByteString.
  //  testStream[ByteString]

  private def testStream[ByteSeq](using ByteSeq: ByteSequence[ByteSeq]) =
    temporaryFileResource[IO]("ByteSeqFileReaderTest").use: file =>
      val content = ByteSeq.fromSeq(Random.nextBytes(3 * ByteSeqFileReader.BufferSize - 7))
      file := content
      ByteSeqFileReader.stream[ByteSeq](file, byteChunkSize = ByteSeqFileReader.BufferSize)
        .compile.foldMonoid.map: bigChunk =>
          assert(bigChunk == content)

  "Poll file size speed" in:
    if isIntelliJIdea then
      val file = Paths.get("target/ByteSeqFileReaderTest.tmp")
      try
        file := "SOMETHING"
        val channel = FileChannel.open(file, READ)
        (1 to 3).foreach: _ =>
          val t = Deadline.now
          val n = 1_000_000
          (1 to n).foreach: _ =>
            channel.size()
          logger.info("FileChannel size: " + itemsPerSecondString(t.elapsed, n, "size"))
      finally
        Files.deleteIfExists(file)
    succeed


object ByteSeqFileReaderTest:
  private val logger = Logger[this.type]
  private val Stop = ByteArray("STOP" + "." * UniqueHeaderSize)
