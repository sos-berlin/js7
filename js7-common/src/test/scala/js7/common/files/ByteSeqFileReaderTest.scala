package js7.common.files

import cats.effect.IO
import cats.effect.std.Queue
import cats.instances.vector.*
import cats.syntax.foldable.*
import cats.syntax.monoid.*
import cats.syntax.traverse.*
import fs2.Chunk
import java.nio.file.Files
import js7.base.data.{ByteArray, ByteSequence}
import js7.base.fs2utils.Fs2ChunkByteSequence.implicitByteSequence
import js7.base.io.file.FileUtils.*
import js7.base.io.file.FileUtils.syntax.*
import js7.base.test.OurAsyncTestSuite
import js7.base.time.ScalaTime.*
import js7.base.utils.AutoClosing.autoClosing
import js7.common.pekkoutils.ByteStringByteSequence.implicitByteSequence
import org.apache.pekko.util.ByteString
import scala.collection.mutable
import scala.util.Random

/**
  * @author Joacim Zschimmer
  */
final class ByteSeqFileReaderTest extends OurAsyncTestSuite:

  "ByteSeqFileReader[ByteArray]" in:
    testWith[ByteArray]

  "ByteSeqFileReader[fs2.Chunk[Byte]" in:
    testWith[fs2.Chunk[Byte]]

  "ByteSeqFileReader[ByteString]" in:
    // Not used. Just for completeness. And test of Pekko's ByteString.
    testWith[ByteString]

  private def testWith[ByteSeq](using ByteSeq: ByteSequence[ByteSeq]) =
    val content = ByteSeq.unsafeWrap(Random.nextBytes(3 * ByteSeqFileReader.ChunkSize - 7))
    val result = mutable.Buffer.empty[ByteSeq]

    withTemporaryFile("ByteSeqFileReaderTest", ".tmp"): file =>
      file := content
      autoClosing(new ByteSeqFileReader[ByteSeq](file)): reader =>
        var eof = false
        while !eof do
          val byteSeq = reader.read()
          if byteSeq.isEmpty then
            eof = true
          else
            result += byteSeq

    assert(result.toSeq.combineAll == content)

  "stream, not growing" in :
    temporaryFileResource[IO]("ByteSeqFileReaderTest").use: file =>
      val content = Chunk.array(Random.nextBytes(3 * ByteSeqFileReader.ChunkSize - 7))
      file := content
      for bigChunk <- ByteSeqFileReader.fileStream(file).chunks.compile.foldMonoid yield
        assert(bigChunk == content)

  "Stream, growing" in :
    temporaryFileResource[IO]("ByteSeqFileReaderTest").use: file =>
      Queue.unbounded[IO, Chunk[Byte]].flatMap: queue =>
        IO
          .both(
            ByteSeqFileReader.growingFileStream(file, pollDuration = 10.ms)
              .chunks
              .foreach(queue.offer)
              .compile.drain,
            (1 to 50)
              .toVector.traverse: _ =>
                val more = Chunk.array(Random.nextBytes(1 + Random.nextInt(10)))
                for
                  _ <- IO.interruptible:
                    file ++= more
                  chunk <- queue.take.timeout(9.s)
                yield assert(chunk == more)
              .<*(IO.interruptible:
                Files.delete(file)))
          .as(succeed)
