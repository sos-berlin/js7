package js7.common.files

import cats.effect.IO
import cats.effect.std.Queue
import cats.instances.vector.*
import cats.syntax.traverse.*
import fs2.Chunk
import java.nio.file.Files
import js7.base.fs2utils.Fs2ChunkByteSequence.*
import js7.base.io.file.FileUtils.*
import js7.base.io.file.FileUtils.syntax.*
import js7.base.test.OurAsyncTestSuite
import js7.base.time.ScalaTime.*
import js7.common.files.GrowingFileStream.growingFileStream
import scala.util.Random

/**
  * @author Joacim Zschimmer
  */
final class GrowingFileStreamTest extends OurAsyncTestSuite:

  "GrowingFileStream, not growing" in:
    temporaryFileResource[IO]("GrowingFileStreamTest")
      .use: file =>
        val content = Chunk.array(Random.nextBytes(3 * ByteSeqFileReader.ChunkSize - 7))
        file := content
        for bigChunk <- growingFileStream(file, fromEnd = false).chunks.compile.foldMonoid yield
          assert(bigChunk == content)

  "GrowingFileStream, growing" in:
    temporaryFileResource[IO]("GrowingFileStreamTest")
      .use: file =>
        Queue.unbounded[IO, Chunk[Byte]].flatMap: queue =>
          IO
            .both(
              growingFileStream(file, pollDuration = Some(10.ms), fromEnd = false)
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
