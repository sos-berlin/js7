package js7.common.files

import cats.effect.IO
import cats.instances.vector.*
import cats.syntax.traverse.*
import fs2.Chunk
import js7.base.data.ByteSequence.ops.*
import java.util.concurrent.ArrayBlockingQueue
import js7.base.data.ByteArray
import js7.base.fs2utils.Fs2ChunkByteSequence.*
import js7.base.io.file.FileUtils.*
import js7.base.io.file.FileUtils.syntax.*
import js7.base.test.{OurAsyncTestSuite, OurTestSuite, TestCatsEffect}
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.common.files.GrowingFileStream.growingFileStream
import scala.concurrent.duration.*
import scala.util.Random

/**
  * @author Joacim Zschimmer
  */
final class GrowingFileStreamTest extends OurAsyncTestSuite:

  "GrowingFileStream, not growing" in:
    temporaryFileResource[IO]("GrowingFileStreamTest")
      .use: file =>
        val content = Chunk.array(Random.nextBytes(3 * ChunkFileReader.ChunkSize - 7))
        file := content
        for bigChunk <- growingFileStream(file).chunks.compile.foldMonoid yield
          assert(bigChunk == content)

  "GrowingFileStream, growing" in:
    temporaryFileResource[IO]("GrowingFileStreamTest")
      .use: file =>
        val queue = new ArrayBlockingQueue[Chunk[Byte]](1)
        IO
          .both(
            (1 to 5).toVector.traverse(_ => IO.interruptible:
              val more = Chunk.array(Random.nextBytes(1 + Random.nextInt(10)))
              file ++= more
              assert(queue.poll(9, SECONDS) == more)),
            growingFileStream(file, pollDuration = Some(10.ms))
              .chunks
              .foreach(chunk => IO:
                queue.add(chunk))
              .compile.drain)
          .as(succeed)
