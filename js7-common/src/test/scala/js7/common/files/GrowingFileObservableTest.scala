package js7.common.files

import java.util.concurrent.ArrayBlockingQueue
import js7.base.data.ByteArray
import js7.base.io.file.FileUtils.*
import js7.base.io.file.FileUtils.syntax.*
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import monix.execution.Scheduler.Implicits.traced
import scala.concurrent.duration.*
import scala.util.Random

/**
  * @author Joacim Zschimmer
  */
final class GrowingFileStreamTest extends OurTestSuite:
  "GrowingFileStream, not growing" in:
    withTemporaryFile("GrowingFileStreamTest", ".tmp") { file =>
      val bytes = ByteArray.fromSeq(Random.alphanumeric.map(_.toByte).take(3 * ByteArrayReader.ChunkSize - 7))
      file := bytes
      val read = new GrowingFileStream(file)
        .fold.headL await 9.s
      assert(read == bytes)
    }
  "GrowingFileStream, growing" in:
    withTemporaryFile("GrowingFileStreamTest", ".tmp") { file =>
      val queue = new ArrayBlockingQueue[String](1)
      new GrowingFileStream(file, pollDuration = Some(10.ms))
        .foreach(o => queue.add(o.utf8String))
      for _ <- 1 to 5 do
        val text = Random.nextString(1 + Random.nextInt(10))
        file ++= text
        assert(queue.poll(9, SECONDS) == text)
    }
