package js7.common.files

import java.util.concurrent.ArrayBlockingQueue
import js7.base.time.ScalaTime._
import js7.base.utils.ScodecUtils.RichByteVector
import js7.common.scalautil.FileUtils._
import js7.common.scalautil.FileUtils.syntax._
import js7.common.scalautil.MonixUtils.syntax._
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.duration._
import scala.util.Random
import scodec.bits.ByteVector
import scodec.interop.cats._

/**
  * @author Joacim Zschimmer
  */
final class GrowingFileObservableTest extends AnyFreeSpec
{
  "GrowingFileObservable, not growing" in {
    withTemporaryFile("GrowingFileObservableTest", ".tmp") { file =>
      val bytes = ByteVector(Random.alphanumeric.map(_.toByte).take(3 * ByteVectorReader.ChunkSize - 7).toVector)
      file := bytes
      val read = new GrowingFileObservable(file)
        .fold.headL await 9.s
      assert(read == bytes)
    }
  }
  "GrowingFileObservable, growing" in {
    withTemporaryFile("GrowingFileObservableTest", ".tmp") { file =>
      val queue = new ArrayBlockingQueue[String](1)
      new GrowingFileObservable(file, pollDuration = Some(10.ms))
        .foreach(o => queue.add(o.utf8String))
      for (_ <- 1 to 5) {
        val text = Random.nextString(1 + Random.nextInt(10))
        file ++= text
        assert(queue.poll(9, SECONDS) == text)
      }
    }
  }
}
