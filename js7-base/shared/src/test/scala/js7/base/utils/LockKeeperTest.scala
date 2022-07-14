package js7.base.utils

import cats.syntax.traverse.*
import java.util.concurrent.ConcurrentHashMap
import js7.base.time.Stopwatch
import js7.base.utils.LockKeeperTest.*
import monix.eval.Task
import monix.execution.Scheduler.Implicits.traced
import org.scalatest.freespec.AsyncFreeSpec
import scala.jdk.CollectionConverters.*

final class LockKeeperTest extends AsyncFreeSpec
{
  "LockKeeper.lockResource" in {
    val keys = 0 until 8
    val lockKeeper = new LockKeeper[Int]

    (1 to 3).toVector.traverse(_ => Task.defer {
      val locks = keys map lockKeeper.lockResource
      val keyToMap = new ConcurrentHashMap[Int, Int]
      for (k <- keys) keyToMap.put(k, 0)
      val stopwatch = new Stopwatch
      val n = 10000
      (for (_ <- 1 to n; k <- keys) yield
        locks(k).use(_ => Task[Unit] {
          val v = keyToMap.get(k)
          keyToMap.put(k, v + 1)
        }
      )).toVector.sequence.map { _ =>
        logger.info(stopwatch.itemsPerSecondString(n * keys.size, "locks"))
        assert(keyToMap.asScala.toMap == keys.map(k => k -> n).toMap)
      }
    }).map(_.head/*Only the Assertion type is relevant, not the value*/)
      .runToFuture
  }
}

object LockKeeperTest
{
  private val logger = scribe.Logger[this.type]
}
