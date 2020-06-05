package js7.core.event.state

import java.util.concurrent.ConcurrentHashMap
import js7.base.time.ScalaTime._
import js7.base.time.Stopwatch
import js7.common.scalautil.Futures.implicits._
import js7.common.scalautil.Logger
import js7.core.event.state.LockKeeperTest._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.Future
import scala.jdk.CollectionConverters._

/**
  * @author Joacim Zschimmer
  */
final class LockKeeperTest extends AnyFreeSpec
{
  "LockKeeper.lock" in {
    val keys = 0 until 8
    val lockKeeper = new LockKeeper[Int]

    for (_ <- 1 to 3) {
      val locks = keys map lockKeeper.lock
      val keyToMap = new ConcurrentHashMap[Int, Int]
      for (k <- keys) keyToMap.put(k, 0)
      val stopwatch = new Stopwatch
      val n = 10000
      (for (_ <- 1 to n; k <- keys) yield
        locks(k).use(_ => Task[Unit] {
          val v = keyToMap.get(k)
          keyToMap.put(k, v + 1)
        }).runToFuture: Future[Unit]) await 99.s
      logger.info(stopwatch.itemsPerSecondString(n * keys.size, "locks"))
      assert(keyToMap.asScala.toMap == keys.map(k => k -> n).toMap)
    }
  }
}

object LockKeeperTest
{
  private val logger = Logger(getClass)
}
