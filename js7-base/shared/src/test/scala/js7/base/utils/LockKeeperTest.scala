package js7.base.utils

import cats.effect.IO
import cats.syntax.traverse.*
import java.util.concurrent.ConcurrentHashMap
import js7.base.log.Logger
import js7.base.test.OurAsyncTestSuite
import js7.base.time.Stopwatch
import js7.base.utils.LockKeeperTest.*
import scala.jdk.CollectionConverters.*

final class LockKeeperTest extends OurAsyncTestSuite:

  "LockKeeper.lockResource" in:
    val keys = 0 until 8
    val lockKeeper = new LockKeeper[Int]

    (1 to 3).toVector.traverse(_ => IO.defer {
      val locks = keys map lockKeeper.lockResource
      val keyToMap = new ConcurrentHashMap[Int, Int]
      for k <- keys do keyToMap.put(k, 0)
      val stopwatch = new Stopwatch
      val n = 10000
      (for _ <- 1 to n; k <- keys yield
        locks(k).use(_ => IO[Unit] {
          val v = keyToMap.get(k)
          keyToMap.put(k, v + 1)
        }
      )).toVector.sequence.map { _ =>
        logger.info(stopwatch.itemsPerSecondString(n * keys.size, "locks"))
        assert(keyToMap.asScala.toMap == keys.map(k => k -> n).toMap)
      }
    }).map(_.head/*Only the Assertion type is relevant, not the value*/)


object LockKeeperTest:
  private val logger = Logger[this.type]
