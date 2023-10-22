package js7.base.utils

import cats.syntax.foldable.*
import js7.base.utils.AtomicUpdaterTest.*
import cats.effect.IO
import monix.execution.Scheduler.Implicits.traced
import js7.base.test.OurAsyncTestSuite

final class AtomicUpdaterTest extends OurAsyncTestSuite:
  "update" in:
    val n = 1000
    val atomicUpdater = new AtomicUpdater(0: java.lang.Integer)
    IO
      .parSequenceUnordered(
        (1 to n).map(_ => IO(
          atomicUpdater.update { i => delay(); i + 1 })))
      .map(_.combineAll)
      .map(_ => assert(atomicUpdater.get == n))
      .runToFuture


object AtomicUpdaterTest:
  private var x = 0
  private def delay() = for _ <- 1 to 1000 do x += 1
