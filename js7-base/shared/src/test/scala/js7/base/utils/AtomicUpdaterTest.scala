package js7.base.utils

import cats.syntax.foldable._
import js7.base.utils.AtomicUpdaterTest._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AsyncFreeSpec

final class AtomicUpdaterTest extends AsyncFreeSpec
{
  "update" in {
    val n = 1000
    val atomicUpdater = new AtomicUpdater(0: java.lang.Integer)
    Task
      .parSequenceUnordered(
        (1 to n).map(_ => Task(
          atomicUpdater.update { i => delay(); i + 1 })))
      .map(_.combineAll)
      .map(_ => assert(atomicUpdater.get == n))
      .runToFuture
  }
}

object AtomicUpdaterTest {
  private var x = 0
  private def delay() = for (_ <- 1 to 1000) x += 1
}
