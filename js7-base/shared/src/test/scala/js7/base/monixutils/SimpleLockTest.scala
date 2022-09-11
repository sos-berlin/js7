package js7.base.monixutils

import cats.syntax.parallel._
import js7.base.time.ScalaTime._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.traced
import org.scalatest.freespec.AsyncFreeSpec

final class SimpleLockTest extends AsyncFreeSpec
{
  "SimpleLock x" in {
    val n = 1000
    val lock = new SimpleLock
    var resource = 0
    (1 to n).map(_ =>
      lock
        .lock(
          Task(resource)
            .delayExecution(100.µs)
            .flatMap(x =>
              Task {
                resource = x + 1
              }))
        .start)
      .toVector
      .parTraverse(_.flatMap(_.join))
      .flatMap(_ => Task {
        assert(resource == n)
        succeed
      })
      .runToFuture
  }
}
