package js7.base.utils

import cats.syntax.all.*
import js7.base.test.OurAsyncTestSuite
import monix.eval.Task
import monix.execution.Scheduler.Implicits.traced
import monix.execution.atomic.Atomic

final class OnceTest extends OurAsyncTestSuite {

  "parallel" in {
    val number = Atomic(0)
    val once = new Once

    (1 to 1000_000)
      .toVector
      .parTraverse(_ => Task(once {
        number += 1
      }))
      .void
      .*>(Task(assert(number.get() == 1)))
      .runToFuture
  }
}
