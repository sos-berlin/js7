package js7.base.utils

import cats.syntax.all.*
import js7.base.test.OurAsyncTestSuite
import js7.base.utils.Atomic.extensions.*
import cats.effect.IO
import monix.execution.Scheduler.Implicits.traced

final class OnceTest extends OurAsyncTestSuite:

  "parallel" in:
    val number = Atomic(0)
    val once = new Once

    (1 to 1000_000)
      .toVector
      .parTraverse(_ => IO(once {
        number += 1
      }))
      .void
      .*>(IO(assert(number.get() == 1)))
      .runToFuture
