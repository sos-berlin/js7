package js7.base.utils

import cats.effect.IO
import cats.syntax.all.*
import js7.base.test.OurAsyncTestSuite
import js7.base.utils.Atomic.extensions.*

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
      .unsafeToFuture()
