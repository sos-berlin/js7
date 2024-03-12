package js7.base.monixutils

import cats.effect.IO
import cats.syntax.parallel.*
import js7.base.test.OurAsyncTestSuite
import js7.base.time.ScalaTime.*

final class SimpleLockTest extends OurAsyncTestSuite:

  "SimpleLock" in:
    val n = 1000
    val lock = SimpleLock[IO]
    var resource = 0
    (1 to n).map(_ =>
      lock
        .surround(
          IO(resource)
            .delayBy(100.µs)
            .flatMap(x =>
              IO {
                resource = x + 1
              }))
        .start)
      .toVector
      .parTraverse(_.flatMap(_.joinWith(IO.raiseError(new Exception("Canceled?")))))
      .flatMap(_ => IO {
        assert(resource == n)
        succeed
      })
