package js7.base.monixutils

import cats.effect.IO
import cats.syntax.parallel.*
import js7.base.test.{OurAsyncTestSuite, OurTestSuite}
import js7.base.time.ScalaTime.*
import org.scalatest.freespec.AsyncFreeSpec

final class SimpleLockTest extends OurAsyncTestSuite

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
      .parTraverse(_.flatMap(_.join))
      .flatMap(_ => IO {
        assert(resource == n)
        succeed
      })
