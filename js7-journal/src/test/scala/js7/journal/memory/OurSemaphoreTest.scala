package js7.journal.memory

import cats.effect.IO
import cats.effect.kernel.Deferred
import js7.base.test.OurAsyncTestSuite
import js7.base.time.ScalaTime.*

final class OurSemaphoreTest extends OurAsyncTestSuite:

  "test" in:
    for
      sema <- OurSemaphore(size = 10)
      _ <- sema.available.map(n => assert(n == 10))
      _ <- sema.acquireN(8)
      _ <- sema.available.map(n => assert(n == 2))

      _ <- sema.releaseN(3)
      _ <- sema.available.map(n => assert(n == 5))

      // Acquire 7, more than available
      a <- Deferred[IO, Unit]
      _ <- (sema.acquireN(7) *> a.complete(())).start
      _ <- IO.sleep(1.ms)
      _ <- a.tryGet.map(maybe => assert(maybe.isEmpty))
      _ <- sema.available.map(n => assert(n == 5))

      // Release 1. Now, 6 are available
      _ <- sema.releaseN(1)
      _ <- IO.sleep(1.ms)
      _ <- a.tryGet.map(maybe => assert(maybe.isEmpty))
      _ <- sema.available.map(n => assert(n == 6))

      // Release 1. Now, 7 are available. This is enough for the pending acquireN
      _ <- sema.releaseN(1)
      _ <- a.get
      _ <- sema.available.map(n => assert(n == 0))

      // Acquire 11, more than size
      a <- Deferred[IO, Unit]
      _ <- (sema.acquireN(11) *> a.complete(())).start
      _ <- IO.sleep(1.ms)
      _ <- a.tryGet.map(maybe => assert(maybe.isEmpty))

      // Release to little
      _ <- sema.releaseN(9)
      _ <- IO.sleep(1.ms)
      _ <- a.tryGet.map(maybe => assert(maybe.isEmpty))
      _ <- sema.available.map(n => assert(n == 9))

      // Release remaining. Now, acquireN(11) will be fullfilled
      _ <- sema.releaseN(1)
      _ <- a.get
      _ <- sema.available.map(n => assert(n == -1))

      _ <- sema.releaseN(1)
      _ <- sema.available.map(n => assert(n == 0))

      _ <- sema.releaseN(10)
      _ <- sema.available.map(n => assert(n == 10))
    yield succeed
