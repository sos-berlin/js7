package js7.base.utils

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import cats.syntax.foldable.*
import cats.syntax.traverse.*
import js7.base.test.OurAsyncTestSuite
import js7.base.utils.AtomicUpdaterTest.*

final class AtomicUpdaterTest extends OurAsyncTestSuite:

  private given IORuntime = ioRuntime

  "update" in:
    val n = 1000
    val atomicUpdater = new AtomicUpdater(0: java.lang.Integer)
    val ios: Seq[IO[Unit]] =
      for _ <- 1 to n yield IO:
        atomicUpdater.update:
          i => delay()
          i + 1
    ios.sequence
      .map(_.combineAll)
      .map(_ => assert(atomicUpdater.get == n))
      .unsafeToFuture()


object AtomicUpdaterTest:
  private var x = 0
  private def delay() = for _ <- 1 to 1000 do x += 1
