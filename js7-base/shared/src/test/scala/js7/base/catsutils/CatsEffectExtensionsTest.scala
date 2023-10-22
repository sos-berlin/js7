package js7.base.catsutils

import cats.effect.IO
import cats.effect.unsafe.IORuntime.global
import js7.base.catsutils.CatsEffectExtensions.*
import js7.base.catsutils.CatsEffectExtensionsTest.*
import js7.base.log.Logger
import js7.base.test.OurTestSuite
import org.scalatest.freespec.AnyFreeSpec

final class CatsEffectExtensionsTest extends OurTestSuite

  "raceFold" - :
    "canceled" in:
      @volatile var canceled = false
      IO.never
        .onCancel(IO {
          canceled = true
          logger.info(s"raceFold canceled")
        })
        .raceFold(IO.unit)
        .map(result =>
          assert(result.getClass == classOf[Unit] && canceled/*NOT RELIABLE ???*/))
        .unsafeToFuture()

object CatsEffectExtensionsTest:
  private val logger = Logger[this.type]
