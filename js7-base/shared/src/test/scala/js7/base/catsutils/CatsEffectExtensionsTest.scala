package js7.base.catsutils

import cats.effect.IO
import cats.syntax.option.*
import js7.base.catsutils.CatsEffectExtensions.*
import js7.base.catsutils.CatsEffectExtensionsTest.*
import js7.base.log.Logger
import js7.base.test.OurAsyncTestSuite
import js7.base.time.ScalaTime.*

final class CatsEffectExtensionsTest extends OurAsyncTestSuite:

  "raceFold" - {
    "canceled" in:
      @volatile var canceled = false
      IO.never
        .onCancel(IO {
          canceled = true
          logger.info(s"raceFold canceled")
        })
        .raceFold(IO.sleep(100.ms)/*Wait for .onCancel*/)
        .map(result =>
          assert(result.getClass == classOf[Unit] && canceled))
  }

  "onErrorTap" - {
    "No error" in:
      var tapped = none[Throwable]
      for
        one <- IO(1).onErrorTap(t => IO { tapped = t.some })
      yield assert(one == 1)

    "Tapped exception" in:
      val throwable = new IllegalArgumentException
      var tapped = none[Throwable]
      for attempted <- IO.raiseError(throwable).onErrorTap(t => IO { tapped = t.some }).attempt
      yield assert(attempted.left.exists(_ eq throwable) && (tapped.get eq throwable))

    "matching PartialFunction" in:
      val throwable = new IllegalArgumentException
      var tapped = none[IllegalArgumentException]
      for
        attempted <- IO.raiseError(throwable)
          .onErrorTap:
            case t: IllegalArgumentException => IO { tapped = t.some }
          .attempt
      yield assert(attempted.left.exists(_ eq throwable) && (tapped.get eq throwable))

    "non-matching PartialFunction" in:
      val throwable = new IllegalStateException()
      var tapped = none[IllegalArgumentException]
      for
        attempted <- IO.raiseError(throwable)
          .onErrorTap:
            case t: IllegalArgumentException => IO { tapped = t.some }
          .attempt
      yield assert(attempted.left.exists(_ eq throwable) && tapped == None)
  }

object CatsEffectExtensionsTest:
  private val logger = Logger[this.type]
