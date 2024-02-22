package js7.base.catsutils

import cats.effect.Resource.ExitCase
import cats.effect.{IO, OutcomeIO, Resource, SyncIO}
import cats.syntax.option.*
import js7.base.catsutils.CatsEffectExtensions.*
import js7.base.test.OurAsyncTestSuite
import js7.base.time.ScalaTime.*
import js7.base.utils.Atomic
import js7.base.utils.Atomic.extensions.*
import scala.collection.mutable

final class CatsEffectExtensionsTest extends OurAsyncTestSuite:

  "IO" - {
    "guaranteeCaseLazy" - {
      "ERROR: guaranteeCase evaluates the Canceled case eagerly even when not Canceled" in:
        val evaluatedCases = mutable.Buffer.empty[OutcomeIO[Int]]

        IO(7)
          .guaranteeCase: outcome =>
            evaluatedCases += outcome
            IO.unit
          .map: _ =>
            assert(evaluatedCases.toString == "ArrayBuffer(Canceled(), Succeeded(IO(7)))")

      "guaranteeCaseLazy evalutes the Canceled case only when Canceled" in:
        val outcomes = mutable.Buffer.empty[OutcomeIO[Int]]

        IO(7)
          .guaranteeCaseLazy: outcome =>
            outcomes += outcome
            IO.unit
          .map: _ =>
            assert(outcomes.toString == "ArrayBuffer(Succeeded(IO(7)))")
    }

    "onCancelLazy" - {
      "ERROR: onCancel evaluates the handler even when not canceled" in :
        var canceledEvaluated = false

        IO(7)
          .onCancel:
            canceledEvaluated = true
            IO.unit
          .map: _ =>
            assert(canceledEvaluated)

      "onCancelLazy evaluate the handler only when canceled" in :
        var canceledEvaluated = false

        IO(7)
          .onCancelLazy:
            canceledEvaluated = true
            IO.unit
          .map: _ =>
            assert(!canceledEvaluated)
    }
  }

  "Fiber" - {
    "joinStd throws when Fiber has been canceled" in:
      val exception = new Exception("TEST")
      for
        fiber <- IO(7).start
        joined <- fiber.joinStd
        _ = assert(joined == 7)

        fiber <- IO.raiseError(exception).start
        joined <- fiber.joinStd.attempt
        _ = assert(joined == Left(exception))

        fiber <- IO.never.start
        _ <- fiber.cancel
        joined <- fiber.joinStd.attempt
      yield
        joined match
          case Left(t: FiberCanceledException) => assert(t.toString ==
            "js7.base.catsutils.CatsEffectExtensions$FiberCanceledException: Fiber has been canceled")
          case _ => fail("Unexpected result vom .joinStd")
  }

  "Resource" - {
    "orIfNone" in:
      var aAcquired, bAcquired, aReleased, bReleased = 0

      def aResource(a: Option[Int]) =
        Resource.make(SyncIO { aAcquired += 1; a })(_ => SyncIO { aReleased += 1 } )

      def bResource(b: Int) =
        Resource.make(SyncIO { bAcquired += 1; b })(_ => SyncIO { bReleased += 1 } )

      assert(aResource(1.some).orIfNone(bResource(2)).use(SyncIO(_)).unsafeRunSync() == 1)
      assert(aAcquired == 1 && aReleased == 1 && bAcquired == 0 && bReleased == 0)

      assert(aResource(none).orIfNone(bResource(2)).use(SyncIO(_)).unsafeRunSync() == 2)
      assert(aAcquired == 2 && aReleased == 2 && bAcquired == 1 && bReleased == 1)

    "makeCancelable" in:
      val canceled = Atomic(false)
      val resource = Resource.makeCancelable[IO, Unit](
        acquire = IO.never.onCancel(IO(canceled := true)))(
        release = _ => IO.unit)

      for
        fiber <- resource.surround(IO.unit).start
        _ <- IO.sleep(100.ms)
        _ <- fiber.cancel
        _ <- fiber.joinWithUnit
      yield
        assert(canceled.get)

    "makeCaseCancelable" in:
      val canceled = Atomic(false)
      val releaseExitCase = Atomic(none[ExitCase])
      val resource = Resource.makeCaseCancelable[IO, Unit](
        acquire = IO.never.onCancel(IO(canceled := true)))(
        release = (_, exitCase) => IO(releaseExitCase := exitCase.some))

      for
        fiber <- resource.surround(IO.unit).start
        _ <- IO.sleep(100.ms)
        _ <- fiber.cancel
        _ <- fiber.joinWithUnit
      yield
        assert(canceled.get && releaseExitCase.get == None)
  }
