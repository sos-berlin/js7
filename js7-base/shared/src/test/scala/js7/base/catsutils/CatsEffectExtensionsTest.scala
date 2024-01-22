package js7.base.catsutils

import cats.effect.{IO, OutcomeIO}
import js7.base.catsutils.CatsEffectExtensions.*
import js7.base.test.OurAsyncTestSuite
import js7.base.time.ScalaTime.*
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
