package js7.base.catsutils

import cats.effect.{IO, OutcomeIO}
import js7.base.catsutils.CatsEffectExtensions.*
import js7.base.test.OurAsyncTestSuite
import js7.base.time.ScalaTime.*
import scala.collection.mutable

final class CatsEffectExtensionsTest extends OurAsyncTestSuite:

  "guaranteeCaseLazy" - {
    "ERROR: guaranteeCase evaluates the Canceled case eagerly even when not Canceled" in:
      val evaluatedCases = mutable.Buffer.empty[OutcomeIO[Int]]

      IO(7)
        .guaranteeCase: outcome =>
          evaluatedCases += outcome
          IO.unit
        .map: _ =>
          assert(evaluatedCases.toString == "ArrayBuffer(Canceled(), Succeeded(IO(7)))")
        .unsafeToFuture()

    "guaranteeCaseLazy evalutes the Canceled case only when Canceled" in:
      val outcomes = mutable.Buffer.empty[OutcomeIO[Int]]

      IO(7)
        .guaranteeCaseLazy: outcome =>
          outcomes += outcome
          IO.unit
        .map: _ =>
          assert(outcomes.toString == "ArrayBuffer(Succeeded(IO(7)))")
        .unsafeToFuture()
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
        .unsafeToFuture()

    "onCancelLazy evaluate the handler only when canceled" in :
      var canceledEvaluated = false

      IO(7)
        .onCancelLazy:
          canceledEvaluated = true
          IO.unit
        .map: _ =>
          assert(!canceledEvaluated)
        .unsafeToFuture()
  }
