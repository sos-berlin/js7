package js7.base.catsutils

import cats.effect.{IO, OutcomeIO}
import js7.base.test.OurAsyncTestSuite
import scala.collection.mutable

final class CatsEffectGuaranteeCancelTest extends OurAsyncTestSuite:

  "guaranteeCase Canceled" in:
    // Fails because the Canceled case is evaluated eagerly,
    // despite IO(7) is not being canceled.
    val evaluatedCases = mutable.Buffer.empty[OutcomeIO[Int]]

    IO(7)
      .guaranteeCase: outcome =>
        evaluatedCases += outcome
        IO.unit
      .map: _ =>
        // evaluatedCases == "ArrayBuffer(Canceled(), Succeeded(IO(7)))"
        pendingUntilFixed:
          assert(evaluatedCases.toString == "ArrayBuffer(Succeeded(IO(7)))")

  "guarantee Canceled" in:
    // Works as expected
    var count = 0

    IO(7)
      .guarantee:
        count += 1
        IO.unit
      .map: _ =>
        assert(count == 1)
