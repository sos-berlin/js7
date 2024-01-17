package js7.base.catsutils

import cats.effect.unsafe.IORuntime
import cats.effect.{IO, OutcomeIO}
import org.scalatest.freespec.AsyncFreeSpec
import scala.collection.mutable

final class CatsEffectGuaranteeCancelTest extends AsyncFreeSpec:

  private given IORuntime = IORuntime.global

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
      .unsafeToFuture()

  "guarantee Canceled" in:
    // Works as expected
    var count = 0

    IO(7)
      .guarantee:
        count += 1
        IO.unit
      .map: _ =>
        assert(count == 1)
      .unsafeToFuture()
