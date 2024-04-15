package js7.base.test

import cats.effect.IO
import cats.effect.Outcome
import cats.effect.testkit.TestControl
import org.scalatest.Assertions.fail

object TestControlExtensions:

  extension[A](control: TestControl[A])
    def finish: IO[A] =
      control.tick *> successOrFail

    /** Returns the succeeded value or fails. */
    def successOrFail: IO[A] =
      control.results.map:
        case Some(Outcome.Succeeded(a)) => a
        case Some(Outcome.Errored(t)) => throw t
        case Some(outcome) => fail(s"Unexpected TestControl.results => $outcome")
        case None => fail("Tested program has not terminated")
