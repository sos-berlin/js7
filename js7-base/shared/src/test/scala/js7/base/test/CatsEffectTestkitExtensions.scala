package js7.base.test

import cats.effect.IO
import cats.effect.kernel.Outcome
import cats.effect.testkit.TestControl

object CatsEffectTestkitExtensions:

  extension[A](control: TestControl[A])
    /** Returns the succeeded value or fails. */
    def toSucceeded: IO[A] =
      control.results.flatMap:
        case Some(Outcome.Succeeded(a)) => IO.pure(a)
        case o => IO.raiseError(new RuntimeException(s"Unexpected Outcome: $o"))
