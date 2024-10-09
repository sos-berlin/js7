package js7.base.test

import cats.effect.IO
import cats.effect.Outcome
import cats.effect.testkit.TestControl
import cats.effect.unsafe.IORuntimeConfig
import org.scalatest.Assertions.fail
import scala.concurrent.duration.FiniteDuration

object TestControlExtensions:

  extension[A](control: TestControl[A])
    def runFor(duration: FiniteDuration): IO[A] =
      control.tickFor(duration) *> finish

    def finish: IO[A] =
      control.tick *> successOrFail

    /** Returns the succeeded value or fails. */
    def successOrFail: IO[A] =
      control.results.map:
        case Some(Outcome.Succeeded(a)) => a
        case Some(Outcome.Errored(t)) => throw t
        case Some(outcome) => fail(s"Unexpected TestControl.results => $outcome")
        case None => fail("Tested program has not terminated")


  extension (x: TestControl.type)
    def executeFor[A](duration: FiniteDuration)(
      program: IO[A],
      config: IORuntimeConfig = IORuntimeConfig(),
      seed: Option[String] = None)
    : IO[A] =
      TestControl.execute(program, config, seed).flatMap:
        _.runFor(duration)
