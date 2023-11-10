package js7.base.test

import cats.effect.IO
import cats.effect.kernel.{Outcome, Resource}
import cats.effect.testkit.TestControl
import cats.effect.unsafe.IORuntimeConfig
import org.scalatest.Assertion

object CatsEffectTestkitExtensions:

  extension[A](control: TestControl[A])
    /** Returns the succeeded value or fails. */
    def orRaiseError: IO[A] =
      control.results.flatMap:
        case Some(Outcome.Succeeded(a)) => IO.pure(a)
        case o => IO.raiseError(new RuntimeException(s"Unexpected TestControl.results Outcome: $o"))

  extension(obj: TestControl.type)
    def test[A](
      program: IO[A],
      config: IORuntimeConfig = IORuntimeConfig(),
      seed: Option[String] = None)
    : Resource[IO, TestControl[A]] =
      Resource.make(
        acquire = TestControl.execute(program, config, seed))(
        release = _.orRaiseError.void)
