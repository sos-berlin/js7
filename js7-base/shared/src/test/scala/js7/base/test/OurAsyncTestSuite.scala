package js7.base.test

import cats.effect.syntax.temporal.*
import cats.effect.{GenTemporal, Sync}
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import js7.base.utils.ScalaUtils.syntax.RichJavaClass
import org.scalatest.Assertion
import org.scalatest.freespec.AsyncFreeSpec
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration

abstract class OurAsyncTestSuite
extends AsyncFreeSpec, LoggingAsyncFreeSpec, TestMixin, TestCatsEffect:

  override implicit def executionContext: ExecutionContext =
    super[TestCatsEffect].executionContext

  protected final def testTimeout[F[_]](timeout: FiniteDuration)(test: F[Assertion])
    (using F: GenTemporal[F, Throwable])
  : F[Assertion] =
    test.timeout(timeout)

  protected final def repeatTest[F[_]](n: Int)(body: Int => F[Assertion])(using F: Sync[F])
  : F[Assertion] =
    (1).tailRecM: i =>
      F.delay(Logger(getClass.scalaName).info(s"╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌ repeatTest #$i"))
        >> body(i)
        .map: result =>
          if i < n then
            Left(i + 1)
          else
            Right(result)
