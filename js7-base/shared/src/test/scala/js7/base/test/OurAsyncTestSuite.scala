package js7.base.test

import cats.effect.GenTemporal
import cats.effect.syntax.temporal.*
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

  //protected final def repeatTest[F[_]](n: Int)(body: Int => F[Assertion]): F[Assertion] =
  //  for i <- 1 to n do
  //    Logger(getClass.scalaName).debugF(s"Test #$i"):
  //      //? withClue(s"#$i: "):
  //        body(i)
