package js7.base.test

import cats.effect.GenTemporal
import cats.effect.syntax.temporal.*
import org.scalatest.Assertion
import org.scalatest.freespec.AsyncFreeSpec
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration

abstract class OurAsyncTestSuite
extends AsyncFreeSpec, LoggingAsyncFreeSpec, TestStandards, TestCatsEffect:

  override implicit def executionContext: ExecutionContext =
    super[TestCatsEffect].executionContext

  protected final def testTimeout[F[_]](timeout: FiniteDuration)(test: F[Assertion])
    (using F: GenTemporal[F, Throwable])
  : F[Assertion] =
    test.timeout(timeout)
