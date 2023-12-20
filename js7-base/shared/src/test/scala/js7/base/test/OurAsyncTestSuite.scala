package js7.base.test

import org.scalatest.freespec.AsyncFreeSpec
import scala.concurrent.ExecutionContext

abstract class OurAsyncTestSuite extends AsyncFreeSpec, LoggingAsyncFreeSpec, TestCatsEffect:
  override implicit def executionContext: ExecutionContext =
    super[TestCatsEffect].executionContext
