package js7.base.test

import org.scalatest.freespec.AsyncFreeSpec
import cats.effect.testing.scalatest.AsyncIOSpec

abstract class OurAsyncTestSuite extends AsyncFreeSpec, AsyncIOSpec, LoggingAsyncFreeSpec
