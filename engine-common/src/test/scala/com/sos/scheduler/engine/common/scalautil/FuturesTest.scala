package com.sos.scheduler.engine.common.scalautil

import com.sos.scheduler.engine.common.scalautil.Futures.FutureNotSucceededException
import com.sos.scheduler.engine.common.scalautil.Futures.implicits._
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import scala.concurrent.Promise

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class FuturesTest extends FreeSpec {

  "successValue" in {
    val promise = Promise[Int]()
    val future = promise.future
    intercept[FutureNotSucceededException] { promise.successValue }
    intercept[FutureNotSucceededException] { future.successValue }
    promise.success(42)
    assert(promise.successValue == 42)
    assert(future.successValue == 42)
  }

  "successValue failure" in {
    val promise = Promise[Int]()
    val future = promise.future
    promise.failure(new TestException)
    intercept[TestException] { promise.successValue }
    intercept[TestException] { future.successValue }
  }

  private class TestException extends Exception
}
