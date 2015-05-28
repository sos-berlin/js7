package com.sos.scheduler.engine.common.scalautil

import com.sos.scheduler.engine.common.scalautil.Futures.FutureNotSucceededException
import com.sos.scheduler.engine.common.scalautil.Futures.implicits._
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future, Promise}

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

  "successValue's failure exception is extended with future's creation stack trace" in {
    val future = Future[Int] { throw new TestException }
    Await.ready(future, 2.seconds)
    assert(!stackTraceContainsCreationsStackTrace { future.value.get.get })
    assert(stackTraceContainsCreationsStackTrace { future.successValue })
  }


  "withThisStackTrace failure exception is extended with future's creation stack trace" in {
    val future = Future[Int] { throw new TestException }
    Await.ready(future, 2.seconds)
    assert(!stackTraceContainsCreationsStackTrace { future.value.get.get })
    val f = future.withThisStackTrace
    Await.ready(f, 2.seconds)
    assert(stackTraceContainsCreationsStackTrace { f.value.get.get })
  }

  private def stackTraceContainsCreationsStackTrace(body: ⇒ Int): Boolean =
    intercept[TestException] { body } .getStackTrace exists { _.toString contains classOf[FreeSpec].getName }

  private class TestException extends Exception
}
