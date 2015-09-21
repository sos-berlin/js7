package com.sos.scheduler.engine.common.scalautil

import com.sos.scheduler.engine.common.scalautil.Futures.implicits._
import com.sos.scheduler.engine.common.scalautil.Futures.{FutureNotSucceededException, NoFuture, catchInFuture}
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

  "catchInFuture" in {
    def f(t: Boolean): Future[Unit] = {
      if (t) throw new RuntimeException
      Future { throw new RuntimeException }
    }
    intercept[RuntimeException] { f(true) }
    Await.ready(f(false), 2.seconds).value.get.failed.get
    Await.ready(catchInFuture { f(true) }, 2.seconds).value.get.failed.get
  }

  "NoFuture" in {
    val neverHappeningFuture: Future[Int] = NoFuture
    assert(!neverHappeningFuture.isCompleted)
  }

  private def stackTraceContainsCreationsStackTrace(body: ⇒ Int): Boolean =
    intercept[TestException] { body } .getStackTrace exists { _.toString contains classOf[FreeSpec].getName }

  private class TestException extends Exception
}
