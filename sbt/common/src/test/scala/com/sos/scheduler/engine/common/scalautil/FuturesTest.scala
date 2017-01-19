package com.sos.scheduler.engine.common.scalautil

import com.sos.scheduler.engine.common.scalautil.Futures.implicits._
import com.sos.scheduler.engine.common.scalautil.Futures.{FutureNotSucceededException, NoFuture, catchInFuture}
import com.sos.scheduler.engine.common.time.ScalaTime._
import java.util.concurrent.TimeoutException
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future, Promise}

/**
 * @author Joacim Zschimmer
 */
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


  "appendCurrentStackTrace failure exception is extended with future's creation stack trace" in {
    val future = Future[Int] { throw new TestException }
    Await.ready(future, 2.seconds)
    assert(!stackTraceContainsCreationsStackTrace { future.value.get.get })
    val f = future.appendCurrentStackTrace
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

  "future.await" in {
    Future { true } await 1.s shouldBe true
    intercept[TimeoutException] {
      Future { sleep(100.ms) } await 1.ms shouldBe true
    }
  }

  "futures.await" in {
    List(Future { true }, Future { 1 }) await 1.s shouldBe List(true, 1)
    intercept[TimeoutException] {
      Future { sleep(100.ms) } await 1.ms shouldBe true
    }
  }

  "future.flatten" in {
    val a = Future { Future { 77 } }
    assert((a.flatten await 100.ms) == 77)
  }

  "NoFuture" in {
    val neverHappeningFuture: Future[Int] = NoFuture
    assert(!neverHappeningFuture.isCompleted)
  }

  private def stackTraceContainsCreationsStackTrace(body: ⇒ Int): Boolean =
    intercept[TestException] { body } .getStackTrace exists { _.toString contains classOf[FreeSpec].getName }

  private class TestException extends Exception
}
