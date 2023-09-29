package js7.base.thread

import java.util.concurrent.TimeoutException
import js7.base.test.OurTestSuite
import js7.base.thread.Futures.implicits.*
import js7.base.thread.Futures.{FutureNotSucceededException, catchInFuture, namedThreadFuture, promiseFuture}
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.measureTime
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers.*
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future, Promise}
import scala.util.Failure

/**
 * @author Joacim Zschimmer
 */
final class FuturesTest extends OurTestSuite:
  "successValue" in:
    val promise = Promise[Int]()
    val future = promise.future
    intercept[FutureNotSucceededException] { promise.successValue }
    intercept[FutureNotSucceededException] { future.successValue }
    promise.success(42)
    assert(promise.successValue == 42)
    assert(future.successValue == 42)

  "successValue's failure exception is extended with future's creation stack trace" in:
    val future = Future[Int] { throw new TestException }
    Await.ready(future, 9.s)
    assert(!stackTraceContainsCreationsStackTrace { future.value.get.get })
    assert(stackTraceContainsCreationsStackTrace { future.successValue })

  "appendCurrentStackTrace failure exception is extended with future's creation stack trace" in:
    val future = Future[Int] { throw new TestException }
    Await.ready(future, 9.s)
    assert(!stackTraceContainsCreationsStackTrace { future.value.get.get })
    val f = future.appendCurrentStackTrace
    Await.ready(f, 9.s)
    assert(stackTraceContainsCreationsStackTrace { f.value.get.get })

  "catchInFuture" in:
    def f(t: Boolean): Future[Unit] =
      if t then throw new RuntimeException
      Future { throw new RuntimeException }
    intercept[RuntimeException] { f(true) }
    Await.ready(f(false), 99.s).value.get.failed.get
    Await.ready(catchInFuture { f(true) }, 99.s).value.get.failed.get

  "namedThreadFuture" in:
    val (n, warmUp) = sys.props.get("test.speed").fold((100, 100))(o => (o.toInt, 1000))
    info(measureTime(n, "namedThreadFuture", warmUp = warmUp) {
      val future = namedThreadFuture("FuturesTest") { "x" }
      assert(Await.result(future, 9.s) == "x")
    }.toString)
    val future = namedThreadFuture("FuturesTest") { sys.error("TEST-ERROR") }
    assert(Await.ready(future, 9.s).value.get.asInstanceOf[Failure[?]].exception.getMessage contains "TEST-ERROR")

  "promiseFuture" in:
    val a: Future[Int] = promiseFuture[Int] { _ => }
    assert(!a.isCompleted)
    val b: Future[Int] = promiseFuture[Int] { _.success(7) }
    assert(b.value.get.get == 7)

  "future.await" in:
    Future { true } await 9.s shouldBe true
    intercept[TimeoutException]:
      Future { sleep(1.s) } await 1.ms

  "futures.await" in:
    List(Future { true }, Future { 1 }) await 9.s shouldBe List(true, 1)
    intercept[TimeoutException]:
      Future { sleep(1.s) } await 1.ms

  private def stackTraceContainsCreationsStackTrace(body: => Int): Boolean =
    intercept[TestException] { body } .getStackTrace.exists(_.toString contains classOf[AnyFreeSpec].getName)

  private class TestException extends Exception
