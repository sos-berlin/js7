package js7.base.utils

import js7.base.utils.FutureCompletion.syntax.*
import monix.execution.CancelablePromise
import monix.execution.schedulers.TestScheduler
import js7.base.test.OurAsyncTestSuite
import scala.concurrent.Promise
import scala.util.Success

/**
  * @author Joacim Zschimmer
  */
final class FutureCompletionTest extends OurAsyncTestSuite
{
  "FutureCompletion" in {
    val promise = Promise[Int]()
    val futureCompletion = new FutureCompletion[Int](promise.future)
    val a = futureCompletion.add()
    val b = futureCompletion.add()
    val c = futureCompletion.add()
    assert(futureCompletion.size == 3)

    b.close()
    assert(futureCompletion.size == 2)

    b.close()
    assert(futureCompletion.size == 2)

    promise.success(7)
    for {
      a1 <- a.future
      c1 <- c.future
    } yield {
      assert(a1 == 7 && !b.future.isCompleted && c1 == 7)
      assert(futureCompletion.size == 0)
    }
  }

  "Task cancelOnCompletionOf, not shut down" in {
    implicit val scheduler = TestScheduler()
    val shutdownPromise = Promise[Int]()
    val futureCompletion = new FutureCompletion[Int](shutdownPromise.future)

    locally {
      val p = CancelablePromise[String]()
      val fut = p.future.cancelOnCompletionOf(futureCompletion)
      p.success("OK")
      scheduler.tick()
      assert(fut.isCompleted)
      assert(fut.value == Some(Success("OK")))
      assert(futureCompletion.size == 0)
    }

    locally {
      val p = CancelablePromise[String]()
      val fut = p.future.cancelOnCompletionOf(futureCompletion)
      shutdownPromise.success(7)
      scheduler.tick()
      p.success("LOST")
      scheduler.tick()
      assert(!fut.isCompleted)
      assert(futureCompletion.size == 0)
    }
  }
}
