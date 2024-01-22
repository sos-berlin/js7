package js7.base.utils

import js7.base.test.OurAsyncTestSuite
import scala.annotation.nowarn
import scala.concurrent.Promise

/**
  * @author Joacim Zschimmer
  */
@nowarn("msg=class FutureCompletion in package js7.base.utils is deprecated")
final class FutureCompletionTest extends OurAsyncTestSuite:

  "FutureCompletion" in:
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
    for
      a1 <- a.future
      c1 <- c.future
    yield
      assert(a1 == 7 && !b.future.isCompleted && c1 == 7)
      assert(futureCompletion.size == 0)

  "IO cancelOnCompletionOf, not shut down" in:
    pending // FIXME Monix
    //monix implicit val scheduler = TestScheduler()
    //monix val shutdownPromise = Promise[Int]()
    //monix val futureCompletion = new FutureCompletion[Int](shutdownPromise.future)
    //monix
    //monix locally:
    //monix   val p = CancelablePromise[String]()
    //monix   val fut = p.future.cancelOnCompletionOf(futureCompletion)
    //monix   p.success("OK")
    //monix   scheduler.tick()
    //monix   assert(fut.isCompleted)
    //monix   assert(fut.value == Some(Success("OK")))
    //monix   assert(futureCompletion.size == 0)
    //monix
    //monix locally:
    //monix   val p = CancelablePromise[String]()
    //monix   val fut = p.future.cancelOnCompletionOf(futureCompletion)
    //monix   shutdownPromise.success(7)
    //monix   scheduler.tick()
    //monix   p.success("LOST")
    //monix   scheduler.tick()
    //monix   assert(!fut.isCompleted)
    //monix   assert(futureCompletion.size == 0)
