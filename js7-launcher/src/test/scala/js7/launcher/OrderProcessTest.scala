package js7.launcher

import cats.effect.std.Semaphore
import cats.effect.unsafe.IORuntime
import cats.effect.{FiberIO, IO}
import js7.base.catsutils.CatsEffectExtensions.joinStd
import js7.base.catsutils.UnsafeMemoizable.unsafeMemoize
import js7.base.test.OurAsyncTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.thread.Futures.implicits.*
import js7.base.time.ScalaTime.*
import js7.data.job.JobKey
import js7.data.order.{OrderId, OrderOutcome}
import js7.tester.ScalaTestUtils.awaitAndAssert
import scala.concurrent.Await
import scala.util.Try

final class OrderProcessTest extends OurAsyncTestSuite:

  private given IORuntime = ioRuntime

  "Run an OrderProcess" in:
    val orderProcess = OrderProcess.succeeded()
    for
      outcome <- orderProcess.run(OrderId("ORDER"), JobKey.forTest("JOB"))
    yield
      assert(outcome == OrderOutcome.succeeded)

  "Intermediate test: cancel a Fiber" in:
    import scala.language.unsafeNulls

    val semaphore = Semaphore[IO](0).unsafeMemoize
    def count = semaphore.flatMap(_.count).await(99.s)

    val canceled = new RuntimeException("TEST FIBER CANCELED")
    var fiber: FiberIO[Unit] = null
    val future = semaphore
      .flatMap(_.acquire)
      .start
      .flatTap(fib => IO { fiber = fib })
      .flatMap(_.joinWith(IO.raiseError(canceled)))
      .unsafeToFuture()

    // One waiting acquirer
    awaitAndAssert(count == -1)

    fiber.cancel.await(99.s)

    // The Future completes with Canceled
    val result: Try[Unit] = Await.ready(future, 99.s).value.get
    assert(result.failed.get eq canceled)

    // No waiting acquirer
    awaitAndAssert(count == 0)

  "Cancel an OrderProcess Fiber" in:
    val semaphore = Semaphore[IO](0).unsafeMemoize
    def count = semaphore.flatMap(_.count).await(99.s)

    val orderProcess = OrderProcess.cancelable:
      semaphore.flatMap(_.acquire).as(OrderOutcome.succeeded)

    val future = orderProcess.run(OrderId("ORDER"), JobKey.forTest("JOB")).unsafeToFuture()

    // One waiting acquirer
    awaitAndAssert(count == -1)

    orderProcess.cancel(false).await(99.s)
    assert(future.await(99.s) == OrderOutcome.Failed(Some("Canceled")))

    // No waiting acquirer
    awaitAndAssert(count == 0)
