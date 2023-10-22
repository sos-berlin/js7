package js7.launcher

import js7.base.test.OurTestSuite
import js7.base.thread.Futures.implicits.*
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.data.job.JobKey
import js7.data.order.{OrderId, Outcome}
import js7.tester.ScalaTestUtils.awaitAndAssert
import monix.catnap.Semaphore
import cats.effect.IO
import cats.effect.Fiber
import monix.execution.Scheduler.Implicits.traced
import monix.reactive.subjects.PublishSubject

final class OrderProcessTest extends OurTestSuite:

  "Run an OrderProcess" in:
    val orderProcess = OrderProcess(IO(Outcome.succeeded))
    val stdObservers = newStdObservers
    assert(
      orderProcess.start(OrderId("ORDER"), JobKey.forTest("JOB"), stdObservers).flatten.await(99.s)
        == Outcome.succeeded)

  "Intermediate test: cancel a Fiber" in:
    val semaphore = Semaphore[IO](0).memoize
    def count = semaphore.flatMap(_.count).await(99.s)

    var fiber: FiberIO[Unit] = null
    val future = semaphore
      .flatMap(_.acquire)
      .start
      .tapEval(fib => IO { fiber = fib })
      .flatMap(_.join)
      .runToFuture

    // One waiting acquirer
    awaitAndAssert(count == -1)

    // Does not work if the Future is being canceled. Instead, the Fiber must be canceled.
    //future.cancel()
    fiber.cancel.await(99.s)

    // The Future never completes
    sleep(100.ms)
    assert(!future.isCompleted)

    // No waiting acquirer
    awaitAndAssert(count == 0)

  "Cancel an OrderProcess Fiber" in:
    val semaphore = Semaphore[IO](0).memoize
    def count = semaphore.flatMap(_.count).await(99.s)

    val orderProcess = OrderProcess(semaphore.flatMap(_.acquire).as(Outcome.succeeded))
    val stdObservers = newStdObservers
    val future = orderProcess.start(OrderId("ORDER"), JobKey.forTest("JOB"), stdObservers)
      .flatten.runToFuture

    // One waiting acquirer
    awaitAndAssert(count == -1)

    orderProcess.cancel(false).await(99.s)
    assert(future.await(99.s) == Outcome.Failed(Some("Canceled")))

    // No waiting acquirer
    awaitAndAssert(count == 0)

  private def newStdObservers =
    new StdObservers(PublishSubject(), PublishSubject(), 100, keepLastErrLine = false)
