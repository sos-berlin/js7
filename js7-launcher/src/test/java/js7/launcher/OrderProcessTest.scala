package js7.launcher

import js7.base.test.OurTestSuite
import js7.base.thread.Futures.implicits.*
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.time.WaitForCondition.waitForCondition
import js7.data.order.Outcome
import monix.catnap.Semaphore
import monix.eval.{Fiber, Task}
import monix.execution.Scheduler.Implicits.traced
import monix.reactive.subjects.PublishSubject

final class OrderProcessTest extends OurTestSuite:
  "Run an OrderProcess" in:
    val orderProcess = OrderProcess(Task(Outcome.succeeded))
    val stdObservers = newStdObservers
    assert(orderProcess.start(stdObservers).flatten.await(99.s) == Outcome.succeeded)

  "Intermediate test: cancel a Fiber" in:
    val semaphore = Semaphore[Task](0).memoize
    def count = semaphore.flatMap(_.count).await(99.s)

    var fiber: Fiber[Unit] = null
    val future = semaphore
      .flatMap(_.acquire)
      .start
      .tapEval(fib => Task { fiber = fib })
      .flatMap(_.join)
      .runToFuture

    // One waiting acquirer
    assert(waitForCondition(10.s, 10.ms)(count == -1))

    // Does not work if the Future is being canceled. Instead, the Fiber must be canceled.
    //future.cancel()
    fiber.cancel.await(99.s)

    // The Future never completes
    sleep(100.ms)
    assert(!future.isCompleted)

    // No waiting acquirer
    assert(waitForCondition(10.s, 10.ms)(count == 0))

  "Cancel an OrderProcess Fiber" in:
    val semaphore = Semaphore[Task](0).memoize
    def count = semaphore.flatMap(_.count).await(99.s)

    val orderProcess = OrderProcess(semaphore.flatMap(_.acquire).as(Outcome.succeeded))
    val stdObservers = newStdObservers
    val future = orderProcess.start(stdObservers).flatten.runToFuture

    // One waiting acquirer
    assert(waitForCondition(10.s, 10.ms)(count == -1))

    orderProcess.cancel(false).await(99.s)
    assert(future.await(99.s) == Outcome.Failed(Some("Canceled")))

    // No waiting acquirer
    assert(waitForCondition(10.s, 10.ms)(count == 0))

  private def newStdObservers =
    new StdObservers(PublishSubject(), PublishSubject(), 100, keepLastErrLine = false)
