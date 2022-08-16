package js7.base.monixutils

import js7.base.time.ScalaTime.*
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import js7.base.test.OurAsyncTestSuite
import scala.concurrent.Promise

final class LatchTest extends OurAsyncTestSuite
{
  "Initially isNot" in {
    val latch = new Latch
    latch.isNot
      .map(o => assert(!o))
      .*>(latch.is)
      .map(assert(_))
      .runToFuture
  }

  "switch" in {
    val latch = new Latch
    latch
      .switch
      .map(assert(_)) // first call
      .*>(latch.isNot)
      .map(assert(_))
      .*>(latch.is)
      .map(o => assert(!o))
      .*>(latch.switch)
      .map(o => assert(!o)) // second call
      .*>(latch.isNot)
      .map(assert(_))
      .runToFuture
  }

  "when" in {
    val latch = new Latch
    val promise = Promise[Unit]()

    val whenClosed = latch
      .when
      .*>(Task { promise.success(()) })
      .runToFuture

    Task.sleep(100.ms)
      .map(_ => assert(!promise.isCompleted))
      .*>(latch.switch)
      .map(assert(_))
      .*>(Task.fromFuture(promise.future))
      .*>(Task.fromFuture(whenClosed).as(succeed))
    .runToFuture
  }

  "switchThen" in {
    val latch = new Latch
    val promise = Promise[Unit]()
    latch
      .switchThen(Task { promise.success(()); 7 })
      .map(maybe => assert(maybe.contains(7) && promise.isCompleted))
      .*>(latch.switchThen(Task { promise.success(()) }))
      .map(maybe => assert(maybe.isEmpty))
      .runToFuture
  }
}
