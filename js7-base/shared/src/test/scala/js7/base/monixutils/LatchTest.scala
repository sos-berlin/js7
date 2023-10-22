package js7.base.monixutils

import js7.base.time.ScalaTime.*
import cats.effect.IO
import monix.execution.Scheduler.Implicits.traced
import js7.base.test.OurAsyncTestSuite
import scala.concurrent.Promise

final class LatchTest extends OurAsyncTestSuite:
  "Initially isNot" in:
    val latch = new Latch
    latch.isNot
      .map(o => assert(!o))
      .*>(latch.is)
      .map(assert(_))
      .runToFuture

  "switch" in:
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

  "when" in:
    val latch = new Latch
    val promise = Promise[Unit]()

    val whenClosed = latch
      .when
      .*>(IO { promise.success(()) })
      .runToFuture

    IO.sleep(100.ms)
      .map(_ => assert(!promise.isCompleted))
      .*>(latch.switch)
      .map(assert(_))
      .*>(IO.fromFuture(promise.future))
      .*>(IO.fromFuture(whenClosed).as(succeed))
    .runToFuture

  "switchThen" in:
    val latch = new Latch
    val promise = Promise[Unit]()
    latch
      .switchThen(IO { promise.success(()); 7 })
      .map(maybe => assert(maybe.contains(7) && promise.isCompleted))
      .*>(latch.switchThen(IO { promise.success(()) }))
      .map(maybe => assert(maybe.isEmpty))
      .runToFuture
