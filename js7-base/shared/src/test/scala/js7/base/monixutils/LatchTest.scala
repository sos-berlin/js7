package js7.base.monixutils

import cats.effect.IO
import js7.base.test.OurAsyncTestSuite
import js7.base.time.ScalaTime.*
import scala.concurrent.Promise

final class LatchTest extends OurAsyncTestSuite:

  "Initially isNot" in:
    val latch = Latch()
    latch.isNot
      .map(o => assert(o))
      .*>(latch.is)
      .map(o => assert(!o))

  "switch" in:
    val latch = Latch()
    latch
      .switch
      .map(assert(_)) // first call
      .*>(latch.isNot)
      .map(o => assert(!o))
      .*>(latch.is)
      .map(o => assert(o))
      .*>(latch.switch)
      .map(o => assert(!o)) // second call
      .*>(latch.is)
      .map(o => assert(o))

  "when" in:
    val latch = Latch()
    val promise = Promise[Unit]()

    val whenClosed = latch
      .when
      .*>(IO { promise.success(()) })

    val switch = IO.sleep(100.ms)
      .map(_ => assert(!promise.isCompleted))
      .*>(latch.switch)
      .map(assert(_))
      .*>(IO.fromFuture(IO.pure(promise.future)))

    whenClosed
      .both(switch)
      .as(succeed)

  "switchThen" in:
    val latch = Latch()
    val promise = Promise[Unit]()
    latch
      .switchThen(IO { promise.success(()); 7 })
      .map(maybe => assert(maybe.contains(7) && promise.isCompleted))
      .*>(latch.switchThen(IO { promise.success(()) }))
      .map(maybe => assert(maybe.isEmpty))
