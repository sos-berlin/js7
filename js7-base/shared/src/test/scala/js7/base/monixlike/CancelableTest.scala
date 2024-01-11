package js7.base.monixlike

import js7.base.monixlike.MonixLikeExtensions.scheduleOnce
import js7.base.test.{OurTestSuite, TestCatsEffect}
import js7.base.time.ScalaTime.*

final class CancelableTest extends OurTestSuite, TestCatsEffect:

  private val scheduler = ioRuntime.scheduler

  "cancel" in:
    var scheduled = false

    val cancelable: Cancelable =
      scheduler.scheduleOnce(50.ms):
        scheduled = true

    cancelable.unsafeCancelAndForget()
    sleep(00.ms)
    assert(!scheduled)
