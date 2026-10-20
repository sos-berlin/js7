package js7.base.time

import cats.effect.IO
import cats.effect.unsafe.Scheduler
import js7.base.catsutils.CatsEffectExtensions.fromFutureDummyCancelable
import js7.base.test.OurAsyncTestSuite
import js7.base.time.ScalaTime.*
import js7.base.utils.Atomic
import js7.base.utils.Atomic.extensions.*
import scala.concurrent.Promise
import scala.concurrent.duration.*

final class ClockCheckingTest extends OurAsyncTestSuite:

  "An alarm scheduled while an earlier alarm is due but not yet fired" in:
    // Scheduler#sleep may run a due task synchronously, re-entering ClockChecking#scheduleNext.
    // The outer scheduleNext must not replace the timer set by the recursive call.
    val scheduler = new SyncWhenDueScheduler(ioRuntime.scheduler)
    val clock = AlarmClock(Some(1.minute))(using scheduler)
    // Callbacks must not block: they run in the IORuntime's threads, even under ClockChecking's lock
    val aFired = Promise[Unit]()
    val bFired = Promise[Unit]()
    IO:
      clock.scheduleAt(clock.now() + 1.s, "A")(aFired.trySuccess(()))
      scheduler.jump(1.s) // Now, A is due, but its timer has not yet fired
      clock.scheduleAt(clock.now() + 50.ms, "B")(bFired.trySuccess(()))
    .productR:
      IO.fromFutureDummyCancelable(IO(aFired.future)).timeout(9.s)
    .productR:
      IO.fromFutureDummyCancelable(IO(bFired.future)).timeout(9.s)
    .guarantee:
      IO(clock.stop())
    .as(succeed)


private final class SyncWhenDueScheduler(underlying: Scheduler) extends Scheduler:
  private val offsetMillis = Atomic(0L)

  def jump(duration: FiniteDuration): Unit =
    offsetMillis += duration.toMillis

  def sleep(delay: FiniteDuration, task: Runnable): Runnable =
    if delay <= Duration.Zero then
      task.run()
      () => ()
    else
      underlying.sleep(delay, task)

  def nowMillis(): Long =
    underlying.nowMillis() + offsetMillis.get()

  def monotonicNanos(): Long =
    underlying.monotonicNanos() + offsetMillis.get() * 1_000_000
