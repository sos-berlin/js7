package js7.base.time

import cats.effect.IO
import js7.base.catsutils.Js7IORuntime
import js7.base.test.{OurAsyncTestSuite, OurTestControl}
import js7.base.time.ScalaTime.*
import js7.base.utils.Atomic
import js7.base.utils.Atomic.extensions.*
import org.scalatest.Assertion
import scala.concurrent.duration.*

final class AlarmClockTest extends OurAsyncTestSuite:

  private val start = Timestamp("2021-01-01T00:00:00Z")
  private val clockCheckInterval = 1.minute

  "scheduleAt, and tick at scheduled time" in:
    testWithAlarmClock: clock =>
      val x = Atomic(0)
      clock.scheduleAt(start + 10.minutes + 7.s) { x += 1 }
      assert(clock.toString ==
        s"ClockCheckingTestAlarmClock($start, alarms=2021-01-01T00:10:07Z, ticking 60s)")
      assert(x.get() == 0)

      for
        _ <- clock.tick(1.s)
        _ = assert(x.get() == 0)
        _ <- clock.tick(clockCheckInterval)
        _ = assert(x.get() == 0)
        _ <- clock.tick(10.minutes + 7.s - (1.s + clockCheckInterval))
        _ <- clock.tickEpsilon // Allow scheduleAt callback to run
        _ = assert(x.get() == 1)
      yield
        assert(clock.toString == s"ClockCheckingTestAlarmClock(${clock.now()}, no alarm)")
        succeed

  "scheduleAt, not ticking due to short delay" in:
    testWithAlarmClock: clock =>
      val x = Atomic(0)
      clock.scheduleAt(start + clockCheckInterval) { x += 1 }
      assert(clock.toString ==
        s"ClockCheckingTestAlarmClock(${clock.now()}, alarms=2021-01-01T00:01:00Z)")
      assert(x.get() == 0)

      for
        _ <- clock.tick(clockCheckInterval)
        _ <- clock.tickEpsilon // Allow scheduleAt callback to run
        _ = assert(x.get() == 1)
      yield
        assert(clock.toString == s"ClockCheckingTestAlarmClock(${clock.now()}, no alarm)")

  "scheduleAt, and tick late" in:
    testWithAlarmClock: clock =>
      val x = Atomic(0)
      clock.scheduleAt(start + 10.minutes) { x += 1 }

      for
        _ <- clock.tick(10.minutes - 3.s)
        _ = assert(x.get() == 0)
        // Tick later than the scheduled time
        _ <- clock.tick(3.s + clockCheckInterval)
        _ <- clock.tickEpsilon // Allow scheduleAt callback to run
        _ = assert(x.get() == 1)
      yield
        assert(clock.toString == s"ClockCheckingTestAlarmClock(${clock.now()}, no alarm)")

  "Put the clock forward, but before the schedule" in:
    testWithAlarmClock: clock =>
      val x = Atomic(0)
      clock.scheduleAt(start + 10.minutes + 7.s) { x += 1 }
      assert(x.get() == 0)

      for
        // Put clock forward
        _ <- clock.resetTo(clock.now() + 30.s)
        _ = assert(x.get() == 0)

        _ <- clock.tick()
        _ = assert(x.get() == 0)

        _ <- clock.tick(9.minutes + 30.s)
        _ = assert(x.get() == 0)

        _ <- clock.tick(6.s)
        _ = assert(x.get() == 0)

        _ <- clock.tick(1.s)
        _ <- clock.tickEpsilon // Allow scheduleAt callback to run
        _ = assert(x.get() == 1)
      yield
        assert(clock.toString == s"ClockCheckingTestAlarmClock(${clock.now()}, no alarm)")

  "Put the clock forward, after the schedule" in:
    testWithAlarmClock: clock =>
      val x = Atomic(0)
      clock.scheduleAt(start + 10.minutes + 7.s) { x += 1 }
      assert(x.get() == 0)

      for
        // Put clock forward
        _ <- clock.resetTo(clock.now() + 10.minutes + 30.s)
        _ = assert(x.get() == 0)

        _ <- clock.tick()
        _ = assert(x.get() == 0)

        _ <- clock.tick(clockCheckInterval)
        _ <- clock.tickEpsilon // Allow scheduleAt callback to run
        _ = assert(x.get() == 1)
      yield
        assert(clock.toString == s"ClockCheckingTestAlarmClock(${clock.now()}, no alarm)")

  "Put the clock backward" in:
    testWithAlarmClock: clock =>
      val x = Atomic(0)
      clock.scheduleAt(start + 10.minutes) { x += 1 }
      assert(x.get() == 0)

      for
        // Put clock backward
        _ <- clock.resetTo(clock.now() - 5.minutes)
        _ = assert(x.get() == 0)

        _ <- clock.tick()
        _ = assert(x.get() == 0)

        _ <- clock.tick(10.minutes)
        _ = assert(x.get() == 0)

        _ <- clock.tick(4.minutes)
        _ = assert(x.get() == 0)

        _ <- clock.tick(1.minutes)
        _ <- clock.tickEpsilon // Allow scheduleAt callback to run
        _ = assert(x.get() == 1)
      yield
        assert(clock.toString == s"ClockCheckingTestAlarmClock(${clock.now()}, no alarm)")

  "Multiple schedules" in:
    testWithAlarmClock: clock =>
      val x = Atomic(0)
      clock.scheduleAt(start + 10.minutes) { x += 1 }
      clock.scheduleAt(start + 10.minutes) { x += 2 }
      clock.scheduleAt(start + 10.minutes) { x += 4 }

      clock.scheduleAt(start + 20.minutes) { x += 10 }
      clock.scheduleAt(start + 20.minutes) { x += 20 }
      clock.scheduleAt(start + 20.minutes) { x += 40 }

      clock.scheduleAt(start + 30.minutes) { x += 100 }
      clock.scheduleAt(start + 30.minutes) { x += 200 }
      clock.scheduleAt(start + 30.minutes) { x += 400 }

      assert(x.get() == 0)
      assert(clock.toString ==
        s"ClockCheckingTestAlarmClock(${clock.now()}, alarms=2021-01-01T00:10:00Z, 3 alarms, ticking 60s)")

      for
        _ <- clock.tick(10.minutes)
        _ <- clock.tickEpsilon // Allow scheduleAt callback to run
        _ = assert(x.get() == 7)

        _ <- clock.tick(20.minutes)
        _ <- clock.tickEpsilon // Allow scheduleAt callback to run
        _ = assert(x.get() == 777)
      yield
        assert(clock.toString == s"ClockCheckingTestAlarmClock(${clock.now()}, no alarm)")

  "cancel" in:
    testWithAlarmClock: clock =>
      val x = Atomic(0)
      val a = clock.scheduleAt(start + 10.minutes) { x += 1 }
      val b = clock.scheduleAt(start + 10.minutes) { x += 2 }
      val c = clock.scheduleAt(start + 10.minutes) { x += 4 }
      val d = clock.scheduleAt(start + 20.minutes) { x += 10 }
      val e = clock.scheduleAt(start + 20.minutes) { x += 20 }
      val f = clock.scheduleAt(start + 20.minutes) { x += 40 }

      a.cancel()
      c.cancel()
      b.cancel()
      e.cancel()

      for
        _ <- clock.tick(20.minutes)
        _ <- clock.tickEpsilon // Allow scheduleAt callback to run
      yield
        assert(x.get() == 50)
        assert(clock.toString == s"ClockCheckingTestAlarmClock(${clock.now()}, no alarm)")

  private def testWithAlarmClock(body: TestAlarmClock => IO[Assertion]): IO[Assertion] =
    OurTestControl.executeEmbed:
      for
        ec <- IO.executionContext
        _ = assert(ec != Js7IORuntime.ioRuntime.compute)
        assertion <- TestAlarmClock
          .resource(start, Some(clockCheckInterval))
          .use(body)
      yield
        assertion
