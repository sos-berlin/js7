package js7.base.time

import js7.base.test.Test
import js7.base.time.ScalaTime.*
import monix.execution.atomic.Atomic
import scala.concurrent.duration.*

final class AlarmClockTest extends Test
{
  private val start = Timestamp("2021-01-01T00:00:00Z")
  private val clockCheckInterval = 1.minute

  "scheduleAt, and tick at scheduled time" in {
    val clock = TestAlarmClock.forTest(start, clockCheckInterval = clockCheckInterval)
    val x = Atomic(0)

    clock.scheduleAt(start + 10.minutes + 7.s) { x += 1 }
    assert(clock.toString ==
      s"ClockCheckingTestAlarmClock($start, alarms=2021-01-01T00:10:07Z, ticking 60s)")
    assert(x() == 0)

    clock.tick(1.s)
    assert(x() == 0)

    clock.tick(clockCheckInterval)
    assert(x() == 0)

    clock.tick(10.minutes + 7.s - (1.s + clockCheckInterval))
    assert(x() == 1)

    assert(clock.toString == s"ClockCheckingTestAlarmClock(${clock.now()}, no alarm)")
  }

  "scheduleAt, not ticking due to short delay" in {
    val clock = TestAlarmClock.forTest(start, clockCheckInterval = clockCheckInterval)
    val x = Atomic(0)

    clock.scheduleAt(start + clockCheckInterval) { x += 1 }
    assert(clock.toString ==
      s"ClockCheckingTestAlarmClock(${clock.now()}, alarms=2021-01-01T00:01:00Z)")
    assert(x() == 0)

    clock.tick(clockCheckInterval)
    assert(x() == 1)

    assert(clock.toString == s"ClockCheckingTestAlarmClock(${clock.now()}, no alarm)")
  }

  "scheduleAt, and tick late" in {
    val clock = TestAlarmClock.forTest(start, clockCheckInterval = clockCheckInterval)
    val x = Atomic(0)

    clock.scheduleAt(start + 10.minutes) { x += 1 }
    clock.tick(10.minutes - 3.s)
    assert(x() == 0)

    // Tick later than the scheduled time
    clock.tick(3.s + clockCheckInterval)
    assert(x() == 1)

    assert(clock.toString == s"ClockCheckingTestAlarmClock(${clock.now()}, no alarm)")
  }

  "Put the clock forward, but before the schedule" in {
    val clock = TestAlarmClock.forTest(start, clockCheckInterval = clockCheckInterval)
    val x = Atomic(0)

    clock.scheduleAt(start + 10.minutes + 7.s) { x += 1 }
    assert(x() == 0)

    // Put clock forward
    clock.resetTo(clock.now() + 30.s)
    assert(x() == 0)

    clock.tick()
    assert(x() == 0)

    clock.tick(9.minutes + 30.s)
    assert(x() == 0)

    clock.tick(6.s)
    assert(x() == 0)

    clock.tick(1.s)
    assert(x() == 1)

    assert(clock.toString == s"ClockCheckingTestAlarmClock(${clock.now()}, no alarm)")
  }

  "Put the clock forward, after the schedule" in {
    val clock = TestAlarmClock.forTest(start, clockCheckInterval = clockCheckInterval)
    val x = Atomic(0)

    clock.scheduleAt(start + 10.minutes + 7.s) { x += 1 }
    assert(x() == 0)

    // Put clock forward
    clock.resetTo(clock.now() + 10.minutes + 30.s)
    assert(x() == 0)

    clock.tick()
    assert(x() == 0)

    clock.tick(clockCheckInterval)
    assert(x() == 1)

    assert(clock.toString == s"ClockCheckingTestAlarmClock(${clock.now()}, no alarm)")
  }

  "Put the clock backward" in {
    val clock = TestAlarmClock.forTest(start, clockCheckInterval = clockCheckInterval)
    val x = Atomic(0)

    clock.scheduleAt(start + 10.minutes) { x += 1 }
    assert(x() == 0)

    // Put clock backward
    clock.resetTo(clock.now() - 5.minutes)
    assert(x() == 0)

    clock.tick()
    assert(x() == 0)

    clock.tick(10.minutes)
    assert(x() == 0)

    clock.tick(4.minutes)
    assert(x() == 0)

    clock.tick(1.minutes)
    assert(x() == 1)

    assert(clock.toString == s"ClockCheckingTestAlarmClock(${clock.now()}, no alarm)")
  }

  "Multiple schedules" in {
    val clock = TestAlarmClock.forTest(start, clockCheckInterval = clockCheckInterval)
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

    assert(clock.toString ==
      s"ClockCheckingTestAlarmClock(${clock.now()}, alarms=2021-01-01T00:10:00Z, 3 alarms, ticking 60s)")
    assert(x() == 0)

    clock.tick(10.minutes)
    assert(x() == 7)

    clock.tick(20.minutes)
    assert(x() == 777)

    assert(clock.toString == s"ClockCheckingTestAlarmClock(${clock.now()}, no alarm)")
  }

  "cancel" in {
    val clock = TestAlarmClock.forTest(start, clockCheckInterval = clockCheckInterval)
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
    clock.tick(20.minutes)
    assert(x() == 50)
    assert(clock.toString == s"ClockCheckingTestAlarmClock(${clock.now()}, no alarm)")
  }
}
