package js7.base.monixutils

import js7.base.monixutils.MonixDeadline.{monotonicClock, now}
import js7.base.time.ScalaTime.*
import monix.execution.schedulers.TestScheduler
import js7.base.test.OurAsyncTestSuite

/**
  * @author Joacim Zschimmer
  */
final class MonixDeadlineTest extends OurAsyncTestSuite:
  private implicit val scheduler: TestScheduler = TestScheduler()

  "now" in:
    implicit val scheduler = TestScheduler()
    assert(now.nanos == 0)
    scheduler.tick(1.s)
    assert(now.nanos == 1_000_000_000)

  "monotonicClock" in:
    val scheduler = TestScheduler()
    monotonicClock.flatMap { deadline0 =>
      scheduler.tick(1.s)
      monotonicClock.map { deadline1 =>
        assert(deadline0.nanos == 0 && deadline1.nanos == 1_000_000_000)
      }
    }
    .runToFuture(scheduler)

  "Zero MonixDeadline" in:
    implicit val scheduler = TestScheduler()
    assert(!now.hasTimeLeft)
    assert(now.hasElapsed)

  "Late MonixDeadline" in:
    implicit val scheduler = TestScheduler()
    val late = now
    assert(late == now)
    scheduler.tick(1.s)

    assert(late.elapsed == 1.s)
    assert(late.elapsedOrZero == 1.s)
    assert(late.timeLeft == -1.s)
    assert(late.timeLeftOrZero == 0.s)
    assert(late.hasElapsed)
    assert(!late.hasTimeLeft)
    assert(late.isOverdue)

  "Early MonixDeadline" in:
    val early = now + 2.s
    scheduler.tick(1.s)

    assert(early.elapsed == -1.s)
    assert(early.elapsedOrZero == 0.s)
    assert(early.timeLeft == 1.s)
    assert(early.timeLeftOrZero == 1.s)
    assert(!early.hasElapsed)
    assert(early.hasTimeLeft)
    assert(!early.isOverdue)

  "Operations" in:
    assert(now + 1.s - now == 1.s)
    assert(now - 1.s - now == -1.s)

  "Ordering" in:
    assert((now + 1.s).compare(now) > 0)
    assert(now.compare(now) == 0)
    assert((now - 1.s).compare(now) < 0)

    assert(now + 1.s > now)
    assert(now + 1.s >= now)
    assert(now >= now)
    assert(now == now)
    assert(now <= now)
    assert(now <= now + 1.s)
    assert(now < now + 1.s)
