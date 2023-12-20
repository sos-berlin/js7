package js7.base.catsutils

import js7.base.catsutils.CatsDeadline.now
import js7.base.test.{OurAsyncTestSuite, OurTestSuite, WithTestScheduler}
import js7.base.time.ScalaTime.*
import js7.base.utils.CatsUtils.syntax.monotonicTime

final class CatsDeadlineTest extends OurTestSuite, WithTestScheduler:

  "now" in:
    now.flatMap { deadline0 =>
      scheduler.tick(1.s)
      now.map { deadline1 =>
        assert(deadline0.sinceZero.isZero && deadline1.sinceZero == 1.s)
      }
    }

  "Zero CatsDeadline" in:
    assert(!scheduler.monotonicTime().hasTimeLeft)
    assert(scheduler.monotonicTime().hasElapsed)

  "Late CatsDeadline" in:
    val late = scheduler.monotonicTime()
    assert(late == scheduler.monotonicTime())
    scheduler.tick(1.s)

    assert(late.elapsed == 1.s)
    assert(late.elapsedOrZero == 1.s)
    assert(late.timeLeft == -1.s)
    assert(late.timeLeftOrZero == 0.s)
    assert(late.hasElapsed)
    assert(!late.hasTimeLeft)
    assert(late.isOverdue)

  "Early CatsDeadline" in:
    val early = scheduler.monotonicTime() + 2.s
    scheduler.tick(1.s)

    assert(early.elapsed == -1.s)
    assert(early.elapsedOrZero == 0.s)
    assert(early.timeLeft == 1.s)
    assert(early.timeLeftOrZero == 1.s)
    assert(!early.hasElapsed)
    assert(early.hasTimeLeft)
    assert(!early.isOverdue)

  "Operations" in:
    assert(scheduler.monotonicTime() + 1.s - scheduler.monotonicTime() == 1.s)
    assert(scheduler.monotonicTime() - 1.s - scheduler.monotonicTime() == -1.s)

  "Ordering" in:
    assert((scheduler.monotonicTime() + 1.s).compare(scheduler.monotonicTime()) > 0)
    assert(scheduler.monotonicTime().compare(scheduler.monotonicTime()) == 0)
    assert((scheduler.monotonicTime() - 1.s).compare(scheduler.monotonicTime()) < 0)

    assert(scheduler.monotonicTime() + 1.s > scheduler.monotonicTime())
    assert(scheduler.monotonicTime() + 1.s >= scheduler.monotonicTime())
    assert(scheduler.monotonicTime() >= scheduler.monotonicTime())
    assert(scheduler.monotonicTime() == scheduler.monotonicTime())
    assert(scheduler.monotonicTime() <= scheduler.monotonicTime())
    assert(scheduler.monotonicTime() <= scheduler.monotonicTime() + 1.s)
    assert(scheduler.monotonicTime() < scheduler.monotonicTime() + 1.s)
