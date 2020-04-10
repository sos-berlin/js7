package com.sos.jobscheduler.base.monixutils

import com.sos.jobscheduler.base.monixutils.MonixDeadline.now
import com.sos.jobscheduler.base.time.ScalaTime._
import monix.execution.schedulers.TestScheduler
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class MonixDeadlineTest extends AnyFreeSpec
{
  private implicit val scheduler = TestScheduler()

  "Late MonixDeadline" in {
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
  }

  "Early MonixDeadline" in {
    val early = now + 2.s
    scheduler.tick(1.s)

    assert(early.elapsed == -1.s)
    assert(early.elapsedOrZero == 0.s)
    assert(early.timeLeft == 1.s)
    assert(early.timeLeftOrZero == 1.s)
    assert(!early.hasElapsed)
    assert(early.hasTimeLeft)
    assert(!early.isOverdue)
  }

  "Operations" in {
    assert(now + 1.s - now == 1.s)
    assert(now - 1.s - now == -1.s)
  }

  "Ordering" in {
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
  }
}
