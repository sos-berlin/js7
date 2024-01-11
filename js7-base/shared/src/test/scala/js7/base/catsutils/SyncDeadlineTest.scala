package js7.base.catsutils

import cats.effect.unsafe.Scheduler
import js7.base.test.TestCatsEffect
import js7.base.time.ScalaTime.*
import org.scalatest.freespec.AnyFreeSpec

final class SyncDeadlineTest extends AnyFreeSpec, TestCatsEffect:

  private given Scheduler = ioRuntime.scheduler

  private val now = SyncDeadline.now
  private val later = now + 1.h

  "+" in:
    assert((now + 3.s).nanosSinceZero == now.nanosSinceZero + 3.s.toNanos)

  "- FiniteDuration" in:
    assert((now - 3.s).nanosSinceZero == now.nanosSinceZero - 3.s.toNanos)

  "- SyncDeadline" in:
    val a = now + 3.s
    assert(a - now == 3.s)

  "isOverdue, hasElapsed" in:
    assert(now.isOverdue)
    assert(now.hasElapsed)
    while SyncDeadline.now == now do {}
    assert(now.isOverdue)
    assert(now.hasElapsed)
    assert(!later.isOverdue)
    assert(!later.hasElapsed)

  "hasTimeLeft" in:
    assert(!now.hasTimeLeft)
    assert(later.hasTimeLeft)

  "timeLeftOrZero" in:
    assert(now.timeLeftOrZero == 0.s)
    assert(later.timeLeftOrZero > 0.s && later.timeLeftOrZero <= 1.h)

  "elapsedOrZero" in:
    assert(now.elapsedOrZero > 0.s && now.elapsedOrZero < 3.s)
    assert(later.elapsedOrZero == 0.s)

  "elapsed" in:
    assert(now.elapsed > 0.s && now.elapsed < 3.s)
    assert(later.elapsed <= 1.h && later.elapsed > -2.h)

  "compare" in:
    assert(now.compare(later) == -1)
    assert(now.compare(now) == 0)
    assert(later.compare(now) == +1)

    // Overflow:
    assert(SyncDeadline.fromMonotonicNanos(Long.MinValue - 1) <
      SyncDeadline.fromMonotonicNanos(Long.MinValue))
    assert(SyncDeadline.fromMonotonicNanos(Long.MaxValue + 1) >
      SyncDeadline.fromMonotonicNanos(Long.MaxValue))

    assert(now <= later)
    assert(now <= now)
    assert(now == now)
    assert(now >= now)
    assert(later >= now)
    assert(later > now)

  "toCatsDeadline" in:
    assert(now.toCatsDeadline == CatsDeadline.fromMonotonicNanos(now.nanosSinceZero))

  "toString" in:
    assert((now - ((17 * 24).h + 10.s)).toString == "+17days")
    assert((now + ((17 * 24).h + 10.s)).toString == "-17days")
