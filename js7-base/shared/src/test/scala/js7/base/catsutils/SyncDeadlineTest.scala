package js7.base.catsutils

import cats.effect.unsafe.IORuntime
import js7.base.test.OurTestControl
import js7.base.time.ScalaTime.*
import org.scalatest.freespec.AnyFreeSpec

final class SyncDeadlineTest extends AnyFreeSpec:

  "+" in:
    OurTestControl.executeEmbed:
      SyncDeadline.usingNow: now ?=>
        assert((now + 3.s).nanosSinceZero == now.nanosSinceZero + 3.s.toNanos)

  "- FiniteDuration" in:
    OurTestControl.executeEmbed:
      SyncDeadline.usingNow: now ?=>
        assert((now - 3.s).nanosSinceZero == now.nanosSinceZero - 3.s.toNanos)

  "- SyncDeadline" in:
    OurTestControl.executeEmbed:
      SyncDeadline.usingNow: now ?=>
        val a = now + 3.s
        assert(a - now == 3.s)

  "isOverdue, hasElapsed" in:
    OurTestControl.executeEmbed:
      SyncDeadline.usingNow: now ?=>
        val early = now - 1.h
        val later = now + 1.h

        assert(early.isOverdue)
        assert(early.hasElapsed)
        assert(!later.isOverdue)
        assert(!later.hasElapsed)

  "hasTimeLeft" in:
    OurTestControl.executeEmbed:
      SyncDeadline.usingNow: now ?=>
        val early = now - 1.h
        val later = now + 1.h

        assert(!early.hasTimeLeft)
        assert(later.hasTimeLeft)

  "timeLeftOrZero" in:
    OurTestControl.executeEmbed:
      SyncDeadline.usingNow: now ?=>
        val early = now - 1.h
        val later = now + 1.h

        assert(early.timeLeftOrZero == 0.s)
        assert(later.timeLeftOrZero == 1.h)

  "elapsedOrZero" in:
    OurTestControl.executeEmbed:
      SyncDeadline.usingNow: now ?=>
        val early = now - 1.h
        val later = now + 1.h

        assert(early.elapsedOrZero == 1.h)
        assert(later.elapsedOrZero == 0.s)

  "elapsed" in:
    OurTestControl.executeEmbed:
      SyncDeadline.usingNow: now ?=>
        val early = now - 1.h
        val later = now + 1.h

        assert(early.elapsed == 1.h)
        assert(later.elapsed == -1.h)

  "compare" in:
    OurTestControl.executeEmbed:
      SyncDeadline.usingNow: now ?=>
        val early = now - 1.h
        val later = now + 1.h

        assert(early.compare(later) == -1)
        assert(early.compare(early) == 0)
        assert(later.compare(early) == +1)

        assert(early <= early)
        assert(early <= later)
        assert(early == early)
        assert(early >= early)
        assert(later >= early)
        assert(later > early)

  "compare respects long integer overflow" in:
    OurTestControl.executeEmbed:
      SyncDeadline.usingNow:
        val min = SyncDeadline.fromNanos(Long.MinValue)
        val max = SyncDeadline.fromNanos(Long.MaxValue)
        assert(min.nanosSinceZero - 1 > min.nanosSinceZero && min - 1.ns < min)
        assert(max.nanosSinceZero + 1 < max.nanosSinceZero && max + 1.ns > max)

  "toCatsDeadline" in:
    OurTestControl.executeEmbed:
      SyncDeadline.usingNow: now ?=>
        assert(now.toCatsDeadline == CatsDeadline.fromMonotonicNanos(now.nanosSinceZero))

  //"toString" in:
  //  assert((now - ((17 * 24).h + 10.s)).toString == "+17days")
  //  assert((now + ((17 * 24).h + 10.s)).toString == "-17days")
