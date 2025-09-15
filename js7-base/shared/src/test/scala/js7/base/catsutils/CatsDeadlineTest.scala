package js7.base.catsutils

import cats.effect.IO
import cats.effect.testkit.TestControl
import js7.base.test.OurAsyncTestSuite
import js7.base.time.ScalaTime.*

final class CatsDeadlineTest extends OurAsyncTestSuite:

  "now" in:
    TestControl.executeEmbed:
      for
        d0 <- CatsDeadline.now
        d1 <- CatsDeadline.now.delayBy(1.s)
      yield assert:
        d0.monotonic.isZero && d1.monotonic == 1.s && d1 - d0 == 1.s

  "Zero CatsDeadline" in:
    TestControl.executeEmbed:
      for
        deadline <- CatsDeadline.now
        hasElapsed <- deadline.hasElapsed
        hasTimeLeft <- deadline.hasTimeLeft
      yield assert:
        hasElapsed && !hasTimeLeft

  "Late CatsDeadline" in:
    TestControl.executeEmbed:
      for
        late <- CatsDeadline.now.andWait(1.s)
        _ <- for o <- late.timeLeft yield assert(o == -1.s)
        _ <- for o <- late.timeLeftOrZero yield assert(o == 0.s)
        _ <- for o <- late.hasTimeLeft yield assert(!o)
        _ <- for o <- late.elapsed yield assert(o == 1.s)
        _ <- for o <- late.elapsedOrZero yield assert(o == 1.s)
        _ <- for o <- late.hasElapsed yield assert(o)
        _ <- for o <- late.isOverdue yield assert(o)
      yield succeed

  "Early CatsDeadline" in:
    TestControl.executeEmbed:
      for
        early <- CatsDeadline.now.map(_ + 2.s)
        _ <- IO.sleep(1.s)
        _ <- for o <- early.timeLeft yield assert(o == 1.s)
        _ <- for o <- early.timeLeftOrZero yield assert(o == 1.s)
        _ <- for o <- early.hasTimeLeft yield assert(o)
        _ <- for o <- early.elapsed yield assert(o == -1.s)
        _ <- for o <- early.elapsedOrZero yield assert(o == 0.s)
        _ <- for o <- early.hasElapsed yield assert(!o)
        _ <- for o <- early.isOverdue yield assert(!o)
      yield succeed

  "+ and -" in:
    TestControl.executeEmbed:
      for deadline <- CatsDeadline.now yield
        assert(deadline + 1.s - deadline == 1.s)
        assert(deadline - 1.s - deadline == -1.s)

  "Ordering" in:
    TestControl.executeEmbed:
      for deadline <- CatsDeadline.now
      yield
        assert((deadline + 1.s).compare(deadline) > 0)
        assert(deadline.compare(deadline) == 0)
        assert((deadline - 1.s).compare(deadline) < 0)
        assert(deadline + 1.s > deadline)
        assert(deadline + 1.s >= deadline)
        assert(deadline >= deadline)
        assert(deadline == deadline)
        assert(deadline <= deadline)
        assert(deadline <= deadline + 1.s)
        assert(deadline < deadline + 1.s)

  //"toString" in:
  //  TestControl.executeEmbed:
  //    for string <- CatsDeadline.now.map(_ + 1.s).map(_.toString)
  //    yield assert(string == "1.s??")
