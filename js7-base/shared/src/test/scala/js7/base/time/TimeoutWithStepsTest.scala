package js7.base.time

import js7.base.monixutils.MonixDeadline.syntax.DeadlineSchedule
import js7.base.time.ScalaTime._
import js7.base.time.TimeoutWithSteps.deadlineIterator
import monix.execution.schedulers.TestScheduler
import org.scalatest.freespec.AnyFreeSpec

final class TimeoutWithStepsTest extends AnyFreeSpec
{
  private val scheduler = TestScheduler()

  "instantIterator" in {
    assert(deadlineIterator(scheduler.now + 100.ms, 7.ms, 7.ms).toList == scheduler.now + 100.ms :: scheduler.now + 107.ms :: Nil)
    assert(deadlineIterator(scheduler.now + 100.ms, 7.ms, 3.ms).toList == scheduler.now + 100.ms :: scheduler.now + 103.ms :: scheduler.now + 106.ms :: scheduler.now + 107.ms :: Nil)
    assert(deadlineIterator(scheduler.now + 100.ms, 3.ms, 7.ms).toList == scheduler.now + 100.ms :: scheduler.now + 103.ms :: Nil)
  }
}
