package js7.base.time

import js7.base.monixutils.MonixDeadline.syntax._
import monix.execution.Scheduler
import scala.concurrent.duration.{Duration, FiniteDuration}

/** Returns an endless sequence of durations usable to delay operation. */
final class DelayIterator(durations: Seq[FiniteDuration])(implicit scheduler: Scheduler)
extends Iterator[FiniteDuration]
{
  assert(durations.nonEmpty, "DelayIterator must not be empty")

  private val last = durations.last
  @volatile private var it: Iterator[FiniteDuration] = reset()

  def hasNext =
    true

  def next(): FiniteDuration =
    it.next()

  def reset(): Iterator[FiniteDuration] = {
    val start = scheduler.now
    val result = durations.iterator
      .concat(Iterator.continually(last))
      .scanLeft(Duration.Zero)((sum, d) => sum + d)
      .drop(1)
      .map(d => start + d - scheduler.now max Duration.Zero)
    it = result
    result
  }
}
