package js7.base.time

import cats.effect.unsafe.Scheduler
import js7.base.utils.CatsUtils.syntax.monotonicTime
import scala.collection.AbstractIterator
import scala.concurrent.duration.{Duration, FiniteDuration}

/** Returns an endless sequence of durations usable to delay operation. */
final class DelayIterator(durations: Seq[FiniteDuration])(using scheduler: Scheduler)
extends AbstractIterator[FiniteDuration]:

  assert(durations.nonEmpty, "DelayIterator must not be empty")

  private val last = durations.last
  @volatile private var it: Iterator[FiniteDuration] = reset()

  def hasNext =
    true

  def next(): FiniteDuration =
    it.next()

  def reset(): Iterator[FiniteDuration] =
    val start = scheduler.monotonicTime()
    val result = durations.iterator
      .concat(Iterator.continually(last))
      .scanLeft(Duration.Zero)((sum, d) => sum + d)
      .drop(1)
      .map(d => start + d - scheduler.monotonicTime() max Duration.Zero)
    it = result
    result
