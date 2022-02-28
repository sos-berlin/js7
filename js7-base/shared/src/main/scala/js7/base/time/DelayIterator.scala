package js7.base.time

import com.typesafe.config.Config
import js7.base.monixutils.MonixDeadline.syntax._
import js7.base.problem.Checked.catchNonFatal
import js7.base.problem.{Checked, Problem}
import monix.execution.Scheduler
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.jdk.CollectionConverters._
import scala.jdk.DurationConverters.JavaDurationOps

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

object DelayIterator
{
  /**
   * The delay sequence may be provide as a non-empty list.
   * Then the last element is endlessly repeated.
   * Or a single duration can be given, which is endlessly repeated.
   */
  def fromConfig(config: Config, key: String)(implicit s: Scheduler): Checked[DelayIterator] =
    catchNonFatal(config.getDuration(key).toScala :: Nil)
      .orElse(catchNonFatal(
        config.getDurationList(key).asScala.view.map(_.toScala).toVector))
      .flatMap(durations =>
        if (durations.isEmpty)
          Left(Problem(s"Setting '$key' must not be empty"))
        else
          Right(new DelayIterator(durations)))
}
