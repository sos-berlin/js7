package js7.base.time

import js7.base.monixutils.MonixDeadline
import js7.base.time.TimeoutWithSteps.deadlineIterator
import scala.concurrent.duration.*

/** Eine in Schritte unterteilte Frist.
  */
final case class TimeoutWithSteps(timeout: FiniteDuration, step: FiniteDuration):

  /** Liefert einen Iterator mit den Zeitpunkten startInstant, startInstant + step, ..., startInstant + timeout.
    */
  def toDeadlineIterator(start: MonixDeadline): Iterator[MonixDeadline] =
    deadlineIterator(start, timeout, step)


object TimeoutWithSteps:
  /** Liefert einen Iterator mit den Zeitpunkten startInstant, startInstant + step, ..., startInstant + timeout.
  */
  def deadlineIterator(start: MonixDeadline, timeout: FiniteDuration, step: FiniteDuration): Iterator[MonixDeadline] =
    (0L to timeout.toMillis - 1 by step.toMillis).iterator.map(o => start + o.millis) ++
      Iterator(start + timeout)
