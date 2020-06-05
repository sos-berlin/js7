package js7.common.time

import js7.common.time.TimeoutWithSteps._
import scala.concurrent.duration._

/** Eine in Schritte unterteilte Frist.
  */
final case class TimeoutWithSteps(timeout: FiniteDuration, step: FiniteDuration) {

  /** Liefert einen Iterator mit den Zeitpunkten startInstant, startInstant + step, ..., startInstant + timeout.
    */
  def toDeadlineIterator(start: Deadline): Iterator[Deadline] =
    deadlineIterator(start, timeout, step)
}

object TimeoutWithSteps
{
  /** Liefert einen Iterator mit den Zeitpunkten startInstant, startInstant + step, ..., startInstant + timeout.
  */
  def deadlineIterator(start: Deadline, timeout: FiniteDuration, step: FiniteDuration): Iterator[Deadline] =
    (0L to timeout.toMillis - 1 by step.toMillis).iterator.map(o => start + o.millis) ++
      Iterator(start + timeout)
}
