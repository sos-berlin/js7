package com.sos.jobscheduler.common.time

import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.common.time.TimeoutWithSteps._
import scala.concurrent.duration.FiniteDuration

/** Eine in Schritte unterteilte Frist.
  */
final case class TimeoutWithSteps(timeout: FiniteDuration, step: FiniteDuration) {

  /** Liefert einen Iterator mit den Zeitpunkten startInstant, startInstant + step, ..., startInstant + timeout.
    */
  def toMillisInstantIterator(start: Timestamp) =
    millisInstantIterator(start.toEpochMilli, timeout.toMillis, step.toMillis)
}

object TimeoutWithSteps
{
  /** Liefert einen Iterator mit den Zeitpunkten startInstant, startInstant + step, ..., startInstant + timeout.
  */
  def millisInstantIterator(startMillis: Long, timeout: Long, step: Long): Iterator[Long] =
    ((0L to timeout - 1 by step).toIterator map {startMillis + _}) ++ Iterator(startMillis + timeout)
}
