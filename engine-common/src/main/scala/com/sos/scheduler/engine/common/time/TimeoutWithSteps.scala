package com.sos.scheduler.engine.common.time

import com.sos.scheduler.engine.common.time.TimeoutWithSteps._
import java.time.{Duration, Instant}

/** Eine in Schritte unterteilte Frist.
  */
final case class TimeoutWithSteps(timeout: Duration, step: Duration) {

  /** Liefert einen Iterator mit den Zeitpunkten startInstant, startInstant + step, ..., startInstant + timeout.
    */
  def toMillisInstantIterator(startInstant: Instant) =
    millisInstantIterator(startInstant.toEpochMilli, timeout.toMillis, step.toMillis)
}

object TimeoutWithSteps {
/** Liefert einen Iterator mit den Zeitpunkten startInstant, startInstant + step, ..., startInstant + timeout.
  */
  def millisInstantIterator(startInstant: Long, timeout: Long, step: Long): Iterator[Long] =
    ((0L to timeout - 1 by step).toIterator map {startInstant + _}) ++ Iterator(startInstant + timeout)
}
