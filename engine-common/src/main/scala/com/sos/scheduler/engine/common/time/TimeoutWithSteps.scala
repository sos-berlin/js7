package com.sos.scheduler.engine.common.time

import org.joda.time.{Duration, ReadableInstant}

/** Eine in Schritte unterteilte Frist.
  */
final case class TimeoutWithSteps(timeout: Duration, step: Duration) {
  import TimeoutWithSteps._
  /** Liefert einen Iterator mit den Zeitpunkten startInstant, startInstant + step, ..., startInstant + timeout.
    */
  def toMillisInstantIterator(startInstant: ReadableInstant) =
    millisInstantIterator(startInstant.getMillis, timeout.getMillis, step.getMillis)
}

object TimeoutWithSteps {
/** Liefert einen Iterator mit den Zeitpunkten startInstant, startInstant + step, ..., startInstant + timeout.
  */
  def millisInstantIterator(startInstant: Long, timeout: Long, step: Long): Iterator[Long] =
    ((0L to timeout - 1 by step).toIterator map {startInstant + _}) ++ Iterator(startInstant + timeout)
}
