package com.sos.jobscheduler.common.event

import com.google.inject.ImplementedBy

/**
  * @author Joacim Zschimmer
  */
@ImplementedBy(classOf[EventIdClock.JavaClock])
trait EventIdClock {
  /** Current time in milliseconds since 1970-01-01 UTC, like Java currentTimeMillis. */
  def currentTimeMillis: Long
}

object EventIdClock {
  def Default: EventIdClock = new JavaClock

  final class JavaClock extends EventIdClock {
    def currentTimeMillis = System.currentTimeMillis
  }

  final class Fixed(val currentTimeMillis: Long) extends EventIdClock
}
