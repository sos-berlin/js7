package com.sos.scheduler.engine.common.time

import java.time._
import org.joda
import scala.language.implicitConversions

/**
 * @author Joacim Zschimmer
 */
object JodaJavaTimeConversions {
  object implicits {
    implicit def asJodaDuration(o: Duration): joda.time.Duration = joda.time.Duration.millis(o.toMillis)

    implicit def asJodaInstant(o: Instant): joda.time.Instant = new joda.time.Instant(o.toEpochMilli)

    implicit def asJavaDuration(o: joda.time.Duration): Duration = Duration.ofMillis(o.getMillis)

    implicit def asJavaInstant(o: joda.time.ReadableInstant): Instant = Instant.ofEpochMilli(o.getMillis)
  }
}
