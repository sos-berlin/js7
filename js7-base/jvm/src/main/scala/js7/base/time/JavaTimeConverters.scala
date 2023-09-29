package js7.base.time

import java.time.{Duration, Instant, ZonedDateTime}
import java.util.concurrent.TimeUnit.NANOSECONDS
import js7.base.log.Logger
import js7.base.time.ScalaTime.*
import scala.concurrent.duration.{FiniteDuration, Duration as ScalaDuration}

object JavaTimeConverters
{
  private val logger = Logger[this.type]
  private val MaxDuration = Duration.ofNanos(Long.MaxValue)
  private val MinDuration = Duration.ofNanos(Long.MinValue + 1)
  private lazy val useJava8Workaround =
    try {
      Duration.ofNanos(Long.MinValue + 1).toNanos
      false
    } catch { case _: ArithmeticException =>
      // https://bugs.openjdk.java.net/browse/JDK-8146747
      logger.debug("Using workaround for bug JDK-8146747")
      true
    }

  implicit final class AsScalaInstant(private val instant: Instant) extends AnyVal
  {
    def toTimestamp: Timestamp =
      JavaTimestamp.ofEpochMilli(instant.toEpochMilli)
  }

  implicit final class RichZonedDateTime(private val zoned: ZonedDateTime) extends AnyVal
  {
    def toTimestamp: Timestamp =
      zoned.toInstant.toTimestamp
  }

  implicit final class AsScalaDuration(private val underlying: Duration) extends AnyVal
  {
    /** Absolute values above ~292 years are converted to Inf respective MinusInf. */
    def toScala: ScalaDuration =
      if (underlying compareTo MaxDuration) > 0 then ScalaDuration.Inf
      else if (underlying compareTo MinDuration) < 0 then ScalaDuration.MinusInf
      else javaToFiniteDuration(underlying)

    /** Absolute values above ~292 years are capped at about ~292 years (±2**63ns). */
    def toFiniteDuration: FiniteDuration =
      if (underlying compareTo MaxDuration) > 0 then FiniteDuration.MaxValue
      else if (underlying compareTo MinDuration) < 0 then FiniteDuration.MinValue
      else javaToFiniteDuration(underlying)
  }

  implicit final class AsJavaFiniteDuration(private val underlying: FiniteDuration) extends AnyVal
  {
    def asJava = Duration.ofNanos(underlying.toNanos)
  }

  private def javaToFiniteDuration(o: Duration): FiniteDuration =
    if o.isZero then
      ZeroDuration // "0 seconds" instead of "0 nanoseconds", for logging
    else if o.isNegative && useJava8Workaround then
      new FiniteDuration(-o.negated.toNanos, NANOSECONDS).toCoarsest
    else
      new FiniteDuration(o.toNanos, NANOSECONDS).toCoarsest
}
