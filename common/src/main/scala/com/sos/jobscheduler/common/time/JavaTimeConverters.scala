package com.sos.jobscheduler.common.time

import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.base.time.ScalaTime._
import java.time.{Duration, Instant}
import java.util.concurrent.TimeUnit.NANOSECONDS
import scala.concurrent.duration.{FiniteDuration, Duration => ScalaDuration}

object JavaTimeConverters
{
  private val logger = Logger(getClass)
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

  implicit final class AsScalaInstant(private val delegate: Instant) extends AnyVal
  {
    def toTimestamp = Timestamp.ofEpochMilli(delegate.toEpochMilli)
  }

  implicit final class AsScalaDuration(private val underlying: Duration) extends AnyVal
  {
    /** Absolute values above ~292 years are converted to Inf respective MinusInf. */
    def toScala: ScalaDuration =
      if ((underlying compareTo MaxDuration) > 0) ScalaDuration.Inf
      else if ((underlying compareTo MinDuration) < 0) ScalaDuration.MinusInf
      else javaToFiniteDuration(underlying)

    /** Absolute values above ~292 years are capped at about ~292 years (±2**63ns). */
    def toFiniteDuration: FiniteDuration =
      if ((underlying compareTo MaxDuration) > 0) FiniteDuration.MaxValue
      else if ((underlying compareTo MinDuration) < 0) FiniteDuration.MinValue
      else javaToFiniteDuration(underlying)
  }

  implicit final class AsJavaFiniteDuration(private val underlying: FiniteDuration) extends AnyVal
  {
    def asJava = Duration.ofNanos(underlying.toNanos)
  }

  private def javaToFiniteDuration(o: Duration): FiniteDuration =
    if (o.isNegative && useJava8Workaround)
      new FiniteDuration(-o.negated.toNanos, NANOSECONDS).toCoarsest
    else
      new FiniteDuration(o.toNanos, NANOSECONDS).toCoarsest
}
