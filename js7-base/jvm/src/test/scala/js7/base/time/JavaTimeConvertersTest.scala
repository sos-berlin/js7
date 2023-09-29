package js7.base.time

import java.time.Duration
import js7.base.test.OurTestSuite
import js7.base.time.JavaTimeConverters.*
import scala.concurrent.duration.*

/**
  * @author Joacim Zschimmer
  */
final class JavaTimeConvertersTest extends OurTestSuite:
  "toScala" in:
    assert(Duration.ofMillis(1234).toScala == 1234.millis)
    assert(Duration.ofMillis(-1234).toScala == -1234.millis)
    assert(Duration.ofNanos(111222333444555666L).toScala == 111222333444555666L.nanos)
    assert(Duration.ofNanos(Long.MaxValue).toScala == Long.MaxValue.nanos)
    assert((Duration.ofNanos(Long.MaxValue) plus Duration.ofNanos(1)).toScala == scala.concurrent.duration.Duration.Inf)   // Limit exceeded
    assert(Duration.ofNanos(Long.MinValue + 1).toScala == (Long.MinValue + 1).nanos)
    assert(Duration.ofNanos(Long.MinValue).toScala == scala.concurrent.duration.Duration.MinusInf)   // Limit exceeded

  "toFiniteDuration" in:
    assert(Duration.ofMillis(1234).toFiniteDuration == 1234.millis)
    assert(Duration.ofNanos(111222333444555666L).toFiniteDuration == 111222333444555666L.nanos)
    assert(Duration.ofNanos(Long.MaxValue).toFiniteDuration == Long.MaxValue.nanos)
    assert((Duration.ofNanos(Long.MaxValue) plus Duration.ofNanos(1)).toFiniteDuration == Long.MaxValue.nanos)   // Limit exceeded
    assert(Duration.ofNanos(Long.MinValue).toFiniteDuration == (Long.MinValue + 1).nanos)
    assert((Duration.ofNanos(Long.MinValue) minus Duration.ofNanos(1)).toFiniteDuration == (Long.MinValue + 1).nanos)   // Limit exceeded

  "concurrent Duration to Java Duration" in:
    assert(0.seconds.asJava.isZero)
    assert(123.seconds.asJava == Duration.ofSeconds(123))
    assert(123.millis.asJava == Duration.ofMillis(123))
    assert(123.nanos.asJava == Duration.ofNanos(123))
    assert(292.days.asJava == Duration.ofDays(292))  // ~Maximum
    assert((-292).days.asJava == Duration.ofDays(-292))  // ~Minimum
