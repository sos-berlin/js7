package com.sos.scheduler.engine.common.time

import com.sos.scheduler.engine.common.time.ScalaTime._
import java.time.format.DateTimeParseException
import java.time.{Duration, Instant, LocalTime}
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner
import scala.concurrent.duration.{DurationInt, DurationLong}

@RunWith(classOf[JUnitRunner])
final class ScalaTimeTest extends FreeSpec {

  "Duration" - {
    "Int.μs" in {
      (7.μs: Duration).toNanos shouldEqual 7000
    }

    "Long.μs" in {
      (7L.μs: Duration).toNanos shouldEqual 7000
    }

    "Int.ms" in {
      (7.ms: Duration).toMillis shouldEqual 7
    }

    "Long.ms" in {
      (7L.ms: Duration).toMillis shouldEqual 7
    }

    "Int.s" in {
      (7.s: Duration).getSeconds shouldEqual 7
      (7.s: Duration).toMillis shouldEqual (7*1000)
    }

    "Long.s" in {
      (7L.s: Duration).getSeconds shouldEqual 7
      (7L.s: Duration).toMillis shouldEqual (7*1000)
    }

    "BigDecimal.s and toBigDecimal" in {
      List(
        BigDecimal(0) → Duration.ZERO,
        BigDecimal(1) → Duration.ofSeconds(1),
        BigDecimal("0.001") → Duration.ofMillis(1),
        BigDecimal("111222333444555666.123456789") → Duration.ofSeconds(111222333444555666L, 123456789),
        BigDecimal("-111222333444555666.123456789") → Duration.ofSeconds(111222333444555666L, 123456789).negated)
      .foreach { case (bigDecimal, duration) ⇒
        assert(bigDecimal.s == duration)
        assert(bigDecimalToDuration(bigDecimal) == duration)
        assert(duration.toBigDecimal == bigDecimal)
      }
    }

//    "Int.min" in {
//      (7.min: Duration).toMinutes shouldEqual 7
//      (7.min: Duration).toMillis shouldEqual (7*60*1000)
//    }
//
//    "Long.min" in {
//      (7L.min: Duration).toHours shouldEqual 7
//      (7L.min: Duration).toMillis shouldEqual (7*60*1000)
//    }

    "Int.h" in {
      (7.h: Duration).toHours shouldEqual 7
      (7.h: Duration).toMillis shouldEqual (7*3600*1000)
    }

    "Long.h" in {
      (7L.h: Duration).toHours shouldEqual 7
      (7L.h: Duration).toMillis shouldEqual (7*3600*1000)
    }

//    "Int.days" in {
//      (7.days: Duration).toDays shouldEqual 7
//      (7.days: Duration).toMillis shouldEqual (7*24*3600*1000)
//    }
//    "Long.days" in {
//      (7L.days: Duration).toDays shouldEqual 7
//      (7L.days: Duration).toMillis shouldEqual (7*24*3600*1000)
//    }

    "randomDuration" in {
      val durations = 1 to 1000 map { _ ⇒ randomDuration(2.s) }
      val min = durations.min
      val max = durations.max
      assert(min >= 0.s && min <= 100.ms)
      assert(max >= 1900.ms && min <= 2.s)
    }

    "-Duration" in {
      assert(-7.s == Duration.ofSeconds(-7))
    }

    "Duration + Duration" in {
      (7.s + 2.ms: Duration).toMillis shouldEqual (7*1000 + 2)
    }

    "Duration - Duration" in {
      (7.s - 2.ms: Duration).toMillis shouldEqual (7*1000 - 2)
    }

    "Duration * Int" in {
      (7.s * 2).toMillis shouldEqual 14000
    }

    "Duration / Int" in {
      (7.s / 2).toMillis shouldEqual 3500
    }

    "Duration * BigDecimal" in {
      (3.s * BigDecimal("2.5")).toMillis shouldEqual 7500
      (3.s * 2.5).toMillis shouldEqual 7500
    }

    "Duration / BigDecimal" in {
      (10.s / BigDecimal("2.5")).toMillis shouldEqual 4000
      (10.s / 2.5).toMillis shouldEqual 4000
    }

    "Int * Duration" in {
      (3 * 7.s: Duration).toMillis shouldEqual (3 * 7*1000)
    }

    "Long * Duration" in {
      (3L * 7.s: Duration).toMillis shouldEqual (3 * 7*1000)
    }

    "min" in {
      assert((1.s min 2.s) == 1.s)
      assert((-1.s min -2.s) == -2.s)
    }

    "max" in {
      assert((1.s max 2.s) == 2.s)
      assert((-1.s max -2.s) == -1.s)
    }

    "pretty" in {
      Long.MaxValue.s.pretty shouldEqual s"${Long.MaxValue}s"   // No overflow
      0.s.pretty shouldEqual "0s"
      1.s.pretty shouldEqual "1s"
      1200.ms.pretty shouldEqual "1.2s"
      1230.ms.pretty shouldEqual "1.23s"
      1234.ms.pretty shouldEqual "1.234s"
      10.ms.pretty shouldEqual "0.01s"
      1.ms.pretty shouldEqual "0.001s"
      (-10).ms.pretty shouldEqual "-0.01s"
      (-1).s.pretty shouldEqual "-1s"
      Duration.ofNanos(1000000).pretty shouldEqual "0.001s"
      Duration.ofNanos(1000001).pretty shouldEqual "0.001s"
      Duration.ofNanos(1001000).pretty shouldEqual "1001µs"
      Duration.ofNanos(100000).pretty shouldEqual "100µs"
      Duration.ofNanos(100).pretty shouldEqual "100ns"
    }

    "parseDuration" in {
      assert(parseDuration("0.123s") == 123.ms)
      assert(parseDuration("PT0.123S") == 123.ms)
      assert(parseDuration("1m") == 60.s)
      assert(parseDuration("1h2m3s") == 1.h + 2*60.s + 3.s)
      intercept[DateTimeParseException] { parseDuration("1d") }
      assert(parseDuration("P1d") == 24 * 3600.s)
      intercept[DateTimeParseException] { parseDuration(".123s") }
      intercept[DateTimeParseException] { parseDuration("1") }
    }

    "toConcurrent" in {
      assert(1234.ms.toConcurrent == 1234.millis)
      assert(Duration.ofNanos(111222333444555666L).toConcurrent == 111222333444555666L.nanos)
      assert(Duration.ofNanos(Long.MaxValue).toConcurrent == Long.MaxValue.nanos)
      assert((Duration.ofNanos(Long.MaxValue) + Duration.ofNanos(1)).toConcurrent == scala.concurrent.duration.Duration.Inf)   // Limit exceeded
    }

    "toFiniteDuration" in {
      assert(1234.ms.toFiniteDuration== 1234.millis)
      assert(Duration.ofNanos(111222333444555666L).toFiniteDuration == 111222333444555666L.nanos)
      assert(Duration.ofNanos(Long.MaxValue).toFiniteDuration == Long.MaxValue.nanos)
      assert((Duration.ofNanos(Long.MaxValue) + Duration.ofNanos(1)).toFiniteDuration == Long.MaxValue.nanos)   // Limit exceeded
    }

//    "Duration * Int" in {
//      ((7.s * 3): Duration).toMillis shouldEqual (7*1000 * 3)
//    }

    "Duration < Duration" in {
      assert(!(7.s < 2.s))
      assert(!(7.s <= 2.s))
      assert(7.s > 2.s)
      assert(7.s >= 2.s)
      assert(2.s < 7.s)
      assert(2.s <= 7.s)
      assert(!(2.s > 2.s))
      assert(2.s >= 2.s)
      assert(!(7.s < 7.s))
      assert(7.s <= 7.s)
    }
  }

  "Instant" - {
    "Instant + Duration" in {
      (Instant.ofEpochMilli(7) + 2.ms: Instant) shouldEqual Instant.ofEpochMilli(7 + 2)
    }

    "Instant - Duration" in {
      (Instant.ofEpochMilli(7) - 2.ms: Instant) shouldEqual Instant.ofEpochMilli(7 - 2)
    }

    "Instant - Instant" in {
      (Instant.ofEpochMilli(7) - Instant.ofEpochMilli(2): Duration) shouldEqual Duration.ofMillis(7 - 2)
    }

    "Instant max Instant" in {
      Instant.ofEpochMilli(1) max Instant.ofEpochMilli(2) shouldEqual Instant.ofEpochMilli(2)
    }

    "Instant min Instant" in {
      Instant.ofEpochMilli(1) min Instant.ofEpochMilli(2) shouldEqual Instant.ofEpochMilli(1)
    }

    "roundTo" in {
      assert(Instant.parse("2015-01-01T12:01:01.499Z").roundTo(1.s) == Instant.parse("2015-01-01T12:01:01Z"))
      assert(Instant.parse("2015-01-01T12:01:01.500Z").roundTo(1.s) == Instant.parse("2015-01-01T12:01:02Z"))
      assert(Instant.parse("2015-01-01T12:01:29.999Z").roundTo(60.s) == Instant.parse("2015-01-01T12:01:00Z"))
      assert(Instant.parse("2015-01-01T12:01:30Z"    ).roundTo(60.s) == Instant.parse("2015-01-01T12:02:00Z"))
    }

    "roundDownTo" in {
      assert(Instant.parse("2015-01-01T12:01:01.499Z").roundDownTo(1.s) == Instant.parse("2015-01-01T12:01:01Z"))
      assert(Instant.parse("2015-01-01T12:01:01.500Z").roundDownTo(1.s) == Instant.parse("2015-01-01T12:01:01Z"))
      assert(Instant.parse("2015-01-01T12:01:29.999Z").roundDownTo(60.s) == Instant.parse("2015-01-01T12:01:00Z"))
      assert(Instant.parse("2015-01-01T12:01:30Z"    ).roundDownTo(60.s) == Instant.parse("2015-01-01T12:01:00Z"))
    }
  }

//  "DateTime" - {
//    "DateTime + Duration" in {
//      (new DateTime(7) + 2.ms: DateTime) shouldEqual new DateTime(7 + 2)
//    }
//    "DateTime - Duration" in {
//      (new DateTime(7) - 2.ms: DateTime) shouldEqual new DateTime(7 - 2)
//    }
//    "DateTime - DateTime" in {
//      (new DateTime(7) - new DateTime(2): Duration) shouldEqual new Duration(7 - 2)
//    }
//  }

  "LocalTime" - {
    "LocalTime < LocalTime" in {
      LocalTime.ofSecondOfDay(7) < LocalTime.ofSecondOfDay(2) shouldEqual false
      LocalTime.ofSecondOfDay(7) <= LocalTime.ofSecondOfDay(2) shouldEqual false
      LocalTime.ofSecondOfDay(7) > LocalTime.ofSecondOfDay(2) shouldEqual true
      LocalTime.ofSecondOfDay(7) >= LocalTime.ofSecondOfDay(2) shouldEqual true

      LocalTime.ofSecondOfDay(2) < LocalTime.ofSecondOfDay(7) shouldEqual true
      LocalTime.ofSecondOfDay(2) <= LocalTime.ofSecondOfDay(7) shouldEqual true
      LocalTime.ofSecondOfDay(2) > LocalTime.ofSecondOfDay(7) shouldEqual false
      LocalTime.ofSecondOfDay(2) >= LocalTime.ofSecondOfDay(7) shouldEqual false

      LocalTime.ofSecondOfDay(7) < LocalTime.ofSecondOfDay(7) shouldEqual false
      LocalTime.ofSecondOfDay(7) <= LocalTime.ofSecondOfDay(7) shouldEqual true
      LocalTime.ofSecondOfDay(7) > LocalTime.ofSecondOfDay(7) shouldEqual false
      LocalTime.ofSecondOfDay(7) >= LocalTime.ofSecondOfDay(7) shouldEqual true
    }
  }
}
