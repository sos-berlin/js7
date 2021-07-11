package js7.base.time

import cats.syntax.show._
import java.time.format.DateTimeParseException
import java.time.{Duration, Instant, LocalDateTime, LocalTime, ZoneId}
import js7.base.convert.As
import js7.base.time.JavaTime._
import js7.base.time.JavaTimestamp.specific._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._
import scala.util.Random

final class JavaTimeTest extends AnyFreeSpec {

  "Duration" - {
    "Int.µs" in {
      assert(7.µs == Duration.ofNanos(7000))
      assert(Int.MaxValue.µs == Duration.ofNanos(1000L * Int.MaxValue))
    }

    "Long.µs" in {
      assert(7L.µs == Duration.ofNanos(7000))
      assert(Long.MaxValue.µs == Duration.ofSeconds(Long.MaxValue / 1000000, Long.MaxValue % 1000000 * 1000))
    }

    "Int.ms" in {
      assert(7.ms == Duration.ofMillis(7))
      assert(Int.MaxValue.ms == Duration.ofMillis(Int.MaxValue))
    }

    "Long.ms" in {
      assert(7L.ms == Duration.ofMillis(7))
      assert(Long.MaxValue.ms == Duration.ofMillis(Long.MaxValue))
    }

    "Int.s" in {
      (7.s: Duration).getSeconds shouldEqual 7
      assert(Long.MaxValue.s == Duration.ofSeconds(Long.MaxValue))
    }

    "Long.s" in {
      (7L.s: Duration).getSeconds shouldEqual 7
      (7L.s: Duration).toMillis shouldEqual (7*1000)
    }

    "BigDecimal.s and toBigDecimal" in {
      List(
        BigDecimal(0) -> Duration.ZERO,
        BigDecimal(1) -> Duration.ofSeconds(1),
        BigDecimal("0.001") -> Duration.ofMillis(1),
        BigDecimal("111222333444555666.123456789") -> Duration.ofSeconds(111222333444555666L, 123456789),
        BigDecimal("-111222333444555666.123456789") -> Duration.ofSeconds(111222333444555666L, 123456789).negated)
      .foreach { case (bigDecimal, duration) =>
        assert(bigDecimal.s == duration)
        assert(bigDecimalToDuration(bigDecimal) == duration)
        assert(duration.toBigDecimal == bigDecimal)
      }
      intercept[ArithmeticException] {
        bigDecimalToDuration(BigDecimal("0.1112223334"))
      }
    }

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
      val durations = (1 to 1000).map(_ => randomDuration(2.s))
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
      assert(0.s.pretty == "0s")
      assert(Duration.ofNanos(1).pretty == "1ns")
      assert(Duration.ofNanos(12).pretty == "12ns")
      assert(Duration.ofNanos(123).pretty == "0.123µs")
      assert(Duration.ofNanos(1234).pretty == "1.23µs")
      assert(Duration.ofNanos(12345).pretty == "12.3µs")
      assert(Duration.ofNanos(123456).pretty == "0.123ms")
      assert(Duration.ofNanos(1234567).pretty == "1.23ms")
      assert(10.ms.pretty == "10ms")
      assert((-10).ms.pretty == "-10ms")
      assert(Duration.ofNanos(12345678).pretty == "12.3ms")
      assert(Duration.ofNanos(123456789).pretty == "0.123s")
      assert(1.ms.pretty == "1ms")
      assert(1200.ms.pretty == "1.2s")
      assert(1230.ms.pretty == "1.23s")
      assert(1234.ms.pretty == "1.234s")
      assert(1.s.pretty == "1s")
      assert((-1).s.pretty == "-1s")
      assert(179.s.pretty == "179s")
      assert(180.s.pretty == "3min")
      assert((5*60+1).s.pretty == "5min")
      assert((-5*60-1).s.pretty == "-5min")
      assert((5*60*60).s.pretty == "5h")
      assert((-5*60*60).s.pretty == "-5h")
      assert((71*60*60).s.pretty == "71h")
      assert((-71*60*60).s.pretty == "-71h")
      assert((72*60*60).s.pretty == "3days")
      assert((-72*60*60).s.pretty == "-3days")
      assert((365*24*60*60).s.pretty == "365days")
      assert((-365*24*60*60).s.pretty == "-365days")
      assert((366*24*60*60).s.pretty == "12~months")
      assert((-366*24*60*60).s.pretty == "-12~months")
      assert(((2*366+365)*24*60*60).s.pretty == "36~months")
      assert((-(2*366+365)*24*60*60).s.pretty == "-36~months")
      assert((3*366*24*60*60).s.pretty == "3~years")
      assert((-3*366*24*60*60).s.pretty == "-3~years")
      assert(Long.MaxValue.s.pretty == s"${Long.MaxValue / (366*24*60*60)}~years")   // No overflow
    }

    "parseDuration" in {
      intercept[DateTimeParseException] { parseDuration(".1s") }
      intercept[DateTimeParseException] { parseDuration(".1") }
      assert(parseDuration("0.123s") == 123.ms)
      assert(parseDuration("PT0.123S") == 123.ms)
      assert(parseDuration("1m") == 60.s)
      assert(parseDuration("1h2m3s") == 1.h + 2*60.s + 3.s)
      intercept[DateTimeParseException] { parseDuration("1d") }
      assert(parseDuration("P1d") == 24 * 3600.s)
      assert(parseDuration("1") == 1.s)
      assert(parseDuration("111222333444555666.123456789") == Duration.ofSeconds(111222333444555666L, 123456789))
      assert(parseDuration("111222333444555666ms") == 111222333444555666L.ms)
      assert(parseDuration("111222333444555666µs") == Duration.ofSeconds(111222333444L, 555666000L))
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

    "StringAsDuration" in {
      val conv = implicitly[As[String, Duration]]
      intercept[DateTimeParseException] { conv("") }
      intercept[DateTimeParseException] { conv("1 s") }
      intercept[DateTimeParseException] { conv("3 minutes") }  // HOCON format is not supported
      assert(conv("1.234") == 1234.ms)  // This is in contrast to HOCON which assumes a bare number means milliseconds
      assert(conv("1s") == 1.s)
      assert(conv("3m") == 3*60.s)
      assert(conv("123.456789s") == 123456789.µs)
      assert(conv("PT1H1S") == 3601.s)
    }

    "StringAsDurationOption" in {
      val conv = implicitly[As[String, Option[Duration]]]
      intercept[DateTimeParseException] { conv("1 s") }
      intercept[DateTimeParseException] { conv("something") }
      assert(conv("") == None)
      assert(conv("never") == None)
      assert(conv("eternal") == None)
      assert(conv("1s") == Some(1.s))
      assert(conv("123.456789s") == Some(123456789.µs))
      assert(conv("PT1H1S") == Some(3601.s))
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

  "LocalDateTime" - {
    "compare" in {
      val a = LocalDateTime.of(2016, 1, 1, 12, 0, 0)
      val b = LocalDateTime.of(2016, 1, 1, 12, 0, 1)
      assert(a < b)
      assert(a <= b)
      assert(!(a >= b))
      assert(!(a > b))
      assert(a.compareTo(LocalDateTime.of(2016, 1, 1, 12, 0, 0)) == 0)
    }

    "toInstant" in {
      val timeZone = ZoneId of "Europe/Helsinki"
      assert(LocalDateTime.of(2016, 1, 1, 12, 0, 0).toInstant(timeZone) == Instant.parse("2016-01-01T10:00:00Z"))
      assert(LocalDateTime.of(2016, 7, 1, 12, 0, 0).toInstant(timeZone) == Instant.parse("2016-07-01T09:00:00Z"))
    }
  }

  "java.util.Date.show" in {
    assert(Timestamp("2018-11-21T12:34:56Z").toJavaUtilDate.show == "2018-11-21T12:34:56Z")
    assert(Timestamp("2018-11-21T12:34:56.987Z").toJavaUtilDate.show == "2018-11-21T12:34:56.987Z")
  }

  private def randomDuration(maximum: Duration): Duration =
    Duration.ofNanos((maximum.toNanos * Random.nextFloat()).toLong)
}
