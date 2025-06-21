package js7.base.time

import cats.syntax.show.*
import java.time.{Duration, Instant, LocalDateTime, LocalTime, ZoneId}
import js7.base.test.OurTestSuite
import js7.base.time.JavaTime.extensions.*
import js7.base.time.JavaTime.{bigDecimalToDuration, given_Show_Date}
import js7.base.time.JavaTimestamp.specific.*
import js7.base.time.TimestampForTests.ts
import org.scalatest.matchers.should.Matchers.*
import scala.math.Ordering.Implicits.*

final class JavaTimeTest extends OurTestSuite:

  import JavaTimeTest.ImplicitDurationInt

  "Duration" - {
    "Int.µs" in:
      assert(7.µs == Duration.ofNanos(7000))
      assert(Int.MaxValue.µs == Duration.ofNanos(1000L * Int.MaxValue))

    "Int.ms" in:
      assert(7.ms == Duration.ofMillis(7))
      assert(Int.MaxValue.ms == Duration.ofMillis(Int.MaxValue))

    "Int.s" in:
      (7.s: Duration).getSeconds shouldEqual 7
      assert(Int.MaxValue.s == Duration.ofSeconds(Int.MaxValue))

    "BigDecimal.s and toBigDecimal" in:
      List(
        BigDecimal(0) -> Duration.ZERO,
        BigDecimal(1) -> Duration.ofSeconds(1),
        BigDecimal("0.001") -> Duration.ofMillis(1),
        BigDecimal("111222333444555666.123456789") -> Duration.ofSeconds(111222333444555666L, 123456789),
        BigDecimal("-111222333444555666.123456789") -> Duration.ofSeconds(111222333444555666L, 123456789).negated)
      .foreach { case (bigDecimal, duration) =>
        assert(bigDecimalToDuration(bigDecimal) == duration)
        assert(duration.toBigDecimal == bigDecimal)
      }
      intercept[ArithmeticException]:
        bigDecimalToDuration(BigDecimal("0.1112223334"))

    "Int.h" in:
      (7.h: Duration).toHours shouldEqual 7
      (7.h: Duration).toMillis shouldEqual (7*3600*1000)

//    "Int.days" in {
//      (7.days: Duration).toDays shouldEqual 7
//      (7.days: Duration).toMillis shouldEqual (7*24*3600*1000)
//    }
//    "Long.days" in {
//      (7L.days: Duration).toDays shouldEqual 7
//      (7L.days: Duration).toMillis shouldEqual (7*24*3600*1000)
//    }

    "-Duration" in:
      assert(-7.s == Duration.ofSeconds(-7))

    "Duration + Duration" in:
      (7.s + 2.ms: Duration).toMillis shouldEqual (7*1000 + 2)

    "Duration - Duration" in:
      (7.s - 2.ms: Duration).toMillis shouldEqual (7*1000 - 2)

    "Duration * Int" in:
      (7.s * 2).toMillis shouldEqual 14000

    "Duration / Int" in:
      (7.s / 2).toMillis shouldEqual 3500

    "Duration * BigDecimal" in:
      (3.s * BigDecimal("2.5")).toMillis shouldEqual 7500
      (3.s * 2.5).toMillis shouldEqual 7500

    "Duration / BigDecimal" in:
      (10.s / BigDecimal("2.5")).toMillis shouldEqual 4000
      (10.s / 2.5).toMillis shouldEqual 4000

    "Int * Duration" in:
      (7.s * 3: Duration).toMillis shouldEqual (3 * 7*1000)

    "Long * Duration" in:
      (7.s * 3L: Duration).toMillis shouldEqual (3 * 7*1000)

    "min" in:
      assert(1.s.min(2.s) == 1.s)
      assert(-1.s.min(-2.s) == -2.s)

    "max" in:
      assert(1.s.max(2.s) == 2.s)
      assert(-1.s.max(-2.s) == -1.s)

    "Duration < Duration" in:
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

  "Instant" - {
    "Instant + Duration" in:
      (Instant.ofEpochMilli(7) + 2.ms: Instant) shouldEqual Instant.ofEpochMilli(7 + 2)

    "Instant - Duration" in:
      (Instant.ofEpochMilli(7) - 2.ms: Instant) shouldEqual Instant.ofEpochMilli(7 - 2)

    "Instant - Instant" in:
      (Instant.ofEpochMilli(7) - Instant.ofEpochMilli(2): Duration) shouldEqual Duration.ofMillis(7 - 2)

    "Instant max Instant" in:
      Instant.ofEpochMilli(1).max(Instant.ofEpochMilli(2)) shouldEqual Instant.ofEpochMilli(2)

    "Instant min Instant" in:
      Instant.ofEpochMilli(1).min(Instant.ofEpochMilli(2)) shouldEqual Instant.ofEpochMilli(1)

    "roundTo" in:
      assert(Instant.parse("2015-01-01T12:01:01.499Z").roundTo(1.s) == Instant.parse("2015-01-01T12:01:01Z"))
      assert(Instant.parse("2015-01-01T12:01:01.500Z").roundTo(1.s) == Instant.parse("2015-01-01T12:01:02Z"))
      assert(Instant.parse("2015-01-01T12:01:29.999Z").roundTo(60.s) == Instant.parse("2015-01-01T12:01:00Z"))
      assert(Instant.parse("2015-01-01T12:01:30Z"    ).roundTo(60.s) == Instant.parse("2015-01-01T12:02:00Z"))

    "roundDownTo" in:
      assert(Instant.parse("2015-01-01T12:01:01.499Z").roundDownTo(1.s) == Instant.parse("2015-01-01T12:01:01Z"))
      assert(Instant.parse("2015-01-01T12:01:01.500Z").roundDownTo(1.s) == Instant.parse("2015-01-01T12:01:01Z"))
      assert(Instant.parse("2015-01-01T12:01:29.999Z").roundDownTo(60.s) == Instant.parse("2015-01-01T12:01:00Z"))
      assert(Instant.parse("2015-01-01T12:01:30Z"    ).roundDownTo(60.s) == Instant.parse("2015-01-01T12:01:00Z"))
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
    "LocalTime < LocalTime" in:
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

  "LocalDateTime" - {
    "compare" in:
      val a = LocalDateTime.of(2016, 1, 1, 12, 0, 0)
      val b = LocalDateTime.of(2016, 1, 1, 12, 0, 1)
      assert(a < b)
      assert(a <= b)
      assert(!(a >= b))
      assert(!(a > b))
      assert(a.compareTo(LocalDateTime.of(2016, 1, 1, 12, 0, 0)) == 0)

    "toInstant" in:
      val timeZone = ZoneId.of("Europe/Helsinki")
      assert(LocalDateTime.of(2016, 1, 1, 12, 0, 0).toInstant(timeZone) == Instant.parse("2016-01-01T10:00:00Z"))
      assert(LocalDateTime.of(2016, 7, 1, 12, 0, 0).toInstant(timeZone) == Instant.parse("2016-07-01T09:00:00Z"))
  }

  "java.util.Date.show" in:
    assert(ts"2018-11-21T12:34:56Z".toJavaUtilDate.show == "2018-11-21T12:34:56Z")
    assert(ts"2018-11-21T12:34:56.987Z".toJavaUtilDate.show == "2018-11-21T12:34:56.987Z")

  "Timestamp.fromJavaTime, toJavaUtilDate" in:
    val timestamp = ts"2018-11-21T12:34:56Z"
    val date = timestamp.toJavaUtilDate
    assert(Timestamp.fromJavaUtilDate(date) == timestamp)


object JavaTimeTest:

  implicit private final class ImplicitDurationInt(private val delegate: Int) extends AnyVal:
    /**
     * Duration, counted in microseconds.
     */
    def µs = Duration.ofNanos(1000L * delegate)

    /**
     * Duration, counted in milliseconds.
     */
    def ms = Duration.ofMillis(delegate)

    /**
     * Duration, counted in seconds.
     */
    def s = Duration.ofSeconds(delegate)

    /**
     * Duration, counted in hours.
     */
    def h = Duration.ofHours(delegate)
