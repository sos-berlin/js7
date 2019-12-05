package com.sos.jobscheduler.base.time

import com.sos.jobscheduler.base.convert.As
import com.sos.jobscheduler.base.time.ScalaTime._
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import scala.concurrent.duration._

final class ScalaTimeTest extends FreeSpec
{
  "Duration" - {
    "Int.µs" in {
      assert(7.µs == Duration(7000, NANOSECONDS))
    }

    "Long.µs" in {
      assert(7L.µs == Duration(7000, NANOSECONDS))
    }

    "Int.ms" in {
      assert(7.ms == Duration(7, MILLISECONDS))
    }

    "Long.ms" in {
      assert(7L.ms == Duration(7, MILLISECONDS))
    }

    "Int.s" in {
      (7.s: Duration).toSeconds shouldEqual 7
    }

    "Long.s" in {
      (7L.s: Duration).toMillis shouldEqual 7000
    }

    "BigDecimal.s and toBigDecimal" in {
      List(
        BigDecimal(0) -> Duration.Zero,
        BigDecimal(1) -> 1.second,
        BigDecimal("0.001") -> 1.milliseconds,
        BigDecimal("9223372036.854775807") -> 9223372036854775807L.nanoseconds,
        BigDecimal("-9223372036.854775807") -> (-9223372036854775807L).nanoseconds)
      .foreach { case (bigDecimal, duration) =>
        assert(bigDecimal.s == duration)
        assert(bigDecimalToDuration(bigDecimal) == duration)
        assert(duration.toBigDecimal == bigDecimal)
      }
      intercept[ArithmeticException] {
        bigDecimalToDuration(BigDecimal("0.1112223334"))
      }
      intercept[ArithmeticException] {
        bigDecimalToDuration(BigDecimal("9223372036.854775808"))  // Long.MaxValue + 1
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

    "randomDuration" in {
      val durations = 1 to 1000 map { _ => randomDuration(2.s) }
      val min = durations.min
      val max = durations.max
      assert(min >= 0.s && min <= 100.ms)
      assert(max >= 1900.ms && min <= 2.s)
    }

    "-Duration" in {
      assert(-7.s == Duration(-7, SECONDS))
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
      assert(Duration(1, NANOSECONDS).pretty == "1ns")
      assert(Duration(12, NANOSECONDS).pretty == "12ns")
      assert(Duration(123, NANOSECONDS).pretty == "0.123µs")
      assert(Duration(1234, NANOSECONDS).pretty == "1.23µs")
      assert(Duration(12345, NANOSECONDS).pretty == "12.3µs")
      assert(Duration(123456, NANOSECONDS).pretty == "0.123ms")
      assert(Duration(1234567, NANOSECONDS).pretty == "1.23ms")
      assert(10.ms.pretty == "10ms")
      assert((-10).ms.pretty == "-10ms")
      assert(Duration(12345678, NANOSECONDS).pretty == "12.3ms")
      assert(Duration(123456789, NANOSECONDS).pretty == "0.123s")
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
      assert((14*24*60*60).s.pretty == "14days")
      assert((-14*24*60*60).s.pretty == "-14days")
      assert((15*24*60*60).s.pretty == "2weeks")
      assert((-15*24*60*60).s.pretty == "-2weeks")
      //assert((365*24*60*60).s.pretty == "365days")
      //assert((-365*24*60*60).s.pretty == "-365days")
      //assert((366*24*60*60).s.pretty == "12~months")
      //assert((-366*24*60*60).s.pretty == "-12~months")
      //assert((3*365*24*60*60).s.pretty == "36~months")
      //assert((-3*365*24*60*60).s.pretty == "-36~months")
      //assert((3*366*24*60*60).s.pretty == "3~years")
      //assert((-3*366*24*60*60).s.pretty == "-3~years")
      //assert(Long.MaxValue.nanoseconds.pretty == "292~years")
      assert(Long.MaxValue.nanoseconds.pretty == "15250weeks")
    }

    "toDecimalString" in {
      assert(0.s.toDecimalString == "0")
      assert(1.s.toDecimalString == "1")
      assert(Duration(1, NANOSECONDS).toDecimalString == "0.000000001")
    }

    "parseDuration" in {
      intercept[IllegalArgumentException] { parseDuration(".1s") }
      intercept[IllegalArgumentException] { parseDuration(".1") }
      assert(parseDuration("123") == 123.s)
      assert(parseDuration("0.123") == 123.ms)
      assert(parseDuration("123s") == 123.s)
      assert(parseDuration("0.123s") == 123.ms)
      //assert(parseDuration("PT0.123S") == 123.ms)
      //assert(parseDuration("1m") == 60.s)
      //assert(parseDuration("1h2m3s") == 1.h + 2*60.s + 3.s)
      intercept[IllegalArgumentException] { parseDuration("1d") }
      //assert(parseDuration("P1d") == 24 * 3600.s)
      assert(parseDuration("1") == 1.s)
      assert(parseDuration("9223372036.854775807") == Duration(9223372036854775807L, NANOSECONDS))
      assert(parseDuration("9223372036854ms") == 9223372036854L.ms)
      assert(parseDuration("9223372036854.775µs") == Duration(9223372036854775L, NANOSECONDS))
      intercept[ArithmeticException] { parseDuration("1.2345µs") }
    }

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
      val conv = implicitly[As[String, FiniteDuration]]
      intercept[IllegalArgumentException] { conv("") }
      intercept[IllegalArgumentException] { conv("1 s") }
      intercept[IllegalArgumentException] { conv("3 minutes") }  // HOCON format is not supported
      assert(conv("1.234") == 1234.ms)  // This is in contrast to HOCON which assumes a bare number means milliseconds
      assert(conv("1s") == 1.s)
      assert(conv("123.456789s") == 123456789.µs)
      //assert(conv("3m") == 3*60.s)
      //assert(conv("PT1H1S") == 3601.s)
    }

    "StringAsDurationOption" in {
      val conv = implicitly[As[String, Option[FiniteDuration]]]
      intercept[IllegalArgumentException] { conv("1 s") }
      intercept[IllegalArgumentException] { conv("something") }
      assert(conv("") == None)
      assert(conv("never") == None)
      assert(conv("eternal") == None)
      assert(conv("1s") == Some(1.s))
      assert(conv("123.456789s") == Some(123456789.µs))
      //assert(conv("PT1H1S") == Some(3601.s))
    }
  }

  "FiniteDuration" - {
    "withMillis" in {
      assert(1.s.withMillis(0) == 1.s)
      assert(-1.s.withMillis(0) == -1.s)
      assert(123999999.µs.withMillis(456) == 123456.ms)
      assert(-123999999.µs.withMillis(456) == -123456.ms)
    }

    "roundUpToNext" in {
      assert(0.s.roundUpToNext(1.s) == 0.s)
      assert(1.ns.roundUpToNext(1.s) == 1.s)
      assert(1000.ns.roundUpToNext(1.µs) == 1.µs)
      assert(1001.ns.roundUpToNext(1.µs) == 2.µs)
      assert(1999.ns.roundUpToNext(1.µs) == 2.µs)
      assert(2000.ns.roundUpToNext(1.µs) == 2.µs)
      assert(1234.ns.roundUpToNext(10.ns) == 1240.ns)

      assert(-1.ns.roundUpToNext(1.µs) == -1.µs)
      assert(-999.ns.roundUpToNext(1.µs) == -1.µs)
      assert(-1000.ns.roundUpToNext(1.µs) == -1.µs)
      assert(-1999.ns.roundUpToNext(1.µs) == -2.µs)
      assert(-2000.ns.roundUpToNext(1.µs) == -2.µs)
      assert(-1234.ns.roundUpToNext(10.ns) == -1240.ns)
    }
  }

  "RichDeadline" - {
    "hasElapsed" in {
      assert((Deadline.now - 1.s).hasElapsed)
    }

    "elapsed" in {
      assert((Deadline.now - 2.s).elapsed > 1.s)
    }

    "elapsedOrZero" in {
      assert((Deadline.now + 2.s).elapsed < -1.s)
      assert((Deadline.now + 2.s).elapsedOrZero == 0.s)
      assert((Deadline.now - 2.s).elapsedOrZero > 1.s)
    }

    "timeLeftOrZero" in {
      assert((Deadline.now - 2.s).timeLeft < -1.s)
      assert((Deadline.now - 2.s).timeLeftOrZero == 0.s)
      assert((Deadline.now + 2.s).timeLeftOrZero > 1.s)
    }
  }

  "Timestamp" - {
    "Timestamp + Duration" in {
      (Timestamp.ofEpochMilli(7) + 2.ms: Timestamp) shouldEqual Timestamp.ofEpochMilli(7 + 2)
    }

    "Timestamp - Duration" in {
      (Timestamp.ofEpochMilli(7) - 2.ms: Timestamp) shouldEqual Timestamp.ofEpochMilli(7 - 2)
    }

    "Timestamp - Timestamp" in {
      (Timestamp.ofEpochMilli(7) - Timestamp.ofEpochMilli(2): Duration) shouldEqual Duration(7 - 2, MILLISECONDS)
    }

    "Timestamp max Timestamp" in {
      Timestamp.ofEpochMilli(1) max Timestamp.ofEpochMilli(2) shouldEqual Timestamp.ofEpochMilli(2)
    }

    "Timestamp min Timestamp" in {
      Timestamp.ofEpochMilli(1) min Timestamp.ofEpochMilli(2) shouldEqual Timestamp.ofEpochMilli(1)
    }

    "roundTo" in {
      assert(Timestamp.parse("2015-01-01T12:01:01.499Z").roundTo(1.s) == Timestamp.parse("2015-01-01T12:01:01Z"))
      assert(Timestamp.parse("2015-01-01T12:01:01.500Z").roundTo(1.s) == Timestamp.parse("2015-01-01T12:01:02Z"))
      assert(Timestamp.parse("2015-01-01T12:01:29.999Z").roundTo(60.s) == Timestamp.parse("2015-01-01T12:01:00Z"))
      assert(Timestamp.parse("2015-01-01T12:01:30Z"    ).roundTo(60.s) == Timestamp.parse("2015-01-01T12:02:00Z"))
    }

    "roundDownTo" in {
      assert(Timestamp.parse("2015-01-01T12:01:01.499Z").roundDownTo(1.s) == Timestamp.parse("2015-01-01T12:01:01Z"))
      assert(Timestamp.parse("2015-01-01T12:01:01.500Z").roundDownTo(1.s) == Timestamp.parse("2015-01-01T12:01:01Z"))
      assert(Timestamp.parse("2015-01-01T12:01:29.999Z").roundDownTo(60.s) == Timestamp.parse("2015-01-01T12:01:00Z"))
      assert(Timestamp.parse("2015-01-01T12:01:30Z"    ).roundDownTo(60.s) == Timestamp.parse("2015-01-01T12:01:00Z"))
    }
  }
}
