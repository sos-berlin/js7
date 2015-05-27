package com.sos.scheduler.engine.common.time

import com.sos.scheduler.engine.common.time.ScalaTime._
import java.time.temporal.ChronoUnit._
import java.time.{Duration, Instant, LocalTime}
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner
import scala.concurrent.duration._

@RunWith(classOf[JUnitRunner])
final class ScalaTimeTest extends FreeSpec {

  "Duration" - {
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

    "Duration + Duration" in {
      (7.s + 2.ms: Duration).toMillis shouldEqual (7*1000 + 2)
    }

    "Duration - Duration" in {
      (7.s - 2.ms: Duration).toMillis shouldEqual (7*1000 - 2)
    }

    "Int * Duration" in {
      (3 * 7.s: Duration).toMillis shouldEqual (3 * 7*1000)
    }

    "Long * Duration" in {
      (3L * 7.s: Duration).toMillis shouldEqual (3 * 7*1000)
    }

    "pretty" in {
      0.s.pretty shouldEqual "0s"
      1.s.pretty shouldEqual "1s"
      1200.ms.pretty shouldEqual "1.2s"
      1230.ms.pretty shouldEqual "1.23s"
      1234.ms.pretty shouldEqual "1.234s"
      10.ms.pretty shouldEqual "0.01s"
      for (i <- 1 to 10000000) (-10).ms.pretty
      (-10).ms.pretty shouldEqual "-0.01s"
      (-1).s.pretty shouldEqual "-1s"
    }

    "toConcurrentDuration" in {
      assert(1234.ms.toConcurrentDuration == 1234.millis)
      assert(Duration.of(111222333444555666L, NANOS).toConcurrentDuration == 111222333444555666L.nanos)
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
