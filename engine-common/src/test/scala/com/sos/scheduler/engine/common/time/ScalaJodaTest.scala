package com.sos.scheduler.engine.common.time

import com.sos.scheduler.engine.common.time.ScalaJoda._
import org.joda.time.{DateTime, Duration, Instant, LocalTime}
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
final class ScalaJodaTest extends FreeSpec {

  "Duration" - {
    "Int.ms" in {
      (7.ms: Duration).getMillis shouldEqual 7
    }
    "Long.ms" in {
      (7L.ms: Duration).getMillis shouldEqual 7
    }
    "Int.s" in {
      (7.s: Duration).getStandardSeconds shouldEqual 7
      (7.s: Duration).getMillis shouldEqual (7*1000)
    }
    "Long.s" in {
      (7L.s: Duration).getStandardSeconds shouldEqual 7
      (7L.s: Duration).getMillis shouldEqual (7*1000)
    }
    "Int.hours" in {
      (7.hours: Duration).getStandardHours shouldEqual 7
      (7.hours: Duration).getMillis shouldEqual (7*3600*1000)
    }
    "Long.hours" in {
      (7L.hours: Duration).getStandardHours shouldEqual 7
      (7L.hours: Duration).getMillis shouldEqual (7*3600*1000)
    }
    "Int.days" in {
      (7.days: Duration).getStandardDays shouldEqual 7
      (7.days: Duration).getMillis shouldEqual (7*24*3600*1000)
    }
    "Long.days" in {
      (7L.days: Duration).getStandardDays shouldEqual 7
      (7L.days: Duration).getMillis shouldEqual (7*24*3600*1000)
    }
    "Duration + Duration" in {
      (7.s + 2.ms: Duration).getMillis shouldEqual (7*1000 + 2)
    }
    "Duration - Duration" in {
      (7.s - 2.ms: Duration).getMillis shouldEqual (7*1000 - 2)
    }
    "Int * Duration" in {
      (3 * 7.s: Duration).getMillis shouldEqual (3 * 7*1000)
    }
    "Long * Duration" in {
      (3L * 7.s: Duration).getMillis shouldEqual (3 * 7*1000)
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
//    "Duration * Int" in {
//      ((7.s * 3): Duration).getMillis shouldEqual (7*1000 * 3)
//    }
//    it ("Duration < Duration" in {
//      (new Duration(7) < new Duration(2)) shouldEqual (false)
//      (new Duration(7) <= new Duration(2)) shouldEqual (false)
//      (new Duration(7) > new Duration(2)) shouldEqual (true)
//      (new Duration(7) >= new Duration(2)) shouldEqual (true)
//      (new Duration(2) < new Duration(7)) shouldEqual (true)
//      (new Duration(2) <= new Duration(7)) shouldEqual (true)
//      (new Duration(2) > new Duration(2)) shouldEqual (false)
//      (new Duration(2) >= new Duration(2)) shouldEqual (false)
//      (new Duration(7) < new Duration(7)) shouldEqual (false)
//      (new Duration(7) <= new Duration(7)) shouldEqual (true)
//      (new Duration(7) > new Duration(2)) shouldEqual (false)
//      (new Duration(7) >= new Duration(2)) shouldEqual (true)
//    }
  }
  "Instant" - {
    "Instant + Duration" in {
      (new Instant(7) + 2.ms: Instant) shouldEqual new Instant(7 + 2)
    }
    "Instant - Duration" in {
      (new Instant(7) - 2.ms: Instant) shouldEqual new Instant(7 - 2)
    }
    "Instant - Instant" in {
      (new Instant(7) - new Instant(2): Duration) shouldEqual new Duration(7 - 2)
    }
  }
  "DateTime" - {
    "DateTime + Duration" in {
      (new DateTime(7) + 2.ms: DateTime) shouldEqual new DateTime(7 + 2)
    }
    "DateTime - Duration" in {
      (new DateTime(7) - 2.ms: DateTime) shouldEqual new DateTime(7 - 2)
    }
    "DateTime - DateTime" in {
      (new DateTime(7) - new DateTime(2): Duration) shouldEqual new Duration(7 - 2)
    }
  }
  "LocalTime" - {
    "LocalTime < LocalTime" in {
      new LocalTime(7) < new LocalTime(2) shouldEqual false
      new LocalTime(7) <= new LocalTime(2) shouldEqual false
      new LocalTime(7) > new LocalTime(2) shouldEqual true
      new LocalTime(7) >= new LocalTime(2) shouldEqual true

      new LocalTime(2) < new LocalTime(7) shouldEqual true
      new LocalTime(2) <= new LocalTime(7) shouldEqual true
      new LocalTime(2) > new LocalTime(7) shouldEqual false
      new LocalTime(2) >= new LocalTime(7) shouldEqual false

      new LocalTime(7) < new LocalTime(7) shouldEqual false
      new LocalTime(7) <= new LocalTime(7) shouldEqual true
      new LocalTime(7) > new LocalTime(7) shouldEqual false
      new LocalTime(7) >= new LocalTime(7) shouldEqual true
    }
  }
}
