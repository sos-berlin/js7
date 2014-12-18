package com.sos.scheduler.engine.common.time

import com.sos.scheduler.engine.common.time.ScalaJoda._
import org.joda.time.{DateTime, Duration, Instant, LocalTime}
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ScalaJodaTest extends FreeSpec {

  "Duration" - {
    "Int.ms" in {
      (7.ms: Duration).getMillis should equal (7)
    }
    "Long.ms" in {
      (7L.ms: Duration).getMillis should equal (7)
    }
    "Int.s" in {
      (7.s: Duration).getStandardSeconds should equal (7)
      (7.s: Duration).getMillis should equal (7*1000)
    }
    "Long.s" in {
      (7L.s: Duration).getStandardSeconds should equal (7)
      (7L.s: Duration).getMillis should equal (7*1000)
    }
    "Int.hours" in {
      (7.hours: Duration).getStandardHours should equal (7)
      (7.hours: Duration).getMillis should equal (7*3600*1000)
    }
    "Long.hours" in {
      (7L.hours: Duration).getStandardHours should equal (7)
      (7L.hours: Duration).getMillis should equal (7*3600*1000)
    }
    "Int.days" in {
      (7.days: Duration).getStandardDays should equal (7)
      (7.days: Duration).getMillis should equal (7*24*3600*1000)
    }
    "Long.days" in {
      (7L.days: Duration).getStandardDays should equal (7)
      (7L.days: Duration).getMillis should equal (7*24*3600*1000)
    }
    "Duration + Duration" in {
      (7.s + 2.ms: Duration).getMillis should equal (7*1000 + 2)
    }
    "Duration - Duration" in {
      (7.s - 2.ms: Duration).getMillis should equal (7*1000 - 2)
    }
    "Int * Duration" in {
      (3 * 7.s: Duration).getMillis should equal (3 * 7*1000)
    }
    "Long * Duration" in {
      (3L * 7.s: Duration).getMillis should equal (3 * 7*1000)
    }
    "pretty" in {
      0.s.pretty should equal ("0s")
      1.s.pretty should equal ("1s")
      1200.ms.pretty should equal ("1.2s")
      1230.ms.pretty should equal ("1.23s")
      1234.ms.pretty should equal ("1.234s")
      10.ms.pretty should equal ("0.01s")
      for (i <- 1 to 10000000) (-10).ms.pretty
      (-10).ms.pretty should equal ("-0.01s")
      (-1).s.pretty should equal ("-1s")
    }
//    "Duration * Int" in {
//      ((7.s * 3): Duration).getMillis should equal (7*1000 * 3)
//    }
//    it ("Duration < Duration" in {
//      (new Duration(7) < new Duration(2)) should equal (false)
//      (new Duration(7) <= new Duration(2)) should equal (false)
//      (new Duration(7) > new Duration(2)) should equal (true)
//      (new Duration(7) >= new Duration(2)) should equal (true)
//      (new Duration(2) < new Duration(7)) should equal (true)
//      (new Duration(2) <= new Duration(7)) should equal (true)
//      (new Duration(2) > new Duration(2)) should equal (false)
//      (new Duration(2) >= new Duration(2)) should equal (false)
//      (new Duration(7) < new Duration(7)) should equal (false)
//      (new Duration(7) <= new Duration(7)) should equal (true)
//      (new Duration(7) > new Duration(2)) should equal (false)
//      (new Duration(7) >= new Duration(2)) should equal (true)
//    }
  }
  "Instant" - {
    "Instant + Duration" in {
      (new Instant(7) + 2.ms: Instant) should equal (new Instant(7 + 2))
    }
    "Instant - Duration" in {
      (new Instant(7) - 2.ms: Instant) should equal (new Instant(7 - 2))
    }
    "Instant - Instant" in {
      (new Instant(7) - new Instant(2): Duration) should equal (new Duration(7 - 2))
    }
  }
  "DateTime" - {
    "DateTime + Duration" in {
      (new DateTime(7) + 2.ms: DateTime) should equal (new DateTime(7 + 2))
    }
    "DateTime - Duration" in {
      (new DateTime(7) - 2.ms: DateTime) should equal (new DateTime(7 - 2))
    }
    "DateTime - DateTime" in {
      (new DateTime(7) - new DateTime(2): Duration) should equal (new Duration(7 - 2))
    }
  }
  "LocalTime" - {
    "LocalTime < LocalTime" in {
      new LocalTime(7) < new LocalTime(2) should equal (false)
      new LocalTime(7) <= new LocalTime(2) should equal (false)
      new LocalTime(7) > new LocalTime(2) should equal (true)
      new LocalTime(7) >= new LocalTime(2) should equal (true)

      new LocalTime(2) < new LocalTime(7) should equal (true)
      new LocalTime(2) <= new LocalTime(7) should equal (true)
      new LocalTime(2) > new LocalTime(7) should equal (false)
      new LocalTime(2) >= new LocalTime(7) should equal (false)

      new LocalTime(7) < new LocalTime(7) should equal (false)
      new LocalTime(7) <= new LocalTime(7) should equal (true)
      new LocalTime(7) > new LocalTime(7) should equal (false)
      new LocalTime(7) >= new LocalTime(7) should equal (true)
    }
  }
}
