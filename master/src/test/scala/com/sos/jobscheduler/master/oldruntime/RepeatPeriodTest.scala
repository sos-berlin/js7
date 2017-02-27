package com.sos.jobscheduler.master.oldruntime

import com.sos.jobscheduler.common.time.ScalaTime._
import java.time.LocalTime
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class RepeatPeriodTest extends FreeSpec {

  "nextLocalTime" - {
    "Easy case" in {
      val period = RepeatPeriod(LocalTime.of(9, 0), ExtendedLocalTime.of(10, 0), 20*60.s)
      assert(period.nextLocalTime(LocalTime.of(0, 0)) == Some(LocalTime.of(9, 0)))
      assert(period.nextLocalTime(LocalTime.of(9, 0)) == Some(LocalTime.of(9, 20)))
      assert(period.nextLocalTime(LocalTime.of(9, 0, 0, 1)) == Some(LocalTime.of(9, 20)))
      assert(period.nextLocalTime(LocalTime.of(9, 19, 59, 999999999)) == Some(LocalTime.of(9, 20)))
      assert(period.nextLocalTime(LocalTime.of(9, 20)) == Some(LocalTime.of(9, 40)))
      assert(period.nextLocalTime(LocalTime.of(9, 40)) == None)
    }

    "An instant would be at midnight" in {
      val period = RepeatPeriod(LocalTime.of(23, 0), ExtendedLocalTime.of(24, 0), 20*60.s)
      assert(period.nextLocalTime(LocalTime.of(23, 0)) == Some(LocalTime.of(23, 20)))
      assert(period.nextLocalTime(LocalTime.of(23, 40)) == None)
    }

    "An instant would be after midnight" in {
      val period = RepeatPeriod(LocalTime.of(23, 0), ExtendedLocalTime.of(24, 0), 23.h)
      assert(period.nextLocalTime(LocalTime.of(23, 30)) == None)
    }

    "Empty period" in {
      val period = RepeatPeriod(LocalTime.of(1, 0), ExtendedLocalTime.of(1, 0), 1.s)
      assert(period.nextLocalTime(LocalTime.of(1, 0)) == None)
    }
  }
}
