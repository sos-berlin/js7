package com.sos.jobscheduler.master.oldruntime

import java.time.LocalTime
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class SingleStartPeriodTest extends FreeSpec {

  "nextLocalTime" - {
    "00:00" in {
      val period = SingleStartPeriod(LocalTime.of(0, 0))
      assert(period.nextLocalTime(LocalTime.of(0, 0)) == Some(LocalTime.of(0, 0)))
      assert(period.nextLocalTime(LocalTime.of(9, 0)) == None)
    }
    "09:00" in {
      val period = SingleStartPeriod(LocalTime.of(9, 0))
      assert(period.nextLocalTime(LocalTime.of(0, 0)) == Some(LocalTime.of(9, 0)))
      assert(period.nextLocalTime(LocalTime.of(9, 0)) == Some(LocalTime.of(9, 0)))
      assert(period.nextLocalTime(LocalTime.of(9, 0, 0, 1)) == None)
    }
  }
}
