package com.sos.scheduler.engine.master.oldruntime

import com.sos.scheduler.engine.common.time.ScalaTime._
import java.time.LocalTime
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class PeriodTest extends FreeSpec {

  "nextLocalTime" - {
    "Sunshine case" in {
      val period = Period(begin = LocalTime.of(9, 0), end = ExtendedLocalTime.of(10, 0), absoluteRepeat = Some(20*60.s))
      assert(period.nextLocalTime(LocalTime.of(0, 0)) == Some(LocalTime.of(9, 0)))
      assert(period.nextLocalTime(LocalTime.of(9, 0)) == Some(LocalTime.of(9, 20)))
      assert(period.nextLocalTime(LocalTime.of(9, 0, 0, 1)) == Some(LocalTime.of(9, 20)))
      assert(period.nextLocalTime(LocalTime.of(9, 19, 59, 999999999)) == Some(LocalTime.of(9, 20)))
      assert(period.nextLocalTime(LocalTime.of(9, 20)) == Some(LocalTime.of(9, 40)))
      assert(period.nextLocalTime(LocalTime.of(9, 40)) == None)
    }

    "An instant would be at midnight" in {
      val period = Period(begin = LocalTime.of(23, 0), end = ExtendedLocalTime.of(24, 0), absoluteRepeat = Some(20*60.s))
      assert(period.nextLocalTime(LocalTime.of(23, 0)) == Some(LocalTime.of(23, 20)))
      assert(period.nextLocalTime(LocalTime.of(23, 40)) == None)
    }

    "An instant would be after midnight" in {
      val period = Period(begin = LocalTime.of(23, 0), end = ExtendedLocalTime.of(24, 0), absoluteRepeat = Some(23.h))
      assert(period.nextLocalTime(LocalTime.of(23, 30)) == None)
    }

    "Empty period" in {
      val period = Period(begin = LocalTime.of(1, 0), end = ExtendedLocalTime.of(1, 0), absoluteRepeat = Some(1.s))
      assert(period.nextLocalTime(LocalTime.of(1, 0)) == None)
    }
  }
}
