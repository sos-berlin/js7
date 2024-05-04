package js7.provider.scheduledorder.oldruntime

import java.time.LocalTime
import js7.base.test.OurTestSuite

/**
  * @author Joacim Zschimmer
  */
final class SingleStartPeriodTest extends OurTestSuite:

  "nextLocalTime" - {
    "00:00" in:
      val period = SingleStartPeriod(LocalTime.of(0, 0))
      assert(period.nextLocalTime(LocalTime.of(0, 0)) == Some(LocalTime.of(0, 0)))
      assert(period.nextLocalTime(LocalTime.of(9, 0)) == None)
    "09:00" in:
      val period = SingleStartPeriod(LocalTime.of(9, 0))
      assert(period.nextLocalTime(LocalTime.of(0, 0)) == Some(LocalTime.of(9, 0)))
      assert(period.nextLocalTime(LocalTime.of(9, 0)) == Some(LocalTime.of(9, 0)))
      assert(period.nextLocalTime(LocalTime.of(9, 0, 0, 1)) == None)
  }