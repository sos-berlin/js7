package js7.provider.scheduledorder.oldruntime

import java.time.{Duration, LocalTime}
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class RepeatPeriodTest extends AnyFreeSpec {

  "nextLocalTime" - {
    "Easy case" in {
      val period = RepeatPeriod(LocalTime.of(9, 0), ExtendedLocalTime.of(10, 0), Duration.ofMinutes(20))
      assert(period.nextLocalTime(LocalTime.of(0, 0)) == Some(LocalTime.of(9, 0)))
      assert(period.nextLocalTime(LocalTime.of(9, 0)) == Some(LocalTime.of(9, 20)))
      assert(period.nextLocalTime(LocalTime.of(9, 0, 0, 1)) == Some(LocalTime.of(9, 20)))
      assert(period.nextLocalTime(LocalTime.of(9, 19, 59, 999999999)) == Some(LocalTime.of(9, 20)))
      assert(period.nextLocalTime(LocalTime.of(9, 20)) == Some(LocalTime.of(9, 40)))
      assert(period.nextLocalTime(LocalTime.of(9, 40)) == None)
    }

    "An instant would be at midnight" in {
      val period = RepeatPeriod(LocalTime.of(23, 0), ExtendedLocalTime.of(24, 0), Duration.ofMinutes(20))
      assert(period.nextLocalTime(LocalTime.of(23, 0)) == Some(LocalTime.of(23, 20)))
      assert(period.nextLocalTime(LocalTime.of(23, 40)) == None)
    }

    "An instant would be after midnight" in {
      val period = RepeatPeriod(LocalTime.of(23, 0), ExtendedLocalTime.of(24, 0), Duration.ofHours(23))
      assert(period.nextLocalTime(LocalTime.of(23, 30)) == None)
    }

    "Empty period" in {
      val period = RepeatPeriod(LocalTime.of(1, 0), ExtendedLocalTime.of(1, 0), Duration.ofSeconds(1))
      assert(period.nextLocalTime(LocalTime.of(1, 0)) == None)
    }
  }
}
