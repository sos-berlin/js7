package js7.provider.scheduledorder.oldruntime

import java.time.{Duration, LocalTime}
import js7.base.test.OurTestSuite

/**
  * @author Joacim Zschimmer
  */
final class PeriodSeqTest extends OurTestSuite {

  private val periodSeq = PeriodSeq(List(
    RepeatPeriod(LocalTime.of( 9, 0), ExtendedLocalTime.of(10,  0), absoluteRepeat = Duration.ofMinutes(20)),
    SingleStartPeriod(LocalTime.of(11, 0)),
    RepeatPeriod(LocalTime.of(12, 0), ExtendedLocalTime.of(12, 31), absoluteRepeat = Duration.ofMinutes(30))))

  "nextLocalTime" in {
    assert(periodSeq.nextLocalTime(LocalTime.of(0, 0)) == Some(LocalTime.of(9, 0)))
    assert(periodSeq.nextLocalTime(LocalTime.of(9, 0)) == Some(LocalTime.of(9, 20)))
    assert(periodSeq.nextLocalTime(LocalTime.of(9, 0, 0, 1)) == Some(LocalTime.of(9, 20)))
    assert(periodSeq.nextLocalTime(LocalTime.of(9, 19, 59, 999999999)) == Some(LocalTime.of(9, 20)))
    assert(periodSeq.nextLocalTime(LocalTime.of(9, 20)) == Some(LocalTime.of(9, 40)))
    assert(periodSeq.nextLocalTime(LocalTime.of(9, 40)) == Some(LocalTime.of(11, 0)))
    assert(periodSeq.nextLocalTime(LocalTime.of(11, 11)) == Some(LocalTime.of(12,  0)))
    assert(periodSeq.nextLocalTime(LocalTime.of(12,  0)) == Some(LocalTime.of(12, 30)))
    assert(periodSeq.nextLocalTime(LocalTime.of(12, 30)) == None)
  }
}
