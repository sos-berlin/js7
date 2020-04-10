package com.sos.jobscheduler.provider.scheduledorder.oldruntime

import com.sos.jobscheduler.common.time.JavaTime._
import java.time.LocalTime
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class PeriodSeqTest extends AnyFreeSpec {

  private val periodSeq = PeriodSeq(List(
    RepeatPeriod(LocalTime.of( 9, 0), ExtendedLocalTime.of(10,  0), absoluteRepeat = 20*60.s),
    SingleStartPeriod(LocalTime.of(11, 0)),
    RepeatPeriod(LocalTime.of(12, 0), ExtendedLocalTime.of(12, 31), absoluteRepeat = 30*60.s)))

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
