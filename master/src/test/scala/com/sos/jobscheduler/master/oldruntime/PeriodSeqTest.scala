package com.sos.jobscheduler.master.oldruntime

import com.sos.jobscheduler.common.time.ScalaTime._
import java.time.LocalTime
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class PeriodSeqTest extends FreeSpec {

  private val periodSeq = PeriodSeq(List(
    Period(begin = LocalTime.of(9, 0), end = ExtendedLocalTime.of(10, 0), absoluteRepeat = Some(20*60.s)),
    Period(begin = LocalTime.of(12, 0), end = ExtendedLocalTime.of(12, 31), absoluteRepeat = Some(30*60.s))))

  "nextLocalTime" in {
    assert(periodSeq.nextLocalTime(LocalTime.of(0, 0)) == Some(LocalTime.of(9, 0)))
    assert(periodSeq.nextLocalTime(LocalTime.of(9, 0)) == Some(LocalTime.of(9, 20)))
    assert(periodSeq.nextLocalTime(LocalTime.of(9, 0, 0, 1)) == Some(LocalTime.of(9, 20)))
    assert(periodSeq.nextLocalTime(LocalTime.of(9, 19, 59, 999999999)) == Some(LocalTime.of(9, 20)))
    assert(periodSeq.nextLocalTime(LocalTime.of(9, 20)) == Some(LocalTime.of(9, 40)))
    assert(periodSeq.nextLocalTime(LocalTime.of(9, 40)) == Some(LocalTime.of(12, 0)))
    assert(periodSeq.nextLocalTime(LocalTime.of(12, 0)) == Some(LocalTime.of(12, 30)))
    assert(periodSeq.nextLocalTime(LocalTime.of(12, 30)) == None)
  }
}
