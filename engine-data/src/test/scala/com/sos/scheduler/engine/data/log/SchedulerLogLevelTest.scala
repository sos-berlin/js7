package com.sos.scheduler.engine.data.log

import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class SchedulerLogLevelTest extends FreeSpec {

  "contains" in {
    assert(SchedulerLogLevel.info contains SchedulerLogLevel.info)
    assert(SchedulerLogLevel.info contains SchedulerLogLevel.warning)
    assert(!(SchedulerLogLevel.info contains SchedulerLogLevel.debug1))
  }

  "ofCpp" in {
    assert(SchedulerLogLevel.ofCpp(-10) == SchedulerLogLevel.none)
    assert(SchedulerLogLevel.ofCpp(-9) == SchedulerLogLevel.debug9)
    assert(SchedulerLogLevel.ofCpp(-8) == SchedulerLogLevel.debug8)
    assert(SchedulerLogLevel.ofCpp(-7) == SchedulerLogLevel.debug7)
    assert(SchedulerLogLevel.ofCpp(-6) == SchedulerLogLevel.debug6)
    assert(SchedulerLogLevel.ofCpp(-5) == SchedulerLogLevel.debug5)
    assert(SchedulerLogLevel.ofCpp(-4) == SchedulerLogLevel.debug4)
    assert(SchedulerLogLevel.ofCpp(-3) == SchedulerLogLevel.debug3)
    assert(SchedulerLogLevel.ofCpp(-2) == SchedulerLogLevel.debug2)
    assert(SchedulerLogLevel.ofCpp(-1) == SchedulerLogLevel.debug1)
    assert(SchedulerLogLevel.ofCpp(0) == SchedulerLogLevel.info)
    assert(SchedulerLogLevel.ofCpp(1) == SchedulerLogLevel.warning)
    assert(SchedulerLogLevel.ofCpp(2) == SchedulerLogLevel.error)
  }
}
