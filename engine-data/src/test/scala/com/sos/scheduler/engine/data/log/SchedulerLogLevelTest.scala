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
}
