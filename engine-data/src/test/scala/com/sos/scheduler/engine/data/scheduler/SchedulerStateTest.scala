package com.sos.scheduler.engine.data.scheduler

import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class SchedulerStateTest extends FreeSpec {

  "cppName, ofCppName and toString" in {
    for (v ← SchedulerState.values) {
      assert(v == SchedulerState.ofCppName(v.cppName))
      assert(v == SchedulerState.ofCppName(v.toString))
    }
  }
}
