package com.sos.jobscheduler.common.time

import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.Stopwatch._
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class StopwatchTest extends FreeSpec {

  "itemsPerSecondString" in {
    assert(itemsPerSecondString(2.s, 3000, "items") == "2s/3000 items (0.667ms) 1500 items/s")
    assert(itemsPerSecondString(2.s, 3000) == "2s/3000 ops (0.667ms) 1500 ops/s")
  }
}
