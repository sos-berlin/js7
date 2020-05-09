package com.sos.jobscheduler.base.time

import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.time.Stopwatch._
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class StopwatchTest extends AnyFreeSpec {

  "itemsPerSecondString" in {
    assert(itemsPerSecondString(2.s, 3000, "items") == "2s/3000 items (⌀0.667ms) 1500 items/s")
    assert(itemsPerSecondString(2.s, 3000) == "2s/3000 ops (⌀0.667ms) 1500 ops/s")
  }
}
