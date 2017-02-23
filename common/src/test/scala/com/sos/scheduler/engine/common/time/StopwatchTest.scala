package com.sos.scheduler.engine.common.time

import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.common.time.Stopwatch._
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class StopwatchTest extends FreeSpec {

  "itemsPerSecondString" in {
      assert(itemsPerSecondString(2.s, 3000, "item") == "2s/3000 items, 666µs/item, 1500 items/s")
  }
}
