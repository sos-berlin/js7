package js7.base.time

import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.*

/**
  * @author Joacim Zschimmer
  */
final class StopwatchTest extends OurTestSuite:

  "itemsPerSecondString" in:
    assert(itemsPerSecondString(2.s, 3, "items") == "⏱️  2s/3 items (⌀0.67s)")
    assert(itemsPerSecondString(2.s, 3000, "items") == "⏱️  2s/3000 items (⌀0.67ms), 1500 items/s")
    assert(itemsPerSecondString(2.s, 30_000, "items") == "⏱️  2s/30k items (⌀67µs), 15k items/s")
    assert(itemsPerSecondString(2.s, 1_000_000, "items") == "⏱️  2s/million items (⌀2µs), 500k items/s")
    assert(itemsPerSecondString(2.s, 3_000_000, "items") == "⏱️  2s/3m items (⌀0.67µs), 1500k items/s")

  "durationAndPerSecondString" in:
    assert(durationAndPerSecondString(2.s, 3000) == "2s/3000 ops, 1500 ops/s")

  "numberAndPerSecondString" in:
    assert(numberAndPerSecondString(2.s, 3000) == "3000 ops, 1500/s")
