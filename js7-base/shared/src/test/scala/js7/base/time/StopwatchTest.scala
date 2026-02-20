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

  "bytesPerSecondString" in:
    assert(bytesPerSecondString(10.s, 1) == "⏱️  10s/0.001kB")
    assert(bytesPerSecondString(10.s, 1_000) == "⏱️  10s/1kB")
    assert(bytesPerSecondString(10.s, 10_000) == "⏱️  10s/10kB, 1kB/s")
    assert(bytesPerSecondString(10.s, 99_000) == "⏱️  10s/99kB, 9.9kB/s")
    assert(bytesPerSecondString(10.s, 99_900) == "⏱️  10s/100kB, 10kB/s")
    assert(bytesPerSecondString(10.s, 99_000_000) == "⏱️  10s/99·MB, 9.9·MB/s")
    assert(bytesPerSecondString(10.s, 99_000_000_000L) == "⏱️  10s/99·GB, 9.9·GB/s")

  "durationAndPerSecondString" in:
    assert(durationAndPerSecondString(2.s, 3000) == "⏱️  2s/3000 ops, 1500 ops/s")

  "numberAndPerSecondString" in:
    assert(numberAndPerSecondString(2.s, 3000) == "3000 ops, 1500/s")
