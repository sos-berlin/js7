package js7.base.time

import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.*

/**
  * @author Joacim Zschimmer
  */
final class StopwatchTest extends OurTestSuite:

  "itemsPerSecondString" in:
    assert(itemsPerSecondString(2.s, 3, "items") == "⏱️  2s/3 items (⌀0.67s), 1.5 items/s")
    assert(itemsPerSecondString(2.s, 3000, "items") == "⏱️  2s/3000 items (⌀0.67ms), 1500 items/s")
    assert(itemsPerSecondString(2.s, 30_000, "items") == "⏱️  2s/30'000 items (⌀67µs), 15'000 items/s")
    assert(itemsPerSecondString(2.s, 1_000_000, "items") == "⏱️  2s/million items (⌀2µs), 500'000 items/s")
    assert(itemsPerSecondString(2.s, 3_000_000, "items") == "⏱️  2s/3'000'000 items (⌀0.67µs), 1'500'000 items/s")
    assert(itemsPerSecondString(2400.ms, 500_000_000, "items") == "⏱️  2.4s/500'000'000 items (⌀4ns), 208'333'333 items/s")

  "bytesPerSecondString" in:
    assert(bytesPerSecondString(10.s, 1) == "⏱️  10s/0.001kB, 0.0kB/s")
    assert(bytesPerSecondString(10.s, 1_000) == "⏱️  10s/1kB, 0.1kB/s")
    assert(bytesPerSecondString(10.s, 10_000) == "⏱️  10s/10kB, 1kB/s")
    assert(bytesPerSecondString(10.s, 99_000) == "⏱️  10s/99kB, 9.9kB/s")
    assert(bytesPerSecondString(10.s, 99_900) == "⏱️  10s/100kB, 10kB/s")
    assert(bytesPerSecondString(10.s, 99_000_000) == "⏱️  10s/99 MB, 9.9 MB/s")
    assert(bytesPerSecondString(1800.ms, 2_000_000_000) == "⏱️  1.8s/2 GB, 1.1 GB/s")
    assert(bytesPerSecondString(10.s, 10_000_000_000L) == "⏱️  10s/10 GB, 1 GB/s")
    assert(bytesPerSecondString(10.s, 19_100_000_000L) == "⏱️  10s/19 GB, 1.9 GB/s")
    assert(bytesPerSecondString(10.s, 99_000_000_000L) == "⏱️  10s/99 GB, 9.9 GB/s")

    assert(bytesPerSecondString(500.ms, 1_000_000_000L) == "⏱️  0.5s/1 GB, ~2 GB/s")
    assert(bytesPerSecondString(700.ms, 1_000_000_000L) == "⏱️  0.7s/1 GB, ~1.4 GB/s")
    assert(bytesPerSecondString(700.ms, 400_000_000L) == "⏱️  0.7s/400 MB, ~571 MB/s")

  "durationAndPerSecondString" in:
    assert(durationAndPerSecondString(2.s, 3000) == "⏱️  2s/3000 ops, 1500 ops/s")

  "numberAndPerSecondString" in:
    assert(numberAndPerSecondString(2.s, 3000) == "3000 ops, 1500/s")
