package js7.base.log

import java.time.ZoneId
import js7.base.log.AnsiEscapeCodes.{blue, bold, highlight}
import js7.base.log.reader.FastTimestampParser
import js7.base.time.ScalaTime.*
import js7.base.time.TimestampForTests.ts
import js7.benchmark.OurBenchmark
import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Fork, Measurement, Mode, Warmup}

/** Benchmark LogFileIndex.
  * <p>
  *   Start with:
  * <p>
  *   `sbt js7-benchmark/Jmh/run js7.base.log.AnsiEscapeCodesBenchmark`
  * <pre>
  * Benchmark                            Mode  Cnt        Score      Error  Units
  * removeHighlights                    thrpt    5  2313127,492 ± 9937,447  ops/s
  * </pre>
  *
*/
@BenchmarkMode(Array(Mode.Throughput))
@Warmup(time = 1, iterations = 5)
@Measurement(time = 1, iterations = 5)
@Fork(1)
class AnsiEscapeCodesBenchmark extends OurBenchmark:

  private val line = highlight(blue):
    s"2026-02-12T12:00:00.000000 [info] thread class - ${bold("MESSAGE")}"

  private val timestampParser = new FastTimestampParser()(using ZoneId.of("Europe/Mariehamn"))
  private val timestamp = ts"2026-02-12T12:00:00Z"
  private val timestampLength = timestamp.toString.stripSuffix("Z").length
  private val logLines =
    (1 to 1_000_000).map: // 100 lines/s
      i => (timestamp + i.ms * 10).toString.stripSuffix("Z")

  @Benchmark
  def removeHighlights: String =
    AnsiEscapeCodes.removeHighlights(line)

  @Benchmark
  def millionTimestampStringsToEpochNano(): Unit =
    logLines.foreach:
      timestampParser.parse(_, 0, timestampLength)
