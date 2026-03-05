package js7.base.log

import java.time.ZoneId
import js7.base.log.reader.FastTimestampParser
import js7.base.time.ScalaTime.*
import js7.base.time.TimestampForTests.ts
import js7.benchmark.OurBenchmark
import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Fork, Measurement, Mode, Warmup}

/** Benchmark LogFileIndex.
  * <p>
  *   Start with:
  * <p>
  *   `sbt js7-benchmark/Jmh/run js7.base.log.FastTimestampParserBenchmark`
  * <pre>
  * Benchmark               Mode  Cnt        Score      Error  Units
  * millionParseTimestamps  thrpt    5      110,500 ±    1,092  ops/s
  * </pre>
  */
@BenchmarkMode(Array(Mode.Throughput))
@Warmup(time = 1, iterations = 5)
@Measurement(time = 1, iterations = 5)
@Fork(1)
class FastTimestampParserBenchmark extends OurBenchmark:

  private val toNanos = new FastTimestampParser()(using ZoneId.of("Europe/Mariehamn"))
  private val timestamp = ts"2026-02-12T12:00:00Z"
  private val timestampLength = timestamp.toString.stripSuffix("Z").length
  private val logLines =
    (1 to 1_000_000).map: // 100 lines/s
      i => (timestamp + i.ms * 10).toString.stripSuffix("Z")

  @Benchmark
  def millionParseTimestamps(): Unit =
    logLines.foreach:
      toNanos(_, 0, timestampLength)
