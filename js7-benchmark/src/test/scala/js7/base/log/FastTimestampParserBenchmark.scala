package js7.base.log

import java.nio.charset.StandardCharsets.UTF_8
import java.time.ZoneId
import js7.base.fs2utils.Fs2ChunkByteSequence.implicitByteSequence
import js7.base.log.reader.FastTimestampParser
import js7.base.time.EpochNano
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
  * Benchmark               Mode  Cnt   Score   Error  Units
  * millionParseTimestamps  thrpt   5  87,686 ± 1,640  ops/s
  * </pre>
  */
@BenchmarkMode(Array(Mode.Throughput))
@Warmup(time = 1, iterations = 5)
@Measurement(time = 1, iterations = 5)
@Fork(1)
class FastTimestampParserBenchmark extends OurBenchmark:

  private val timestampParser = new FastTimestampParser()(using ZoneId.of("Europe/Mariehamn"))
  private val timestamp = ts"2026-02-12T12:00:00.123+02"
  private val timestampLength = timestamp.toString.length
  private val logLines =
    (1 to 1_000_000).map: // 100 lines/s
      i => fs2.Chunk.array((timestamp + i.ms * 10).toString.getBytes(UTF_8))
    .toArray

  @Benchmark
  def millionParseTimestamps(): Unit =
    assert(timestampParser.parse(logLines.head) != EpochNano.Nix)
    var i = 0
    val n = logLines.length
    while i < n do
      timestampParser.parse(logLines(i))
      i += 1
