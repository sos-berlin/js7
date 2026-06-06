package js7.base.log

import js7.base.log.AnsiEscapeCodes.{blue, bold, highlight}
import js7.benchmark.OurBenchmark
import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Fork, Measurement, Mode, Warmup}

/** Benchmark LogFileIndex.
  * <p>
  *   Start with:
  * <p>
  *   `sbt js7-benchmark/Jmh/run js7.base.log.AnsiEscapeCodesBenchmark`
  * <pre>
  * Benchmark                            Mode  Cnt        Score       Error  Units
  * removeHighlights                    thrpt    5  3093198,735 ± 63898,008  ops/s
  * </pre>
  *
*/
@BenchmarkMode(Array(Mode.Throughput))
@Warmup(time = 1, iterations = 5)
@Measurement(time = 1, iterations = 5)
@Fork(1)
class AnsiEscapeCodesBenchmark extends OurBenchmark:

  private val line = highlight(blue):
    s"2026-02-12T12:00:00.000000+02 [info] thread class - ${bold("MESSAGE")}"

  @Benchmark
  def removeHighlights: String =
    AnsiEscapeCodes.removeHighlights(line)

