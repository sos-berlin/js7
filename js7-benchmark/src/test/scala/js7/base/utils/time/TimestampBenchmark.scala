package js7.base.utils.time

import js7.base.time.Timestamp
import js7.benchmark.OurBenchmark
import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Fork, Measurement, Mode, Warmup}

/** Benchmark for some Timestamp operations.
  * <p>
  *   Start with:
  * <p>
  *   `sbt js7-benchmark/Jmh/run js7.base.utils.time.TimestampBenchmark`
  */
@BenchmarkMode(Array(Mode.Throughput))
@Warmup(time = 1, iterations = 5)
@Measurement(time = 1, iterations = 5)
@Fork(1)
class TimestampBenchmark extends OurBenchmark:

  @Benchmark
  def TimestampNow: Timestamp =
    Timestamp.now
