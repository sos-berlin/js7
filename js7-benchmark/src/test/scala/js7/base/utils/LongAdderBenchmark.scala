package js7.base.utils

import java.util.concurrent.atomic.{AtomicLong, LongAdder}
import js7.benchmark.OurBenchmark
import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Fork, Measurement, Mode, Threads, Warmup}

/** Benchmark for AtomicLong and LongAdder.
  *
  * Use LongAdder only for high and often concurrency.
  *
  * <p>
  *   start with:
  * <p>
  *   `sbt js7-benchmark/Jmh/run js7.base.utils.LongAdderBenchmark`
  */
@BenchmarkMode(Array(Mode.Throughput))
@Warmup(time = 1, iterations = 1)
@Measurement(time = 1, iterations = 5)
@Fork(1)
@Threads(1)
class LongAdderBenchmark extends OurBenchmark:

  private val atomic_ = AtomicLong(0)
  private val longAdder_ = LongAdder()

  @Benchmark
  def atomic(): Unit =
    atomic_.getAndIncrement()

  @Benchmark
  def longAdder(): Unit =
    longAdder_.increment()
