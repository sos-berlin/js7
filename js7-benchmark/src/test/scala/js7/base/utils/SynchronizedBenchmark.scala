package js7.base.utils

import js7.base.utils.Atomic.extensions.*
import js7.benchmark.OurBenchmark
import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Fork, Measurement, Mode, Warmup}

/** Benchmark for synchronised instruction.
  *
  * <p>
  *   start with:
  * <p>
  *   `sbt js7-benchmark/Jmh/run js7.base.utils.SynchronizedBenchmark`
  */
@BenchmarkMode(Array(Mode.Throughput))
@Warmup(time = 1, iterations = 1)
@Measurement(time = 1, iterations = 10)
@Fork(1)
class SynchronizedBenchmark extends OurBenchmark:

  private var a1 = 0L
  private var a2 = 0L
  private val atomic1 = Atomic(0L)
  private val atomic2 = Atomic(0L)

  @Benchmark
  def increment(): Unit =
    a1 += 1
    a2 += 1

  @Benchmark
  def incrementAtomic(): Unit =
    atomic1 += 1
    atomic2 += 1

  @Benchmark
  def incrementSynchronized(): Unit =
    synchronized:
      a1 += 1
      a2 += 1
