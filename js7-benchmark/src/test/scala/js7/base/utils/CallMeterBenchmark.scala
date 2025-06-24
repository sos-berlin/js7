package js7.base.utils

import js7.base.metering.CallMeter
import js7.benchmark.OurBenchmark
import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Fork, Measurement, Mode, Warmup}

/** Benchmark for synchronised instruction.
  *
  * <p>
  *   start with:
  * <p>
  *   `sbt js7-benchmark/Jmh/run js7.base.utils.CallMeterBenchmark`
  */
@BenchmarkMode(Array(Mode.Throughput))
@Warmup(time = 1, iterations = 1)
@Measurement(time = 1, iterations = 5)
@Fork(1)
class CallMeterBenchmark extends OurBenchmark:

  private val meterCall = CallMeter()

  @Benchmark
  def callMeter(): Unit =
    meterCall {}
