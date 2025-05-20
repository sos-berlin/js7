package js7.base.utils

import js7.benchmark.OurBenchmark
import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Fork, Measurement, Mode, Param, Setup, Warmup}
import scala.collection.mutable
import scala.compiletime.uninitialized
import scala.util.Random

/** Benchmark for Scala's PriorityQueue.
  * <p>
  *   start with:
  * <p>
  *   `sbt js7-benchmark/Jmh/run js7.base.utilsPriorityQueueBenchmark`
  */
@BenchmarkMode(Array(Mode.Throughput))
@Warmup(time = 1, iterations = 5)
@Measurement(time = 1, iterations = 5)
@Fork(1)
class PriorityQueueBenchmark extends OurBenchmark:

  @Param(Array("100000", "500000", "10000000"))
  private var size: Int = uninitialized

  private var unordered: Array[Int] = uninitialized

  @Setup
  def setup(): Unit =
    unordered = Random.shuffle(1 to size).to(Array)

  @Benchmark
  def priorizedQueueFromUnordered(): Unit =
    val priorityQueue = mutable.PriorityQueue.from(unordered)
    if priorityQueue.dequeue() != size then throw new AssertionError

  /** Adding values in reverse priority order is half as fast as adding in proper order. */
  @Benchmark
  def priorizedQueueFromReverseOrdered(): Unit =
    val priorityQueue = mutable.PriorityQueue.from(1 to size)
    if priorityQueue.dequeue() != size then throw new AssertionError

  /** Adding values in proper priority order. */
  @Benchmark
  def priorizedQueueFromProperlyOrdered(): Unit =
    val priorityQueue = mutable.PriorityQueue.from((1 to size).reverse)
    if priorityQueue.dequeue() != size then throw new AssertionError
