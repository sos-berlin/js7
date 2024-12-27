package js7.base.utils

import js7.base.utils.VectorListBenchmark.*
import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Fork, Measurement, Mode, Param, Scope, Setup, State, Warmup}
import scala.compiletime.uninitialized
import scala.jdk.StreamConverters.*
import scala.util.Random

@BenchmarkMode(Array(Mode.Throughput))
@State(Scope.Benchmark)
@Measurement(time = 1, iterations = 5)
@Warmup(time = 1, iterations = 5)
@Fork(value = 2)
class VectorListBenchmark:

  @Param(Array("100", "10000", "1000000"))
  private var size: Int = uninitialized

  private var array: IArray[X] = uninitialized
  private var list: List[X] = uninitialized
  private var vector: Vector[X] = uninitialized

  @Setup
  def setup(): Unit =
    array = IArray.fill(size)(X(Random.nextInt))
    list = array.toList
    vector = array.toVector

  @Benchmark
  def buildListBuilder: List[X] =
    val builder = List.newBuilder[X]
    for x <- array do builder.addOne(x)
    builder.result()

  @Benchmark
  def buildVector: Vector[X] =
    var vector = Vector.empty[X]
    for x <- array do vector = vector :+ x
    vector

  @Benchmark
  def buildVectorBuilder: Vector[X] =
    val builder = Vector.newBuilder[X]
    for x <- array do builder.addOne(x)
    builder.result()

  @Benchmark
  def prependList: List[X] =
    var list = List.empty[X]
    for x <- array do list = x :: list
    list

  @Benchmark
  def prependVector: Vector[X] =
    var vector = Vector.empty[X]
    for a <- array do vector = a +: vector
    vector

  @Benchmark
  def sumDirectList: Int =
    list.filter(_.int >= 0).map(_.int).sum

  @Benchmark
  def sumDirectVector: Int =
    vector.filter(_.int >= 0).map(_.int).sum

  @Benchmark
  def sumJavaStreamList: Int =
    list.asJavaSeqStream.filter(_.int >= 0).mapToInt(_.int).sum

  @Benchmark
  def sumJavaStreamVector: Int =
    vector.asJavaSeqStream.filter(_.int >= 0).mapToInt(_.int).sum

  @Benchmark
  def sum1List: Int =
    list.map(_.int).sum

  @Benchmark
  def sum1ListView: Int =
    list.view.map(_.int).sum

  @Benchmark
  def sum1Vector: Int =
    vector.map(_.int).sum

  @Benchmark
  def sum1VectorView: Int =
    vector.view.map(_.int).sum

  @Benchmark
  def sum3List: Int =
    list.map(_.int).map(_ * 2).map(_ + 1).sum

  @Benchmark
  def sum3ListView: Int =
    list.view.map(_.int).map(_ * 2).map(_ + 1).sum

  @Benchmark
  def sum3Vector: Int =
    vector.map(_.int).map(_ * 2).map(_ + 1).sum

  @Benchmark
  def sum3VectorView: Int =
    vector.view.map(_.int).map(_ * 2).map(_ + 1).sum

  @Benchmark
  def sum1ArrayIteratorFilter: Int =
    array.iterator.filter(_.int >= 0).map(_.int).sum

  @Benchmark
  def sum1ListFilter: Int =
    list.filter(_.int >= 0).map(_.int).sum

  @Benchmark
  def sum1ListWithFilter: Int =
    list.withFilter(_.int >= 0).map(_.int).sum

  @Benchmark
  def sum1VectorIteratorFilter: Int =
    vector.iterator.filter(_.int >= 0).map(_.int).sum

  @Benchmark
  def sum1VectorFilter: Int =
    vector.filter(_.int >= 0).map(_.int).sum

  @Benchmark
  def sum1VectorWithFilter: Int =
    vector.withFilter(_.int >= 0).map(_.int).sum

  @Benchmark
  def sum3IteratorFilter: Int =
    vector.iterator.filter(_.int >= 0).map(_.int).map(_ * 2).map(_ + 1).sum

  @Benchmark
  def sum3ListFilter: Int =
    list.filter(_.int >= 0).map(_.int).map(_ * 2).map(_ + 1).sum

  @Benchmark
  def sum3ListWithFilter: Int =
    list.withFilter(_.int >= 0).map(_.int).map(_ * 2).map(_ + 1).sum

  @Benchmark
  def sum3VectorFilter: Int =
    vector.filter(_.int >= 0).map(_.int).map(_ * 2).map(_ + 1).sum

  @Benchmark
  def sum3VectorWithFilter: Int =
    vector.withFilter(_.int >= 0).map(_.int).map(_ * 2).map(_ + 1).sum


object VectorListBenchmark:
  final case class X(int: Int)
