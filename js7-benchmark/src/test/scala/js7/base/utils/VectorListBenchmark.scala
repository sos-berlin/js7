package js7.base.utils

import js7.base.utils.VectorListBenchmark.*
import js7.benchmark.OurBenchmark
import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Fork, Measurement, Mode, Param, Setup, Warmup}
import scala.compiletime.uninitialized
import scala.jdk.StreamConverters.*
import scala.util.Random
import scala.collection.mutable

/** Benchmark for building of some Scala collections.
  * <p>
  *   start with:
  * <p>
  *   `sbt js7-benchmark/Jmh/run js7.base.utils.VectorListBenchmark`
  *
  * <pre>
  * Benchmark                  (size)   Mode  Cnt        Score        Error  Units
  * arrayBufferToArray            100  thrpt    5  5325196,083 ±  73266,675  ops/s
  * arrayBufferToArray          10000  thrpt    5    56545,633 ±   1291,512  ops/s
  * arrayBufferToArray        1000000  thrpt    5      379,946 ±     52,686  ops/s
  * arrayBuilder                  100  thrpt    5  5168547,412 ±  54526,057  ops/s
  * arrayBuilder                10000  thrpt    5    22318,481 ±   1245,905  ops/s
  * arrayBuilder              1000000  thrpt    5      239,169 ±     20,790  ops/s
  * buildListBuilder              100  thrpt    5  5621496,547 ±  36492,801  ops/s
  * buildListBuilder            10000  thrpt    5    57758,688 ±    669,504  ops/s
  * buildListBuilder          1000000  thrpt    5      566,074 ±     66,442  ops/s
  * buildVector                   100  thrpt    5  1082202,077 ±   8613,602  ops/s
  * buildVector                 10000  thrpt    5     9803,058 ±    318,002  ops/s
  * buildVector               1000000  thrpt    5       98,747 ±      1,185  ops/s
  * <b>buildVectorBuilder            100  thrpt    5  7426936,892 ±  26908,960  ops/s</b>
  * <b>buildVectorBuilder          10000  thrpt    5    78622,191 ±    347,921  ops/s</b>
  * <b>buildVectorBuilder        1000000  thrpt    5      804,730 ±      2,257  ops/s</b>
  *
  * prependList                   100  thrpt    5  6842421,292 ±  27692,674  ops/s
  * prependList                 10000  thrpt    5    70444,085 ±    254,809  ops/s
  * prependList               1000000  thrpt    5      703,246 ±      8,007  ops/s
  * prependVector                 100  thrpt    5  1144862,536 ±  15025,385  ops/s
  * prependVector               10000  thrpt    5    10341,913 ±     43,165  ops/s
  * prependVector             1000000  thrpt    5      103,436 ±      0,500  ops/s
  *
  * sum1ArrayIteratorFilter       100  thrpt    5  3565635,938 ±  26168,071  ops/s
  * sum1ArrayIteratorFilter     10000  thrpt    5    41329,736 ±    387,533  ops/s
  * sum1ArrayIteratorFilter   1000000  thrpt    5      150,186 ±      5,118  ops/s
  * sum1List                      100  thrpt    5  1512419,180 ±   6258,076  ops/s
  * sum1List                    10000  thrpt    5    18928,414 ±   1750,026  ops/s
  * sum1List                  1000000  thrpt    5      207,260 ±      5,177  ops/s
  * sum1ListFilter                100  thrpt    5  1773517,922 ±  11458,264  ops/s
  * sum1ListFilter              10000  thrpt    5    18112,104 ±   1418,281  ops/s
  * sum1ListFilter            1000000  thrpt    5      122,379 ±      2,166  ops/s
  * <b>sum1ListView                  100  thrpt    5  5256234,915 ±  27212,217  ops/s</b>
  * <b>sum1ListView                10000  thrpt    5    30251,829 ±   1451,446  ops/s</b>
  * <b>sum1ListView              1000000  thrpt    5      301,460 ±     18,299  ops/s</b>
  * sum1ListWithFilter            100  thrpt    5  3927598,258 ±  11038,074  ops/s
  * sum1ListWithFilter          10000  thrpt    5    24159,166 ±    136,589  ops/s
  * sum1ListWithFilter        1000000  thrpt    5      128,048 ±     10,127  ops/s
  * sum1Vector                    100  thrpt    5  2087773,844 ±  46340,215  ops/s
  * sum1Vector                  10000  thrpt    5    30341,764 ±    288,460  ops/s
  * sum1Vector                1000000  thrpt    5      250,844 ±     15,639  ops/s
  * sum1VectorFilter              100  thrpt    5  2411410,603 ± 116263,833  ops/s
  * sum1VectorFilter            10000  thrpt    5    27941,461 ±    141,722  ops/s
  * sum1VectorFilter          1000000  thrpt    5      149,934 ±      2,410  ops/s
  * sum1VectorIteratorFilter      100  thrpt    5  1997268,885 ±  28598,273  ops/s
  * sum1VectorIteratorFilter    10000  thrpt    5    19965,396 ±    208,000  ops/s
  * sum1VectorIteratorFilter  1000000  thrpt    5      126,771 ±      1,648  ops/s
  * <b>sum1VectorView                100  thrpt    5  4299116,056 ±  20192,505  ops/s</b>
  * <b>sum1VectorView              10000  thrpt    5    33239,177 ±     87,405  ops/s</b>
  * <b>sum1VectorView            1000000  thrpt    5      307,658 ±     11,266  ops/s</b>
  * sum1VectorWithFilter          100  thrpt    5  1485418,989 ±  45839,667  ops/s
  * sum1VectorWithFilter        10000  thrpt    5    21628,003 ±    156,037  ops/s
  * sum1VectorWithFilter      1000000  thrpt    5      103,007 ±      0,791  ops/s
  *
  * sum3IteratorFilter            100  thrpt    5  1099380,466 ±  19880,359  ops/s
  * sum3IteratorFilter          10000  thrpt    5     7634,041 ±    250,972  ops/s
  * sum3IteratorFilter        1000000  thrpt    5       79,537 ±      1,916  ops/s
  * sum3List                      100  thrpt    5   698232,307 ±   9211,273  ops/s
  * sum3List                    10000  thrpt    5     8381,503 ±   2583,149  ops/s
  * sum3List                  1000000  thrpt    5       71,485 ±     21,307  ops/s
  * sum3ListFilter                100  thrpt    5   913962,618 ±   8170,788  ops/s
  * sum3ListFilter              10000  thrpt    5    11668,649 ±    499,136  ops/s
  * sum3ListFilter            1000000  thrpt    5       78,827 ±      5,337  ops/s
  * sum3ListView                  100  thrpt    5  1000384,741 ±  22902,973  ops/s
  * sum3ListView                10000  thrpt    5    11027,871 ±    351,042  ops/s
  * sum3ListView              1000000  thrpt    5      103,077 ±      3,174  ops/s
  * sum3ListWithFilter            100  thrpt    5  1385794,956 ±   7591,985  ops/s
  * sum3ListWithFilter          10000  thrpt    5    14266,325 ±     67,249  ops/s
  * sum3ListWithFilter        1000000  thrpt    5       92,366 ±      2,499  ops/s
  * sum3Vector                    100  thrpt    5   864410,361 ±   2028,232  ops/s
  * sum3Vector                  10000  thrpt    5    10371,082 ±     50,287  ops/s
  * sum3Vector                1000000  thrpt    5       94,067 ±      0,902  ops/s
  * <b>sum3VectorFilter              100  thrpt    5  1733304,251 ±   5994,204  ops/s</b>
  * <b>sum3VectorFilter            10000  thrpt    5    15215,585 ±    101,645  ops/s</b>
  * <b>sum3VectorFilter          1000000  thrpt    5       98,178 ±      6,071  ops/s</b>
  * sum3VectorView                100  thrpt    5  1074953,677 ±   3893,637  ops/s
  * sum3VectorView              10000  thrpt    5    11289,221 ±     57,437  ops/s
  * sum3VectorView            1000000  thrpt    5      107,519 ±      5,928  ops/s
  * sum3VectorWithFilter          100  thrpt    5  1362645,709 ±   9013,492  ops/s
  * sum3VectorWithFilter        10000  thrpt    5    15424,368 ±     80,546  ops/s
  * sum3VectorWithFilter      1000000  thrpt    5       90,360 ±      0,877  ops/s
  *
  * sumDirectList                 100  thrpt    5  1634593,743 ±  29256,483  ops/s
  * sumDirectList               10000  thrpt    5    18001,289 ±   1886,665  ops/s
  * sumDirectList             1000000  thrpt    5      121,536 ±     14,163  ops/s
  * <b>sumDirectVector               100  thrpt    5  2513256,686 ± 112581,803  ops/s</b>
  * <b>sumDirectVector             10000  thrpt    5    27286,453 ±    191,190  ops/s</b>
  * <b>sumDirectVector           1000000  thrpt    5      145,149 ±     18,963  ops/s</b>
  * sumJavaStreamList             100  thrpt    5  2213274,647 ±  90326,968  ops/s
  * sumJavaStreamList           10000  thrpt    5    24957,936 ±   4078,938  ops/s
  * sumJavaStreamList         1000000  thrpt    5      122,406 ±      3,223  ops/s
  * sumJavaStreamVector           100  thrpt    5  2540003,850 ±  11133,696  ops/s
  * sumJavaStreamVector         10000  thrpt    5    28541,676 ±   4833,432  ops/s
  * sumJavaStreamVector       1000000  thrpt    5      139,551 ±      1,984  ops/s
  * </pre>
  */
@BenchmarkMode(Array(Mode.Throughput))
@Warmup(time = 1, iterations = 5)
@Measurement(time = 1, iterations = 5)
@Fork(1)
class VectorListBenchmark extends OurBenchmark:

  @Param(Array("100", "10000", "1000000"))
  private var size: Int = uninitialized

  private var array: IArray[X] = uninitialized
  private var list: List[X] = uninitialized
  private var vector: Vector[X] = uninitialized

  @Setup
  def setup(): Unit =
    array = IArray.fill(size)(X(Random.nextInt()))
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
  def arrayBuilder: Array[X] =
    val builder = Array.newBuilder[X]
    for x <- array do builder.addOne(x)
    builder.result()

  @Benchmark
  def arrayBufferToArray: Array[X] =
    val buffer = mutable.ArrayBuffer.empty[X]
    for x <- array do buffer.addOne(x)
    buffer.toArray

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
