package js7.base.utils

import js7.base.circeutils.CirceUtils.{CompactPrinter, JsonStringInterpolator, RichJson}
import js7.base.data.ByteSequence
import js7.benchmark.OurBenchmark
import js7.common.pekkoutils.ByteStrings.syntax.*
import org.apache.pekko.util.ByteString
import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Fork, Measurement, Mode, Warmup}

/** Benchmark for some Circe operations.
  * <p>
  *   Start with:
  * <p>
  *   `sbt js7-benchmark/Jmh/run js7.base.utils.CirceBenchmark`
  */
@BenchmarkMode(Array(Mode.Throughput))
@Warmup(time = 1, iterations = 5)
@Measurement(time = 1, iterations = 5)
@Fork(1)
class CirceBenchmark extends OurBenchmark:

  val json = json"""{"eventId":1768886019069001,"timestamp":1768886019060,"Key":"test@2026-01-20T05:13:32Z|🍋","TYPE":"OrderMoved","to":[1,"fork+🍋",0,"catch+1",0,"then",0]}"""

  @Benchmark
  def toByteArray(): Unit =
    json.toByteArray

  // Slow
  @Benchmark
  def toPekkoByteStringViaCompactString(): Unit =
    ByteSequence[ByteString].fromString(json.compactPrint)

  // Fast
  @Benchmark
  def toPekkoByteStringViaPrintToByteBuffer(): Unit =
    ByteSequence[ByteString].readByteBuffer:
      CompactPrinter.printToByteBuffer(json)


  // OPTIMISE If we only serialise non-parallelly, we can provide a reusable buffer.
  // Use Pekko's SizePredicator and Circe's unsafePrintToAppendable. Beware Unicode surrogates.
  // Maybe it wont be faster.
  // Something like this:
  //
  //trait SizePredicator:
  //  def predictedSize(): Int
  //  def recordSize(size: Int): Unit
  //
  //private final class ByteSeqAppendable[ByteSeq: ByteSequence](
  //  sizePredicator: SizePredicator)
  //extends Appendable:
  //  // Encode Unicode surrogates as one character, four bytes!
  //  def append(csq: CharSequence) = ???
  //  def append(csq: CharSequence, start: Int, end: Int) = ???
  //  def append(c: Char) = ???
  //  def result(): ByteSeq = ???
