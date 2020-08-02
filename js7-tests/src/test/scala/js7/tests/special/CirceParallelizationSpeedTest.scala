package js7.tests.special

import akka.util.ByteString
import io.circe._
import io.circe.syntax._
import js7.base.circeutils.CirceObjectCodec
import js7.base.circeutils.CirceUtils._
import js7.base.problem.Checked._
import js7.base.time.ScalaTime._
import js7.base.time.Stopwatch.measureTimeOfSingleRun
import js7.base.utils.ScalaUtils.syntax._
import js7.common.log.ScribeUtils.coupleScribeWithSlf4j
import js7.common.scalautil.MonixUtils.syntax._
import js7.tests.special.CirceParallelizationSpeedTest._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observable
import org.scalatest.freespec.AnyFreeSpec
import scala.collection.immutable.Seq
import scala.util.Random

final class CirceParallelizationSpeedTest extends AnyFreeSpec
{
  if (sys.props.contains("test.speed")) {
    coupleScribeWithSlf4j()

    val n = 1000000
    lazy val big = for (i <- 1 to n / 10) yield Big(i, Seq.fill(20)(Random.nextString(10)))
    lazy val small = for (i <- 1 to n) yield Small(i)
    lazy val bigJson = encodeParallelBuffered(big)
    lazy val smallJson = encodeParallelBuffered(small)

    "encode parallel buffered" in {
      testEncode(big, "Big")(encodeParallelBuffered)
      testEncode(small, "Small")(encodeParallelBuffered)
    }

    "decode parallel buffered" in {
      testDecode[Big](bigJson, "Big")(decodeParallelBuffered[Big])
      testDecode[Small](smallJson, "Small")(decodeParallelBuffered[Small])
    }

  if (false) { // too slow
    "encode parallel" in {
      testEncode(big, "Big")(encodeParallel)
      testEncode(small, "Small")(encodeParallel)
    }

    "decode parallel" in {
      testDecode[Big](bigJson, "Big")(decodeParallel[Big])
      testDecode[Small](smallJson, "Small")(decodeParallel[Small])
    }
  }

  if (false) { // slow
    "encode sequential" in {
      testEncode(big, "Big")(encodeSerial)
      testEncode(small, "Small")(encodeSerial)
      succeed
    }

    "decode sequential" in {
      testDecode[Big](bigJson, "Big")(decodeSerial[Big])
      testDecode[Small](smallJson, "Small")(decodeSerial[Small])
      succeed
    }
  }
  }

  private def testEncode[A: Encoder](seq: Seq[A], plural: String)(body: Seq[A] => Seq[ByteString]): Unit = {
    val m = 20
    for (i <- 1 to m) {
      //System.gc()
      val timing = measureTimeOfSingleRun(seq.size, plural) {
        body(seq)
      }
      if (i > m / 2) scribe.info(s"Encode $timing")
    }
  }

  private def testDecode[A: Decoder](seq: Seq[ByteString], plural: String)(body: Seq[ByteString] => Seq[A]): Unit = {
    val m = 20
    for (i <- 1 to m) {
      //System.gc()
      val timing = measureTimeOfSingleRun(seq.size, plural) {
        body(seq)
      }
      if (i > m / 2) scribe.info(s"Decode $timing")
    }
  }

  private def encodeParallelBuffered[A: Encoder](seq: Seq[A]): Seq[ByteString] =
    Observable.fromIterable(seq)
      .bufferTumbling(200)
      .mapParallelOrdered(parallelization)(seq => Task(seq.map(a => ByteString(a.asJson.compactPrint))))
      .flatMap(Observable.fromIterable)
      .toListL
      .await(99.s)

  private def decodeParallelBuffered[A: Decoder](seq: Seq[ByteString]): Seq[A] =
    Observable.fromIterable(seq)
      .bufferTumbling(200)
      .mapParallelOrdered(parallelization)(seq => Task(seq map decode[A]))
      .flatMap(Observable.fromIterable)
      .toListL
      .await(99.s)

  private def encodeParallel[A: Encoder](seq: Seq[A]): Seq[ByteString] =
    Observable.fromIterable(seq)
      .mapParallelOrdered(parallelization)(a => Task(ByteString(a.asJson.compactPrint)))
      .toListL
      .await(99.s)

  private def decodeParallel[A: Decoder](seq: Seq[ByteString]): Seq[A] =
    Observable.fromIterable(seq)
      .mapParallelOrdered(parallelization)(byteString => Task(decode(byteString)))
      .toListL
      .await(99.s)

  private def encodeSerial[A: Encoder](seq: Seq[A]): Seq[ByteString] =
    seq map encode[A]

  private def decodeSerial[A: Decoder](seq: Seq[ByteString]): Seq[A] =
    seq.map(_.utf8String.parseJsonChecked.orThrow.as[A].orThrow)


  private def encode[A: Encoder](a: A): ByteString =
    ByteString(a.asJson.compactPrint)
    //ByteString.fromArrayUnsafe(CompactPrinter.printToByteBuffer(a.asJson).array())

  private def decode[A: Decoder](byteString: ByteString): A =
    byteString.utf8String.parseJsonChecked.orThrow.as[A].orThrow
}

private object CirceParallelizationSpeedTest
{
  private val parallelization = sys.runtime.availableProcessors

  case class Small(int: Int, string: String = "STRING", boolean: Boolean = true)
  object Small {
    implicit val jsonCodec: CirceObjectCodec[Small] = deriveCodec[Small]
  }

  case class Big(int: Int, array: Seq[String])
  object Big {
    implicit val jsonCodec: CirceObjectCodec[Big] = deriveCodec[Big]
  }
}
