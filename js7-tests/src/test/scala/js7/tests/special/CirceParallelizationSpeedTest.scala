package js7.tests.special

import io.circe._
import io.circe.syntax._
import java.nio.charset.StandardCharsets.UTF_8
import js7.base.circeutils.CirceObjectCodec
import js7.base.circeutils.CirceUtils._
import js7.base.data.ByteArray
import js7.base.data.ByteSequence.ops._
import js7.base.monixutils.MonixBase.syntax.RichMonixObservable
import js7.base.problem.Checked._
import js7.base.time.ScalaTime._
import js7.base.time.Stopwatch.measureTimeOfSingleRun
import js7.base.utils.ScodecUtils.syntax._
import js7.common.log.ScribeUtils.coupleScribeWithSlf4j
import js7.common.scalautil.MonixUtils.syntax._
import js7.tests.special.CirceParallelizationSpeedTest._
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observable
import org.scalatest.freespec.AnyFreeSpec
import scala.collection.immutable.Seq
import scala.util.Random
import scodec.bits.ByteVector

final class CirceParallelizationSpeedTest extends AnyFreeSpec
{
  private type MyByteSeq = ByteArray
  private val MyByteSeq = ByteArray

  if (sys.props.contains("test.speed")) {
    coupleScribeWithSlf4j()

    val n = 1000000
    lazy val big = for (i <- 1 to n / 10) yield Big(i, Seq.fill(20)(Random.nextString(10)))
    lazy val small = for (i <- 1 to n) yield Small(i)
    lazy val bigJson = encodeParallelBatch(big)
    lazy val smallJson = encodeParallelBatch(small)

    "encode parallel batch" in {
      testEncode(big, "Big")(encodeParallelBatch)
      testEncode(small, "Small")(encodeParallelBatch)
    }

    "decode parallel batch" in {
      testDecode[Big](bigJson, "Big")(decodeParallelBatch[Big])
      testDecode[Small](smallJson, "Small")(decodeParallelBatch[Small])
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
      testEncode(big, "Big")(seq => encodeSerial(MyByteSeq, seq))
      testEncode(small, "Small")(seq => encodeSerial(MyByteSeq, seq))
      succeed
    }

    "decode sequential" in {
      testDecode[Big](bigJson, "Big")(decodeSerial[Big](MyByteSeq, _))
      testDecode[Small](smallJson, "Small")(decodeSerial[Small](MyByteSeq, _))
      succeed
    }
  }
  }

  private def testEncode[A: Encoder](seq: Seq[A], plural: String)(body: Seq[A] => Seq[MyByteSeq]): Unit = {
    val m = 20
    for (i <- 1 to m) {
      //System.gc()
      val timing = measureTimeOfSingleRun(seq.size, plural) {
        body(seq)
      }
      if (i > m / 2) scribe.info(s"Encode $timing")
    }
  }

  private def testDecode[A: Decoder](seq: Seq[MyByteSeq], plural: String)(body: Seq[MyByteSeq] => Seq[A]): Unit = {
    val m = 20
    for (i <- 1 to m) {
      //System.gc()
      val timing = measureTimeOfSingleRun(seq.size, plural) {
        body(seq)
      }
      if (i > m / 2) scribe.info(s"Decode $timing")
    }
  }

  private def encodeParallelBatch[A: Encoder](seq: Seq[A]): Seq[MyByteSeq] =
    Observable.fromIterable(seq)
      .mapParallelOrderedBatch()(encode[A](MyByteSeq, _))
      .toListL
      .await(99.s)

  private def decodeParallelBatch[A: Decoder](seq: Seq[MyByteSeq]): Seq[A] =
    Observable.fromIterable(seq)
      .mapParallelOrderedBatch()(decode[A])
      .toListL
      .await(99.s)

  private def encodeParallel[A: Encoder](seq: Seq[A]): Seq[MyByteSeq] =
    Observable.fromIterable(seq)
      .mapParallelOrderedBatch()(encode[A](MyByteSeq, _))
      .toListL
      .await(99.s)

  private def decodeParallel[A: Decoder](seq: Seq[MyByteSeq]): Seq[A] =
    Observable.fromIterable(seq)
      .mapParallelOrderedBatch()(bytes => decode(bytes))
      .toListL
      .await(99.s)

  private def encodeSerial[A: Encoder](x: ByteArray.type, seq: Seq[A]): Seq[ByteArray] =
    seq.map(encode[A](ByteArray, _))

  private def encodeSerial[A: Encoder](x: ByteVector.type, seq: Seq[A]): Seq[ByteVector] =
    seq.map(encode[A](ByteVector, _))

  private def decodeSerial[A: Decoder](x: ByteVector.type, seq: Seq[ByteVector]): Seq[A] =
    seq.map(_.parseJsonAs[A].orThrow)

  private def decodeSerial[A: Decoder](x: ByteArray.type, seq: Seq[ByteArray]): Seq[A] =
    seq.map(_.parseJsonAs[A].orThrow)

  private def encode[A: Encoder](x: ByteArray.type, a: A): ByteArray =
    ByteArray.fromString(a.asJson.compactPrint)
    //MyByteSeq(a.asJson.compactPrint.getBytes(UTF_8))

  private def encode[A: Encoder](x: ByteVector.type, a: A): ByteVector =
    ByteVector(a.asJson.compactPrint.getBytes(UTF_8))

  private def decode[A: Decoder](byteVector: ByteVector): A =
    byteVector.parseJsonAs[A].orThrow

  private def decode[A: Decoder](bytes: ByteArray): A =
    bytes.parseJsonAs[A].orThrow
}

private object CirceParallelizationSpeedTest
{
  case class Small(int: Int, string: String = "STRING", boolean: Boolean = true)
  object Small {
    implicit val jsonCodec: CirceObjectCodec[Small] = deriveCodec[Small]
  }

  case class Big(int: Int, array: Seq[String])
  object Big {
    implicit val jsonCodec: CirceObjectCodec[Big] = deriveCodec[Big]
  }
}
