package js7.tests.special

import io.circe._
import io.circe.syntax._
import js7.base.circeutils.CirceObjectCodec
import js7.base.circeutils.CirceUtils._
import js7.base.data.ByteArray
import js7.base.log.ScribeForJava.coupleScribeWithSlf4j
import js7.base.monixutils.MonixBase
import js7.base.monixutils.MonixBase.syntax.RichMonixObservable
import js7.base.problem.Checked._
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.base.time.Stopwatch.measureTimeOfSingleRun
import js7.data.order.{HistoricOutcome, Order, OrderId, Outcome}
import js7.data.value.{ListValue, StringValue}
import js7.data.workflow.WorkflowPath
import js7.data.workflow.position.BranchId.Then
import js7.data.workflow.position.Position
import js7.tests.special.CirceParallelizationSpeedTest._
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observable
import org.scalatest.freespec.AnyFreeSpec

final class CirceParallelizationSpeedTest extends AnyFreeSpec
{
  if (sys.props.contains("test.speed")) {
    coupleScribeWithSlf4j()

    val n = 4 * sys.runtime.availableProcessors * MonixBase.DefaultBatchSize
    lazy val big = {
      val workflowPosition = WorkflowPath("WORKFLOW") ~ "1" /: (Position(1) / Then % 2 / Then % 3)
      val namedValues = Map("A" -> StringValue("a"), "B" -> ListValue((1 to 10).map(_.toString).map(StringValue(_))))
      val historicOutcome = HistoricOutcome(workflowPosition.position, Outcome.Succeeded(namedValues))
      val fakeOrder = Order[Order.State](
        OrderId("?"),
        workflowPosition,
        Order.Forked(Vector(Order.Forked.Child("A", OrderId("A")), Order.Forked.Child("B", OrderId("B")))),
        namedValues,
        historicOutcomes = (1 to 50).map(_ => historicOutcome).toVector)
      scribe.info(s"Big has ${fakeOrder.asJson.compactPrint.size} JSON bytes or ${fakeOrder.asJson.toPrettyString.count(_ == '\n')} JSON lines")
      scribe.debug(fakeOrder.asJson.toPrettyString)
      for (i <- 1 to n / 10) yield Big(fakeOrder.copy(id = OrderId(i.toString)))
    }
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

  if (true) { // slow
    "encode sequential" in {
      testEncode(big, "Big")(seq => encodeSerial(seq))
      testEncode(small, "Small")(seq => encodeSerial(seq))
      succeed
    }

    "decode sequential" in {
      testDecode[Big](bigJson, "Big")(decodeSerial[Big](_))
      testDecode[Small](smallJson, "Small")(decodeSerial[Small](_))
      succeed
    }
  }
  }

  private def testEncode[A: Encoder](seq: Seq[A], plural: String)(body: Seq[A] => Seq[ByteArray]): Unit = {
    val m = 20
    for (i <- 1 to m) {
      //System.gc()
      val timing = measureTimeOfSingleRun(seq.size, plural) {
        body(seq)
      }
      if (i > m / 2) scribe.info(s"Encode $timing")
    }
  }

  private def testDecode[A: Decoder](seq: Seq[ByteArray], plural: String)(body: Seq[ByteArray] => Seq[A]): Unit = {
    val m = 20
    for (i <- 1 to m) {
      //System.gc()
      val timing = measureTimeOfSingleRun(seq.size, plural) {
        body(seq)
      }
      if (i > m / 2) scribe.info(s"Decode $timing")
    }
  }

  private def encodeParallelBatch[A: Encoder](seq: Seq[A]): Seq[ByteArray] =
    Observable.fromIterable(seq)
      .mapParallelBatch()(encode[A](_))
      .toListL
      .await(99.s)

  private def decodeParallelBatch[A: Decoder](seq: Seq[ByteArray]): Seq[A] =
    Observable.fromIterable(seq)
      .mapParallelBatch()(decode[A])
      .toListL
      .await(99.s)

  private def encodeParallel[A: Encoder](seq: Seq[A]): Seq[ByteArray] =
    Observable.fromIterable(seq)
      .mapParallelBatch()(encode[A](_))
      .toListL
      .await(99.s)

  private def decodeParallel[A: Decoder](seq: Seq[ByteArray]): Seq[A] =
    Observable.fromIterable(seq)
      .mapParallelBatch()(bytes => decode(bytes))
      .toListL
      .await(99.s)

  private def encodeSerial[A: Encoder](seq: Seq[A]): Seq[ByteArray] =
    seq.map(encode[A](_))

  private def decodeSerial[A: Decoder](seq: Seq[ByteArray]): Seq[A] =
    seq.map(_.parseJsonAs[A].orThrow)

  private def encode[A: Encoder](a: A): ByteArray =
    a.asJson.toByteArray

  private def decode[A: Decoder](bytes: ByteArray): A =
    bytes.parseJsonAs[A].orThrow
}

private object CirceParallelizationSpeedTest
{
  case class Small(int: Int, string: String = "STRING", boolean: Boolean = true)
  object Small {
    implicit val jsonCodec: CirceObjectCodec[Small] = deriveCodec[Small]
  }

  case class Big(fakeOrder: Order[Order.State])
  object Big {
    implicit val jsonCodec: CirceObjectCodec[Big] = deriveCodec[Big]
  }
}
