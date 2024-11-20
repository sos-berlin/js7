package js7.data.event.tests

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import fs2.Stream
import io.circe.*
import io.circe.generic.semiauto.deriveCodec
import io.circe.syntax.*
import izumi.reflect.Tag
import js7.base.circeutils.CirceUtils.*
import js7.base.data.ByteArray
import js7.base.fs2utils.StreamExtensions
import js7.base.fs2utils.StreamExtensions.mapParallelBatch
import js7.base.log.Logger
import js7.base.monixlike.MonixLikeExtensions.toListL
import js7.base.problem.Checked.*
import js7.base.test.OurAsyncTestSuite
import js7.base.thread.CatsBlocking.syntax.awaitInfinite
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.measureTimeOfSingleRun
import js7.data.event.tests.CirceParallelizationSpeedTest.*
import js7.data.order.{HistoricOutcome, Order, OrderId, OrderOutcome}
import js7.data.value.{ListValue, StringValue}
import js7.data.workflow.WorkflowPath
import js7.data.workflow.position.BranchId.Then
import js7.data.workflow.position.BranchPath.syntax.*
import js7.data.workflow.position.Position

final class CirceParallelizationSpeedTest extends OurAsyncTestSuite:

  private given IORuntime = ioRuntime

  private val allowSlowTests = true

  if sys.props.contains("test.speed") then
    val n = 8 * sys.runtime.availableProcessors * StreamExtensions.DefaultBatchSizeMin
    lazy val big: Seq[Big] =
      val workflowPosition = WorkflowPath("WORKFLOW") ~ "1" /: (Position(1) / Then % 2 / Then % 3)
      val namedValues = Map("A" -> StringValue("a"), "B" -> ListValue((1 to 10).map(_.toString).map(StringValue(_))))
      val historicOutcome =
        HistoricOutcome(workflowPosition.position, OrderOutcome.Succeeded(namedValues))
      val fakeOrder = Order[Order.State](
        OrderId("?"),
        workflowPosition,
        Order.Forked(Vector(Order.Forked.Child("A", OrderId("A")), Order.Forked.Child("B", OrderId("B")))),
        arguments = namedValues,
        historicOutcomes = (1 to 50).map(_ => historicOutcome).toVector)
      logger.info(s"Big has ${fakeOrder.asJson.compactPrint.size} JSON bytes or ${
        fakeOrder.asJson.toPrettyString.count(_ == '\n')} pretty JSON lines")
      logger.debug(fakeOrder.asJson.toPrettyString)
      for i <- 1 to n / 10 yield Big(fakeOrder.copy(id = OrderId(i.toString)))
    lazy val small: Seq[Small] = for i <- 1 to n yield Small(i)
    lazy val bigJson: Seq[ByteArray] = encodeParallelBatch(big)
    lazy val smallJson: Seq[ByteArray] = encodeParallelBatch(small)

    "initialize" in:
      (big, small, bigJson, smallJson)
      succeed

    "encode" - {
      "mapParallelBatch" in:
        testEncode(big, "Big")(encodeParallelBatch)
        testEncode(small, "Small")(encodeParallelBatch)
        succeed

      if allowSlowTests then "parEvalMapUnbounded" in:
        testEncode(big, "Big")(encodeParallelUnbounded)
        testEncode(small, "Small")(encodeParallelUnbounded)
        succeed

      if allowSlowTests then "sequential" in:
        testEncode(big, "Big")(seq => encodeSerial(seq))
        testEncode(small, "Small")(seq => encodeSerial(seq))
        succeed
    }

    "decode" - {
      "mapParallelBatch" in:
        testDecode[Big](bigJson, "Big")(decodeParallelBatch[Big])
        testDecode[Small](smallJson, "Small")(decodeParallelBatch[Small])
        succeed

      if allowSlowTests then "parEvalMapUnbounded" in:
        testDecode[Big](bigJson, "Big")(decodeParallelBounded[Big])
        testDecode[Small](smallJson, "Small")(decodeParallelBounded[Small])
        succeed

      if allowSlowTests then "sequential" in:
        testDecode[Big](bigJson, "Big")(decodeSerial[Big](_))
        testDecode[Small](smallJson, "Small")(decodeSerial[Small](_))
        succeed
    }

  private val iterations = 0 until 20
  private val logFrom = 15

  private def testEncode[A: Encoder](seq: Seq[A], plural: String)(body: Seq[A] => Seq[ByteArray]): Unit =
    for i <- iterations do
      //System.gc()
      val timing = measureTimeOfSingleRun(seq.size, plural):
        body(seq)
      if i > logFrom then logger.info(s"Encode $timing")

  private def testDecode[A: Decoder](seq: Seq[ByteArray], plural: String)(body: Seq[ByteArray] => Seq[A]): Unit =
    for i <- iterations do
      //System.gc()
      val timing = measureTimeOfSingleRun(seq.size, plural):
        body(seq)
      if i > logFrom then logger.info(s"Decode $timing")

  private def encodeParallelBatch[A: Encoder](seq: Seq[A]): Seq[ByteArray] =
    Stream.iterable(seq)
      .covary[IO]
      .mapParallelBatch()(encode[A](_))
      .toListL
      .awaitInfinite

  private def decodeParallelBatch[A: Decoder: Tag](seq: Seq[ByteArray]): Seq[A] =
    Stream.iterable(seq)
      .covary[IO]
      .mapParallelBatch()(decode[A])
      .toListL
      .awaitInfinite

  private def encodeParallelUnbounded[A: Encoder](seq: Seq[A]): Seq[ByteArray] =
    Stream.iterable(seq)
      .covary[IO]
      .parEvalMapUnbounded(a => IO(encode(a)))
      .toListL
      .awaitInfinite

  private def decodeParallelBounded[A: Decoder: Tag](seq: Seq[ByteArray]): Seq[A] =
    Stream.iterable(seq)
      .covary[IO]
      .parEvalMapUnbounded(bytes => IO(decode(bytes)))
      .toListL
      .awaitInfinite

  private def encodeSerial[A: Encoder](seq: Seq[A]): Seq[ByteArray] =
    seq.map(encode[A](_))

  private def decodeSerial[A: Decoder](seq: Seq[ByteArray]): Seq[A] =
    seq.map(_.parseJsonAs[A].orThrow)

  private def encode[A: Encoder](a: A): ByteArray =
    a.asJson.toByteArray

  private def decode[A: Decoder](bytes: ByteArray): A =
    bytes.parseJsonAs[A].orThrow


private object CirceParallelizationSpeedTest:
  private val logger = Logger[this.type]

  case class Small(int: Int, string: String = "STRING", boolean: Boolean = true)
  object Small:
    implicit val jsonCodec: Codec.AsObject[Small] = deriveCodec[Small]

  case class Big(fakeOrder: Order[Order.State])
  object Big:
    implicit val jsonCodec: Codec.AsObject[Big] = deriveCodec[Big]
