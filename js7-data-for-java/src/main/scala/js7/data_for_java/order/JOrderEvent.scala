package js7.data_for_java.order

import io.circe.{Decoder, Encoder}
import io.vavr.control.Either as VEither
import java.time.Instant
import java.util.Optional
import javax.annotation.Nonnull
import js7.base.generic.GenericString
import js7.base.io.process.StdoutOrStderr
import js7.base.problem.Problem
import js7.base.time.JavaTimestamp.specific.*
import js7.data.board.{BoardPath, NoticeId}
import js7.data.order.OrderEvent.{OrderAdded, OrderCancelled, OrderDeleted, OrderFailed, OrderFinished, OrderForked, OrderJoined, OrderNoticesExpected, OrderProcessed, OrderProcessingStarted, OrderStdWritten}
import js7.data.order.{OrderEvent, OrderId, Outcome}
import js7.data.subagent.SubagentId
import js7.data.value.Value
import js7.data_for_java.common.JJsonable
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*

trait JOrderEvent
extends JJsonable[JOrderEvent]:
  type AsScala <: OrderEvent

  protected def companion = JOrderEvent

object JOrderEvent extends JJsonable.Companion[JOrderEvent]:
  type AsScala = OrderEvent

  override def fromJson(jsonString: String): VEither[Problem, JOrderEvent] =
    super.fromJson(jsonString)

  protected val jsonEncoder: Encoder[AsScala] = OrderEvent.jsonCodec.asInstanceOf[Encoder[AsScala]]
  protected val jsonDecoder: Decoder[AsScala] = OrderEvent.jsonCodec.asInstanceOf[Decoder[AsScala]]

  @Nonnull
  def of(@Nonnull orderEvent: OrderEvent): JOrderEvent =
    apply(orderEvent.asInstanceOf[AsScala])

  @Nonnull
  def apply(@Nonnull underlying: AsScala): JOrderEvent =
    underlying match
      case event: OrderAdded => JOrderAdded(event)
      case event: OrderProcessingStarted => JOrderProcessingStarted(event)
      case event: OrderProcessed => JOrderProcessed(event)
      case event: OrderStdWritten => JOrderStdWritten(event)
      case event: OrderForked => JOrderForked(event)
      case event: OrderJoined => JOrderJoined(event)
      case event: OrderFailed => JOrderFailed(event)
      case event: OrderFinished => JOrderFinished(event)
      case event => JOrderEventOther(event)

  final case class JOrderEventOther(asScala: OrderEvent)
  extends JOrderEvent:
    type AsScala = OrderEvent

  final case class JOrderAdded(asScala: OrderAdded)
  extends JOrderEvent:
    type AsScala = OrderAdded

    @Nonnull
    def arguments: java.util.Map[String, Value] =
      asScala.arguments.asJava

    @Nonnull
    def scheduledFor: Optional[Instant] =
      asScala.scheduledFor.map(o => o.toInstant).toJava

  final case class JOrderProcessingStarted private[JOrderEvent](asScala: OrderProcessingStarted)
  extends JOrderEvent:
    type AsScala = OrderProcessingStarted

    /** @return empty iff written by v2.2 */
    def maybeSubagentId: Optional[SubagentId] =
      asScala.subagentId.toJava

  final case class JOrderStdWritten(asScala: OrderStdWritten)
  extends JOrderEvent:
    type AsScala = OrderStdWritten

    @Nonnull
    def stdoutOrStderr: StdoutOrStderr =
      asScala.stdoutStderr

    @Nonnull
    def chunk: String =
      asScala.chunk

  final case class JOrderProcessed(asScala: OrderProcessed)
  extends JOrderEvent:
    type AsScala = OrderProcessed

    @Nonnull
    def outcome: Outcome =
      asScala.outcome

  final case class JOrderForked(asScala: OrderForked)
  extends JOrderEvent:
    type AsScala = OrderForked

    @Nonnull
    def children: java.util.List[JOrderForked.ForkedChild] =
      asScala.children.map(JOrderForked.ForkedChild.fromScala).asJava
  object JOrderForked:
    final case class ForkedChild(
      orderId: OrderId,
      branchId: Optional[ForkBranchId],
      arguments: java.util.Map[String, Value])
    object ForkedChild:
      def fromScala(child: OrderForked.Child) =
        ForkedChild(
          child.orderId,
          child.branchId.map(o => ForkBranchId(o.string)).toJava,
          child.arguments.asJava)

  final case class JOrderJoined(asScala: OrderJoined)
  extends JOrderEvent:
    type AsScala = OrderJoined

    def outcome: Outcome =
      asScala.outcome

  final case class JOrderFailed(asScala: OrderFailed)
  extends JOrderEvent:
    type AsScala = OrderFailed

    @Nonnull
    def outcome: Option[Outcome.NotSucceeded] =
      asScala.outcome

  final case class JOrderFinished private(asScala: OrderFinished)
  extends JOrderEvent:
    type AsScala = OrderFinished
  object JOrderFinished:
    val singleton: JOrderFinished = new JOrderFinished(OrderFinished())
    def apply(asScala: OrderFinished): JOrderFinished =
      singleton

  final case class JOrderDeleted private(asScala: OrderDeleted)
  extends JOrderEvent:
    type AsScala = OrderDeleted
  object JOrderDeleted:
    val singleton: JOrderDeleted = new JOrderDeleted(OrderDeleted)

    def apply(asScala: OrderDeleted): JOrderDeleted =
      singleton

  final case class JOrderCancelled private(asScala: OrderCancelled)
  extends JOrderEvent:
    type AsScala = OrderCancelled
  object JOrderCancelled:
    val singleton: JOrderCancelled = JOrderCancelled(OrderCancelled)

  // TODO Move ForkBranchId out of here
  final case class ForkBranchId(string: String) extends GenericString

  final case class JExpectedNotice(asScala: OrderNoticesExpected.Expected):
    @Nonnull def boardPath: BoardPath =
      asScala.boardPath

    @Nonnull def noticeId: NoticeId =
      asScala.noticeId
