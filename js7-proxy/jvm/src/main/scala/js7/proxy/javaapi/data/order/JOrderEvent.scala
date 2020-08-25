package js7.proxy.javaapi.data.order

import io.circe.{Decoder, Encoder}
import io.vavr.control.{Either => VEither}
import java.time.Instant
import java.util.Optional
import js7.base.generic.GenericString
import js7.base.problem.Problem
import js7.data.order.OrderEvent.{OrderAdded, OrderCancelled, OrderFailed, OrderFinished, OrderForked, OrderJoined, OrderProcessed, OrderProcessingStarted, OrderStdWritten}
import js7.data.order.{OrderEvent, OrderId}
import js7.data.system.StdoutOrStderr
import js7.proxy.javaapi.data.common.JJsonable
import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters._

trait JOrderEvent
extends JJsonable[JOrderEvent]
{
  protected type AsScala <: OrderEvent
  protected def companion = JOrderEvent
}

object JOrderEvent extends JJsonable.Companion[JOrderEvent]
{
  override def fromJson(jsonString: String): VEither[Problem, JOrderEvent] =
    super.fromJson(jsonString)

  val jsonEncoder: Encoder[AsScala] = OrderEvent.jsonCodec.asInstanceOf[Encoder[AsScala]]
  val jsonDecoder: Decoder[AsScala] = OrderEvent.jsonCodec.asInstanceOf[Decoder[AsScala]]

  def of(orderEvent: OrderEvent): JOrderEvent =
    apply(orderEvent.asInstanceOf[AsScala])

  def apply(underlying: AsScala): JOrderEvent =
    underlying match {
      case event: OrderAdded => JOrderAdded(event)
      case event: OrderProcessingStarted => JOrderProcessingStarted(event)
      case event: OrderProcessed => JOrderProcessed(event)
      case event: OrderStdWritten => JOrderStdWritten(event)
      case event: OrderForked => JOrderForked(event)
      case event: OrderJoined => JOrderJoined(event)
      case event: OrderFailed => JOrderFailed(event)
      case event: OrderFinished => JOrderFinished(event)
      case event => JOrderEventOther(event)
    }

  final case class JOrderEventOther(asScala: OrderEvent)
  extends JOrderEvent {
    protected type AsScala = OrderEvent
  }

  final case class JOrderAdded(asScala: OrderAdded)
  extends JOrderEvent {
    protected type AsScala = OrderAdded

    def arguments: java.util.Map[String, String] =
      asScala.arguments.asJava

    def scheduledFor: Optional[Instant] =
      asScala.scheduledFor.map(o => o.toInstant).toJava
  }

  final case class JOrderProcessingStarted private(asScala: OrderProcessingStarted)
  extends JOrderEvent
  {
    protected type AsScala = OrderProcessingStarted
  }
  object JOrderProcessingStarted {
    val singleton = new JOrderProcessingStarted(OrderProcessingStarted)
    def apply(underlying: OrderProcessingStarted) = singleton
  }

  final case class JOrderStdWritten(asScala: OrderStdWritten)
  extends JOrderEvent {
    protected type AsScala = OrderStdWritten

    def stdoutOrStderr: StdoutOrStderr =
      asScala.stdoutStderr

    def chunk: String =
      asScala.chunk
  }

  final case class JOrderProcessed(asScala: OrderProcessed)
  extends JOrderEvent {
    protected type AsScala = OrderProcessed

    def outcome = asScala.outcome
  }

  final case class JOrderForked(asScala: OrderForked)
  extends JOrderEvent {
    protected type AsScala = OrderForked

    def children: java.util.List[JOrderForked.ForkedChild] =
      asScala.children.map(JOrderForked.ForkedChild.fromScala).asJava
  }
  object JOrderForked {
    final case class ForkedChild(branchId: ForkBranchId, orderId: OrderId)
    object ForkedChild {
      def fromScala(child: OrderForked.Child) =
        ForkedChild(ForkBranchId(child.branchId.string), child.orderId)
    }
  }

  final case class JOrderJoined(asScala: OrderJoined)
  extends JOrderEvent {
    protected type AsScala = OrderJoined

    def outcome = asScala.outcome
  }

  final case class JOrderFailed private(asScala: OrderFailed)
  extends JOrderEvent
  {
    protected type AsScala = OrderFailed

    def outcome = asScala.outcome
  }

  final case class JOrderFinished private(asScala: OrderFinished)
  extends JOrderEvent
  {
    protected type AsScala = OrderFinished
  }
  object JOrderFinished {
    val singleton = new JOrderFinished(OrderFinished)
    def apply(underlying: OrderFinished) = singleton
  }

  final case class JOrderCancelled private(asScala: OrderCancelled)
  extends JOrderEvent
  {
    protected type AsScala = OrderCancelled
  }
  object JOrderCancelled {
    val singleton = JOrderCancelled(OrderCancelled)
  }

  // TODO Move ForkBranchId out of here
  final case class ForkBranchId(string: String) extends GenericString
}
