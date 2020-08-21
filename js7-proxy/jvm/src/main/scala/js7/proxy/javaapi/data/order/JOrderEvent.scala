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
import js7.proxy.javaapi.data.JJsonable
import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters._

trait JOrderEvent
extends JJsonable[JOrderEvent]
{
  protected type Underlying <: OrderEvent
  protected def companion = JOrderEvent
}

object JOrderEvent extends JJsonable.Companion[JOrderEvent]
{
  override def fromJson(jsonString: String): VEither[Problem, JOrderEvent] =
    super.fromJson(jsonString)

  val jsonEncoder: Encoder[Underlying] = OrderEvent.jsonCodec.asInstanceOf[Encoder[Underlying]]
  val jsonDecoder: Decoder[Underlying] = OrderEvent.jsonCodec.asInstanceOf[Decoder[Underlying]]

  def of(orderEvent: OrderEvent): JOrderEvent =
    apply(orderEvent.asInstanceOf[Underlying])

  def apply(underlying: Underlying): JOrderEvent =
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

  final case class JOrderEventOther(underlying: OrderEvent)
  extends JOrderEvent {
    protected type Underlying = OrderEvent
  }

  final case class JOrderAdded(underlying: OrderAdded)
  extends JOrderEvent {
    protected type Underlying = OrderAdded

    def arguments: java.util.Map[String, String] =
      underlying.arguments.asJava

    def scheduledFor: Optional[Instant] =
      underlying.scheduledFor.map(o => o.toInstant).toJava
  }

  final case class JOrderProcessingStarted private(underlying: OrderProcessingStarted)
  extends JOrderEvent
  {
    protected type Underlying = OrderProcessingStarted
  }
  object JOrderProcessingStarted {
    val singleton = new JOrderProcessingStarted(OrderProcessingStarted)
    def apply(underlying: OrderProcessingStarted) = singleton
  }

  final case class JOrderStdWritten(underlying: OrderStdWritten)
  extends JOrderEvent {
    protected type Underlying = OrderStdWritten

    def stdoutOrStderr: StdoutOrStderr =
      underlying.stdoutStderr

    def chunk: String =
      underlying.chunk
  }

  final case class JOrderProcessed(underlying: OrderProcessed)
  extends JOrderEvent {
    protected type Underlying = OrderProcessed

    def outcome = underlying.outcome
  }

  final case class JOrderForked(underlying: OrderForked)
  extends JOrderEvent {
    protected type Underlying = OrderForked

    def children: java.util.List[JOrderForked.ForkedChild] =
      underlying.children.map(JOrderForked.ForkedChild.fromUnderlying).asJava
  }
  object JOrderForked {
    final case class ForkedChild(branchId: ForkBranchId, orderId: OrderId)
    object ForkedChild {
      def fromUnderlying(child: OrderForked.Child) =
        ForkedChild(ForkBranchId(child.branchId.string), child.orderId)
    }
  }

  final case class JOrderJoined(underlying: OrderJoined)
  extends JOrderEvent {
    protected type Underlying = OrderJoined

    def outcome = underlying.outcome
  }

  final case class JOrderFailed private(underlying: OrderFailed)
  extends JOrderEvent
  {
    protected type Underlying = OrderFailed

    def outcome = underlying.outcome
  }

  final case class JOrderFinished private(underlying: OrderFinished)
  extends JOrderEvent
  {
    protected type Underlying = OrderFinished
  }
  object JOrderFinished {
    val singleton = new JOrderFinished(OrderFinished)
    def apply(underlying: OrderFinished) = singleton
  }

  final case class JOrderCancelled private(underlying: OrderCancelled)
  extends JOrderEvent
  {
    protected type Underlying = OrderCancelled
  }
  object JOrderCancelled {
    val singleton = JOrderCancelled(OrderCancelled)
  }

  // TODO Move ForkBranchId out of here
  final case class ForkBranchId(string: String) extends GenericString
}
