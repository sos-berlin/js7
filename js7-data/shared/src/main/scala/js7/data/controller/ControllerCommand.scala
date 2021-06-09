package js7.data.controller

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, Json, JsonObject}
import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.ScalaJsonCodecs.{FiniteDurationJsonDecoder, FiniteDurationJsonEncoder}
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.problem.Checked
import js7.base.problem.Checked.implicits.{checkedJsonDecoder, checkedJsonEncoder}
import js7.base.utils.Big
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.base.utils.ScalaUtils.syntax._
import js7.base.web.Uri
import js7.data.agent.AgentPath
import js7.data.cluster.{ClusterCommand, ClusterSetting}
import js7.data.command.{CancellationMode, CommonCommand, SuspensionMode}
import js7.data.controller.ControllerState._
import js7.data.event.EventId
import js7.data.node.NodeId
import js7.data.order.{FreshOrder, HistoricOutcome, OrderId}
import js7.data.workflow.position.Position
import scala.collection.immutable
import scala.concurrent.duration.FiniteDuration

/**
  * @author Joacim Zschimmer
  */
sealed trait ControllerCommand extends CommonCommand
{
  type Response <: ControllerCommand.Response
}

object ControllerCommand extends CommonCommand.Companion
{
  intelliJuseImport((FiniteDurationJsonEncoder, FiniteDurationJsonDecoder,
    checkedJsonEncoder[Int], checkedJsonDecoder[Int],
    itemPathJsonCodec))

  protected type Command = ControllerCommand

  final case class Batch(commands: Seq[ControllerCommand])
  extends ControllerCommand with CommonBatch with Big {
    type Response = Batch.Response
    override def toShortString = s"Batch(${commands.size} commands, ${commands.take(1).map(o => o.toShortString + ", ").mkString} ...)"
  }
  object Batch {
    final case class Response(responses: Seq[Checked[ControllerCommand.Response]])
    extends ControllerCommand.Response with CommonBatch.Response with Big {
      override def productPrefix = "BatchResponse"
    }
  }

  final case class AddOrder(order: FreshOrder) extends ControllerCommand {
    type Response = AddOrder.Response
  }
  object AddOrder {
    final case class Response(ignoredBecauseDuplicate: Boolean)
    extends ControllerCommand.Response
  }

  final case class AddOrders(orders: Seq[FreshOrder]) extends ControllerCommand {
    type Response = AddOrders.Response
    override def toShortString = s"AddOrders(${orders.size} orders, ${orders.take(1).map(o => o.toString.truncateWithEllipsis(200) + ", ").mkString} ...)"
  }
  object AddOrders {
    // AddOrderResponse is unnested to be accessible for Java code
    type Response = AddOrdersResponse
    val Response = AddOrdersResponse
  }
  final case class AddOrdersResponse(eventId: EventId) extends ControllerCommand.Response
  object AddOrdersResponse {
    implicit val jsonCodec = deriveCodec[AddOrdersResponse]
  }

  final case class CancelOrders(orderIds: immutable.Iterable[OrderId], mode: CancellationMode = CancellationMode.FreshOrStarted())
  extends ControllerCommand {
    type Response = Response.Accepted
    override def toShortString = s"CancelOrders(${orderIds.size} orders, ${orderIds.take(3).map(o => o.toString + ", ").mkString} ...)"
  }
  object CancelOrders {
    implicit val jsonEncoder: Encoder.AsObject[CancelOrders] = o =>
      JsonObject.fromIterable(
        ("orderIds" -> o.orderIds.asJson) ::
          (o.mode != CancellationMode.Default).thenList("mode" -> o.mode.asJson))

    implicit val jsonDecoder: Decoder[CancelOrders] = c =>
      for {
        orderIds <- c.get[Vector[OrderId]]("orderIds")
        mode <- c.getOrElse[CancellationMode]("mode")(CancellationMode.Default)
      } yield CancelOrders(orderIds, mode)
  }

  final case class DeleteOrdersWhenTerminated(orderIds: immutable.Iterable[OrderId])
  extends ControllerCommand {
    type Response = Response.Accepted
    override def toShortString = s"DeleteOrdersWhenTerminated(${orderIds.size} orders, ${orderIds.take(3).map(o => o.toString + ", ").mkString} ...)"
  }

  final case class AnswerOrderPrompt(orderId: OrderId)
  extends ControllerCommand {
    type response = Response.Accepted
  }

  final case class NoOperation(duration: Option[FiniteDuration] = None)
  extends ControllerCommand {
    type Response = Response.Accepted
  }

  type EmitTestEvent = EmitTestEvent.type
  /** For tests only. */
  case object EmitTestEvent extends ControllerCommand {
    type Response = Response.Accepted
  }

  /** Controller stops immediately with exit(). */
  final case class EmergencyStop(restart: Boolean = false) extends ControllerCommand {
    type Response = Response.Accepted
  }
  object EmergencyStop {
    implicit val jsonEncoder: Encoder.AsObject[EmergencyStop] = o =>
      JsonObject.fromIterable(
        o.restart.thenList("restart" -> Json.True))

    implicit val jsonDecoder: Decoder[EmergencyStop] = c =>
      for {
        restart <- c.getOrElse[Boolean]("restart")(false)
      } yield EmergencyStop(restart)
  }

  /** Some outer component no longer needs the events until (including) the given `untilEventId`.
    * JS7 may delete these events to reduce the journal,
    * keeping all events after `untilEventId`.
    */
  final case class ReleaseEvents(untilEventId: EventId) extends ControllerCommand {
    type Response = Response.Accepted
  }

  /** Shut down the Controller properly. */
  final case class ShutDown(
    restart: Boolean = false,
    clusterAction: Option[ShutDown.ClusterAction] = None,
    suppressSnapshot: Boolean = false)
  extends ControllerCommand {
    type Response = Response.Accepted
  }
  object ShutDown {
    sealed trait ClusterAction
    object ClusterAction {
      case object Switchover extends ClusterAction
      case object Failover extends ClusterAction
      implicit val jsonCodec: TypedJsonCodec[ClusterAction] = TypedJsonCodec[ClusterAction](
        Subtype(Switchover),
        Subtype(Failover))
    }

    implicit val jsonEncoder: Encoder.AsObject[ShutDown] = o =>
      JsonObject.fromIterable(
        o.restart.thenList("restart" -> Json.True) :::
        ("clusterAction" -> o.clusterAction.asJson) ::
        o.suppressSnapshot.thenList("suppressSnapshot" -> Json.True) :::
        Nil)

    implicit val jsonDecoder: Decoder[ShutDown] = c =>
      for {
        restart <- c.getOrElse[Boolean]("restart")(false)
        clusterAction <- c.get[Option[ClusterAction]]("clusterAction")
        suppressSnapshot <- c.getOrElse[Boolean]("suppressSnapshot")(false)
      } yield ShutDown(restart, clusterAction, suppressSnapshot)
  }

  final case class ResumeOrder(
    orderId: OrderId,
    position: Option[Position] = None,
    historicOutcomes: Option[Seq[HistoricOutcome]] = None)
  extends ControllerCommand {
    type Response = Response.Accepted
  }

  final case class ResumeOrders(orderIds: immutable.Iterable[OrderId])
  extends ControllerCommand {
    type Response = Response.Accepted
  }

  final case class SuspendOrders(orderIds: immutable.Iterable[OrderId], mode: SuspensionMode = SuspensionMode.standard)
  extends ControllerCommand {
    type Response = Response.Accepted
  }

  case object TakeSnapshot extends ControllerCommand {
    type Response = Response.Accepted
  }

  final case class ClusterAppointNodes(
    idToUri: Map[NodeId, Uri],
    activeId: NodeId,
    clusterWatches: Seq[ClusterSetting.Watch])
  extends ControllerCommand {
    type Response = Response.Accepted
  }

  case object ClusterSwitchOver
  extends ControllerCommand {
    type Response = Response.Accepted
  }

  /** For internal use between cluster nodes only. */
  final case class InternalClusterCommand(clusterCommand: ClusterCommand)
  extends ControllerCommand {
    type Response = InternalClusterCommand.Response
  }
  object InternalClusterCommand {
    final case class Response(response: ClusterCommand.Response)
    extends ControllerCommand.Response
  }

  final case class ResetAgent(agentPath: AgentPath)
  extends ControllerCommand {
    type Response = Response.Accepted
  }

  sealed trait Response

  object Response {
    type Accepted = Accepted.type
    case object Accepted extends Response

    implicit val ResponseJsonCodec: TypedJsonCodec[Response] = TypedJsonCodec[Response](
      Subtype(Accepted),
      Subtype.named(deriveCodec[AddOrder.Response], "AddOrder.Response"),
      Subtype.named[AddOrders.Response]("AddOrders.Response"),
      Subtype.named(deriveCodec[Batch.Response], "BatchResponse"),
      Subtype.named(deriveCodec[InternalClusterCommand.Response], "InternalClusterCommand.Response"))
  }

  implicit val jsonCodec: TypedJsonCodec[ControllerCommand] = TypedJsonCodec[ControllerCommand](
    Subtype(deriveCodec[Batch]),
    Subtype(deriveCodec[AddOrder]),
    Subtype(deriveCodec[AddOrders]),
    Subtype[CancelOrders],
    Subtype(deriveCodec[DeleteOrdersWhenTerminated]),
    Subtype(deriveCodec[AnswerOrderPrompt]),
    Subtype(deriveCodec[NoOperation]),
    Subtype(EmitTestEvent),
    Subtype[EmergencyStop],
    Subtype(deriveCodec[ReleaseEvents]),
    Subtype[ShutDown],
    Subtype(deriveCodec[ResumeOrder]),
    Subtype(deriveCodec[ResumeOrders]),
    Subtype(deriveCodec[SuspendOrders]),
    Subtype(deriveCodec[ClusterAppointNodes]),
    Subtype(ClusterSwitchOver),
    Subtype(deriveCodec[InternalClusterCommand]),
    Subtype(deriveCodec[ResetAgent]),
    Subtype(TakeSnapshot))
}
