package js7.controller.data

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, Json, JsonObject}
import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.ScalaJsonCodecs.{FiniteDurationJsonDecoder, FiniteDurationJsonEncoder}
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.crypt.SignedString
import js7.base.problem.Checked._
import js7.base.problem.Checked.implicits.{checkedJsonDecoder, checkedJsonEncoder}
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Big
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.base.utils.ScalaUtils.syntax._
import js7.base.web.Uri
import js7.data.cluster.{ClusterCommand, ClusterSetting}
import js7.data.command.{CancelMode, CommonCommand}
import js7.data.controller.ControllerItems.typedPathJsonDecoder
import js7.data.event.EventId
import js7.data.item.{TypedPath, VersionId}
import js7.data.node.NodeId
import js7.data.order.{FreshOrder, OrderId}
import js7.data.workflow.position.Position
import scala.collection.immutable

/**
  * @author Joacim Zschimmer
  */
sealed trait ControllerCommand extends CommonCommand
{
  type Response <: ControllerCommand.Response
}

object ControllerCommand extends CommonCommand.Companion
{
  intelliJuseImport((FiniteDurationJsonEncoder, FiniteDurationJsonDecoder, checkedJsonEncoder[Int], checkedJsonDecoder[Int], Problem, typedPathJsonDecoder))

  protected type Command = ControllerCommand

  final case class Batch(commands: Seq[ControllerCommand])
  extends ControllerCommand with CommonBatch with Big {
    type Response = Batch.Response
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

  final case class CancelOrders(orderIds: immutable.Iterable[OrderId], mode: CancelMode = CancelMode.FreshOrStarted())
  extends ControllerCommand {
    type Response = Response.Accepted
  }
  object CancelOrders {
    implicit val jsonEncoder: Encoder.AsObject[CancelOrders] = o =>
      JsonObject.fromIterable(
        ("orderIds" -> o.orderIds.asJson) ::
          (o.mode != CancelMode.Default).thenList("mode" -> o.mode.asJson))

    implicit val jsonDecoder: Decoder[CancelOrders] = c =>
      for {
        orderIds <- c.get[Vector[OrderId]]("orderIds")
        mode <- c.get[Option[CancelMode]]("mode") map (_ getOrElse CancelMode.Default)
      } yield CancelOrders(orderIds, mode)
  }

  final case class RemoveOrdersWhenTerminated(orderIds: immutable.Iterable[OrderId])
  extends ControllerCommand {
    type Response = Response.Accepted
  }

  type NoOperation = NoOperation.type
  case object NoOperation extends ControllerCommand {
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
        restart <- c.get[Option[Boolean]]("restart") map (_ getOrElse false)
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
        restart <- c.get[Option[Boolean]]("restart").map(_ getOrElse false)
        cluster <- c.get[Option[ClusterAction]]("clusterAction")
        suppressSnapshot <- c.get[Option[Boolean]]("suppressSnapshot").map(_ getOrElse false)
      } yield ShutDown(restart, cluster, suppressSnapshot)
  }

  final case class ResumeOrders(orderIds: immutable.Iterable[OrderId], position: Option[Position] = None)
  extends ControllerCommand {
    type Response = Response.Accepted
  }

  final case class SuspendOrders(orderIds: immutable.Iterable[OrderId]) extends ControllerCommand {
    type Response = Response.Accepted
  }

  case object TakeSnapshot extends ControllerCommand {
    type Response = Response.Accepted
  }

  final case class UpdateRepo(
    versionId: VersionId,
    change: Seq[SignedString] = Nil,
    delete: Seq[TypedPath] = Nil)
  extends ControllerCommand with Big {
    type Response = Response.Accepted

    def isEmpty = change.isEmpty && delete.isEmpty

    override def toString = s"UpdateRepo($versionId change=${change.size}× delete=${delete.size}×)"
  }

  final case class ReplaceRepo(versionId: VersionId, objects: Seq[SignedString])
  extends ControllerCommand with Big {
    type Response = Response.Accepted

    override def toString = s"ReplaceRepo($versionId, ${objects.size} objects)"
  }

  final case class ClusterAppointNodes(idToUri: Map[NodeId, Uri], activeId: NodeId)
  extends ControllerCommand {
    type Response = Response.Accepted
    ClusterSetting.checkUris(idToUri, activeId).orThrow
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

  sealed trait Response

  object Response {
    type Accepted = Accepted.type
    case object Accepted extends Response

    implicit val ResponseJsonCodec: TypedJsonCodec[Response] = TypedJsonCodec[Response](
      Subtype(Accepted),
      Subtype.named(deriveCodec[AddOrder.Response], "AddOrder.Response"),
      Subtype.named[AddOrders.Response]("AddOrders.Response"),
      Subtype.named(deriveCodec[Batch.Response], "BatchResponse"),
      Subtype.named(deriveCodec[js7.controller.data.ControllerCommand.InternalClusterCommand.Response], "InternalClusterCommand.Response"))
  }

  implicit val jsonCodec: TypedJsonCodec[ControllerCommand] = TypedJsonCodec[ControllerCommand](
    Subtype(deriveCodec[Batch]),
    Subtype(deriveCodec[AddOrder]),
    Subtype(deriveCodec[AddOrders]),
    Subtype[CancelOrders],
    Subtype(deriveCodec[RemoveOrdersWhenTerminated]),
    Subtype(deriveCodec[ReplaceRepo]),
    Subtype(deriveCodec[UpdateRepo]),
    Subtype(NoOperation),
    Subtype(EmitTestEvent),
    Subtype[EmergencyStop],
    Subtype(deriveCodec[ReleaseEvents]),
    Subtype[ShutDown],
    Subtype(deriveCodec[ResumeOrders]),
    Subtype(deriveCodec[SuspendOrders]),
    Subtype(deriveCodec[ClusterAppointNodes]),
    Subtype(ClusterSwitchOver),
    Subtype(deriveCodec[InternalClusterCommand]),
    Subtype(TakeSnapshot))
}
