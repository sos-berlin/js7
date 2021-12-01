package js7.subagent.data

import io.circe.generic.extras.Configuration.default.withDefaults
import io.circe.{Decoder, Encoder}
import js7.base.circeutils.CirceUtils.{deriveCodec, deriveConfiguredCodec}
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.crypt.Signed
import js7.base.io.process.ProcessSignal
import js7.base.io.process.ProcessSignal.SIGTERM
import js7.data.agent.AgentPath
import js7.data.controller.ControllerId
import js7.data.event.EventId
import js7.data.item.{InventoryItem, SignableItem}
import js7.data.order.{Order, OrderId}
import js7.data.subagent.{SubagentId, SubagentRunId}
import js7.data.value.expression.Expression
import js7.subagent.SubagentState._

sealed trait SubagentCommand
{
  type Response <: SubagentCommand.Response
}

object SubagentCommand
{
  sealed trait Response

  type Accepted = Accepted.type
  case object Accepted extends Response

  /** Registers the Controller identified by current User as a new Controller and couples it.
    * The Command is idempotent.
    */
  final case class DedicateSubagent(
    subagentId: SubagentId,
    agentPath: AgentPath,
    controllerId: ControllerId)
  extends SubagentCommand {
    type Response = DedicateSubagent.Response
  }
  object DedicateSubagent {
    final case class Response(subagentRunId: SubagentRunId, subagentEventId: EventId)
    extends SubagentCommand.Response
  }

  final case class CoupleDirector(
    subagentId: SubagentId,
    subagentRunId: SubagentRunId,
    eventId: EventId)
  extends SubagentCommand {
    type Response = SubagentCommand.Accepted
  }

  final case class AttachItem(item: InventoryItem)
  extends SubagentCommand {
    type Response = Accepted
  }
  object AttachItem

  final case class AttachSignedItem(signed: Signed[SignableItem])
  extends SubagentCommand {
    type Response = Accepted
  }
  object AttachSignedItem {
    // Same serialization as SignedItemAdded event
    implicit val jsonEncoder: Encoder.AsObject[AttachSignedItem] =
      o => SignableItem.signedEncodeJson(o.signed.signedString, o.signed.value.itemRevision)

    implicit val jsonDecoder: Decoder[AttachSignedItem] =
      c => SignableItem.signedJsonDecoder.decodeJson(c.value).map(AttachSignedItem(_))
  }

  final case class StartOrderProcess(
    order: Order[Order.Processing],
    defaultArguments: Map[String, Expression])
  extends SubagentCommand {
    type Response = Accepted
  }

  final case class KillProcess(
    orderId: OrderId,
    signal: ProcessSignal = SIGTERM)
  extends SubagentCommand {
    type Response = Accepted
  }

  final case class ShutDown(
    processSignal: Option[ProcessSignal] = None,
    restart: Boolean = false)
  extends SubagentCommand {
    type Response = Accepted
  }
  object ShutDown {
    private implicit val customConfig = withDefaults
    implicit val jsonCodec = deriveConfiguredCodec[ShutDown]
  }

  case object NoOperation extends SubagentCommand {
    type Response = Accepted
  }

  implicit val jsonCodec = TypedJsonCodec[SubagentCommand](
    Subtype(deriveCodec[DedicateSubagent]),
    Subtype(deriveCodec[CoupleDirector]),
    Subtype(deriveCodec[AttachItem]),
    //Subtype(deriveCodec[AttachUnsignedItem]),
    Subtype[AttachSignedItem],
    Subtype(deriveCodec[StartOrderProcess]),
    Subtype(deriveCodec[KillProcess]),
    Subtype(deriveCodec[ShutDown]),
    Subtype(NoOperation))

  implicit val responseJsonCodec = TypedJsonCodec[Response](
    Subtype(Accepted),
    Subtype(deriveCodec[DedicateSubagent.Response]))
}
