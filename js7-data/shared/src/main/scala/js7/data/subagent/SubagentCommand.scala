package js7.data.subagent

import io.circe.generic.semiauto.deriveCodec
import io.circe.{Codec, Decoder, Encoder}
import js7.base.circeutils.CirceUtils.deriveConfiguredCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.crypt.Signed
import js7.base.io.process.ProcessSignal
import js7.base.io.process.ProcessSignal.SIGTERM
import js7.base.log.CorrelIdWrapped
import js7.base.problem.Checked
import js7.base.time.Timestamp
import js7.base.utils.Big
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.version.Version
import js7.data.agent.{AgentPath, AgentRunId}
import js7.data.command.CommonCommand
import js7.data.controller.ControllerId
import js7.data.event.EventId
import js7.data.item.SignableItem
import js7.data.order.{Order, OrderId}
import js7.data.other.HeartbeatTiming
import js7.data.subagent.SubagentState.*
import js7.data.value.expression.Expression

sealed trait SubagentCommand extends CommonCommand:
  type Response <: SubagentCommand.Response


object SubagentCommand extends CommonCommand.Companion:
  protected type Command = SubagentCommand
  sealed trait Response

  type Accepted = Accepted.type
  case object Accepted extends Response

  sealed trait Queueable extends SubagentCommand

  final case class Batch(commands: Seq[CorrelIdWrapped[SubagentCommand]])
  extends SubagentCommand, CommonBatch, Big:
    type Response = Batch.Response
  object Batch:
    final case class Response(responses: Seq[Checked[SubagentCommand.Response]])
    extends SubagentCommand.Response, Big:
      override def toString: String =
        val succeeded = responses count (_.isRight)
        s"Batch($succeeded succeeded and ${responses.size - succeeded} failed)"

  /** Registers the Controller identified by current User as a new Controller and couples it.
    * The Command is idempotent.
    */
  final case class DedicateSubagent(
    subagentId: SubagentId,
    agentPath: AgentPath,
    agentRunId: AgentRunId,
    controllerId: ControllerId)
  extends SubagentCommand:
    type Response = DedicateSubagent.Response
  object DedicateSubagent:
    final case class Response(
      subagentRunId: SubagentRunId,
      subagentEventId: EventId,
      version/*COMPATIBLE with v2.3*/: Option[Version])
    extends SubagentCommand.Response

    given Encoder.AsObject[DedicateSubagent] = deriveCodec[DedicateSubagent]
    given Decoder[DedicateSubagent] =
      c =>
        for
          subagentId <- c.get[SubagentId]("subagentId")
          agentPath <- c.get[AgentPath]("agentPath")
          agentRunId <- c.getOrElse[AgentRunId]("agentRunId")(AgentRunId.pastAgentVersion)
          controllerId <- c.get[ControllerId]("controllerId")
        yield
          DedicateSubagent(subagentId, agentPath, agentRunId, controllerId)

  final case class CoupleDirector(
    subagentId: SubagentId,
    subagentRunId: SubagentRunId,
    eventId: EventId,
    heartbeatTiming: HeartbeatTiming)
  extends SubagentCommand:
    type Response = SubagentCommand.Accepted

  //final case class AttachItem(item: InventoryItem)
  //extends SubagentCommand {
  //  type Response = Accepted
  //
  //  override def toShortString = s"AttachItem(${item.key})"
  //}
  //object AttachItem

  // TODO Send and check AgentRunId with each command
  final case class AttachSignedItem(signed: Signed[SignableItem])
  extends SubagentCommand:
    type Response = Accepted

    override def toShortString = s"AttachSignedItem(${signed.value.key})"
  object AttachSignedItem:
    // Same serialization as SignedItemAdded event
    implicit val jsonEncoder: Encoder.AsObject[AttachSignedItem] =
      o => SignableItem.signedEncodeJson(o.signed.signedString, o.signed.value.itemRevision)

    implicit val jsonDecoder: Decoder[AttachSignedItem] =
      c => SignableItem.signedJsonDecoder.decodeJson(c.value).map(AttachSignedItem(_))

  sealed trait OrderCommand extends Queueable:
    def orderId: OrderId

  final case class StartOrderProcess(
    order: Order[Order.Processing],
    defaultArguments: Map[String, Expression],
    endOfAdmissionPeriod: Option[Timestamp] = None)
  extends OrderCommand:
    type Response = Accepted

    def orderId: OrderId = order.id
    override def toShortString = s"StartOrderProcess(${order.id})"

  final case class KillProcess(
    orderId: OrderId,
    signal: ProcessSignal = SIGTERM)
  extends OrderCommand:
    type Response = Accepted

  // COMPATIBLE with v2.8.0
  final case class DetachProcessedOrder(orderId: OrderId)
  extends OrderCommand:
    type Response = Accepted

  final case class DetachProcessedOrders(orderIds: Seq[OrderId])
  extends Queueable, Big:
    type Response = Accepted

  final case class ShutDown(
    processSignal: Option[ProcessSignal] = None,
    dontWaitForDirector: Boolean = false,
    restart: Boolean = false)
  extends Queueable:
    type Response = Accepted

    override def toString =
      "ShutDown(" +
        Seq(processSignal, dontWaitForDirector ? "dontWaitForDirector", restart ? "restart")
          .flatten.mkString(" ") +
        ")"
  object ShutDown:
    implicit val jsonCodec: Codec.AsObject[ShutDown] = deriveConfiguredCodec

  /** Some outer component no longer needs the events until (including) the given `untilEventId`.
   * JS7 may delete these events to reduce the journal,
   * keeping all events after `untilEventId`.
   * The command MUST also be issued after OrderProcessed to release the OrderId.
   * This is to detect a duplicate (idempotent) StartOrderProcess command.
   */
  final case class ReleaseEvents(untilEventId: EventId)
  extends Queueable:
    type Response = Accepted

  case object NoOperation extends SubagentCommand:
    type Response = Accepted

  implicit val jsonCodec: TypedJsonCodec[SubagentCommand] = TypedJsonCodec[SubagentCommand](
    Subtype(deriveCodec[Batch]),
    Subtype[DedicateSubagent],
    Subtype(deriveCodec[CoupleDirector]),
    //Subtype(deriveCodec[AttachItem]),
    Subtype[AttachSignedItem],
    Subtype(deriveCodec[StartOrderProcess]),
    Subtype(deriveCodec[DetachProcessedOrder]),
    Subtype(deriveCodec[DetachProcessedOrders]),
    Subtype(deriveCodec[ReleaseEvents]),
    Subtype[ShutDown],
    Subtype(deriveCodec[KillProcess]),
    Subtype(NoOperation))

  implicit val responseJsonCodec: TypedJsonCodec[Response] = TypedJsonCodec(
    Subtype(Accepted),
    Subtype(deriveCodec[DedicateSubagent.Response]))
