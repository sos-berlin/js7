package js7.agent.data.commands

import io.circe.generic.extras.Configuration.default.withDefaults
import io.circe.{Decoder, Encoder, Json, JsonObject}
import js7.agent.data.AgentState
import js7.agent.data.AgentState.{inventoryItemKeyJsonCodec, signableItemJsonCodec, unsignedSimpleItemJsonCodec}
import js7.base.circeutils.CirceCodec
import js7.base.circeutils.CirceUtils.{deriveCodec, deriveConfiguredCodec, singletonCodec}
import js7.base.circeutils.ScalaJsonCodecs._
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.crypt.Signed
import js7.base.io.process.ProcessSignal
import js7.base.problem.Checked
import js7.base.problem.Checked._
import js7.base.problem.Checked.implicits.{checkedJsonDecoder, checkedJsonEncoder}
import js7.base.utils.Big
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.base.utils.ScalaUtils.syntax._
import js7.data.agent.{AgentPath, AgentRunId}
import js7.data.command.CommonCommand
import js7.data.controller.ControllerId
import js7.data.event.{EventId, ItemContainer}
import js7.data.item.{InventoryItemKey, SignableItem, UnsignedSimpleItem}
import js7.data.order.{Order, OrderId, OrderMark}
import js7.data.subagent.SubagentId

/**
 * @author Joacim Zschimmer
 */
sealed trait AgentCommand extends CommonCommand {
  type Response <: AgentCommand.Response
}

object AgentCommand extends CommonCommand.Companion
{
  protected type Command = AgentCommand

  trait Response
  object Response {
    type Accepted = Accepted.type
    case object Accepted extends Response {
      implicit val jsonCodec: CirceCodec[Accepted] = singletonCodec(Accepted)
    }
  }

  final case class Batch(commands: Seq[AgentCommand])
  extends AgentCommand with CommonBatch with Big {
    type Response = Batch.Response
  }
  object Batch {
    final case class Response(responses: Seq[Checked[AgentCommand.Response]])
    extends AgentCommand.Response with Big {
      override def toString = {
        val succeeded = responses count (_.isRight)
        s"Batch($succeeded succeeded and ${responses.size - succeeded} failed)"
      }
    }
  }

  final case class MarkOrder(orderId: OrderId, mark: OrderMark) extends OrderCommand {
    type Response = Response.Accepted
  }

  final case class EmergencyStop(restart: Boolean = false) extends AgentCommand {
    /** The JVM is halted before responding. */
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
  final case class ReleaseEvents(untilEventId: EventId) extends OrderCommand {
    type Response = Response.Accepted
  }

  case object NoOperation extends AgentCommand {
    type Response = Response.Accepted
  }

  /** Registers the Controller identified by current User as a new Controller and couples it.
    * The Agent starts as a new Agent, dedicated to the Controller.
    * Command may be given twice (in case of a sudden restart).
    */
  final case class DedicateAgentDirector(
    subagentId: Option[SubagentId],
    controllerId: ControllerId,
    agentPath: AgentPath)
  extends AgentCommand {
    type Response = DedicateAgentDirector.Response
  }
  object DedicateAgentDirector {
    /**
      * @param agentRunId Use the value for `CoupleController`. */
    final case class Response(agentRunId: AgentRunId, agentEventId: EventId)
    extends AgentCommand.Response
  }

  /** Couples the registered Controller identified by current User.
    * @param agentRunId Must be the value returned by `DedicateAgentDirector`. */
  final case class CoupleController(agentPath: AgentPath, agentRunId: AgentRunId, eventId: EventId)
  extends AgentCommand {
    type Response = CoupleController.Response
  }
  object CoupleController {
    final case class Response(orderIds: Set[OrderId])
    extends AgentCommand.Response with Big
    {
      override def toString = s"Response(${orderIds.size} Orders attached)"
    }
  }

  final case class Reset(agentRunId: Option[AgentRunId])
  extends AgentCommand
  object Reset {
    type Response = AgentCommand.Response
  }

  type TakeSnapshot = TakeSnapshot.type
  case object TakeSnapshot extends AgentCommand {
    type Response = Response.Accepted
  }

  sealed trait ShutdownOrAbort extends AgentCommand

  final case class ShutDown(
    processSignal: Option[ProcessSignal] = None,
    suppressSnapshot: Boolean = false,
    restart: Boolean = false)
  extends ShutdownOrAbort {
    type Response = Response.Accepted
  }
  object ShutDown {
    private implicit val customConfig = withDefaults
    implicit val jsonCodec = deriveConfiguredCodec[ShutDown]
  }

  sealed trait OrderCommand extends AgentCommand

  final case class AttachItem(item: UnsignedSimpleItem)
  extends AgentCommand
  {
    type Response = Response.Accepted

    override def toShortString =
      s"AttachItem(${item.key}${item.itemRevision.fold("")(o => "~" + o.number)})"
  }

  final case class AttachSignedItem(signed: Signed[SignableItem])
  extends AgentCommand
  {
    type Response = Response.Accepted
    override def toShortString = s"AttachSignedItem(${signed.value.key})"
    override def toString = toShortString
  }
  object AttachSignedItem {
    // Same serialization as SignedItemAdded event
    implicit val jsonEncoder: Encoder.AsObject[AttachSignedItem] =
      o => SignableItem.signedEncodeJson(o.signed.signedString, o.signed.value.itemRevision)

    implicit def jsonDecoder[S: ItemContainer.Companion]
    : Decoder[AttachSignedItem] =
      c => SignableItem.signedJsonDecoder.decodeJson(c.value).map(AttachSignedItem(_))
  }

  final case class DetachItem(key: InventoryItemKey)
  extends AgentCommand
  {
    type Response = Response.Accepted
  }

  sealed trait AttachOrDetachOrder extends OrderCommand

  final case class AttachOrder(order: Order[Order.IsFreshOrReady])
  extends AttachOrDetachOrder with Big {
    order.workflowId.requireNonAnonymous()
    order.attached.orThrow

    type Response = Response.Accepted

    override def toShortString = s"AttachOrder(${order.id.string}, ${order.workflowPosition}, " +
      s"${order.state.getClass.simpleScalaName}))"
  }
  object AttachOrder {
    def apply(order: Order[Order.IsFreshOrReady], agentPath: AgentPath) =
      new AttachOrder(order.copy(attachedState = Some(Order.Attached(agentPath))))
  }

  final case class DetachOrder(orderId: OrderId)
  extends AttachOrDetachOrder {
    type Response = Response.Accepted
  }

  final case class ResetSubagent(subagentId: SubagentId, force: Boolean)
  extends AgentCommand {
    type Response = Response.Accepted
  }

  implicit val jsonCodec: TypedJsonCodec[AgentCommand] = {
    implicit val S = AgentState
    TypedJsonCodec[AgentCommand](
      Subtype(deriveCodec[Batch]),
      Subtype(deriveCodec[MarkOrder]),
      Subtype[EmergencyStop],
      Subtype(deriveCodec[ReleaseEvents]),
      Subtype(NoOperation),
      Subtype(deriveCodec[DedicateAgentDirector]),
      Subtype(deriveCodec[CoupleController]),
      Subtype(deriveCodec[Reset]),
      Subtype[ShutDown],
      Subtype(deriveCodec[AttachItem]),
      Subtype[AttachSignedItem],
      Subtype(deriveCodec[DetachItem]),
      Subtype(deriveCodec[AttachOrder]),
      Subtype(deriveCodec[DetachOrder]),
      Subtype(TakeSnapshot),
      Subtype(deriveCodec[ResetSubagent]))
  }

  implicit val responseJsonCodec: TypedJsonCodec[AgentCommand.Response] =
    TypedJsonCodec[AgentCommand.Response](
      Subtype.named(deriveCodec[CoupleController.Response], "CoupleController.Response"),
      Subtype.named(deriveCodec[Batch.Response], "Batch.Response"),
      Subtype(Response.Accepted),
      Subtype.named(deriveCodec[DedicateAgentDirector.Response], "DedicateAgentDirector.Response"))

  intelliJuseImport((FiniteDurationJsonDecoder,
    checkedJsonEncoder[Int], checkedJsonDecoder[Int],
    signableItemJsonCodec, unsignedSimpleItemJsonCodec, inventoryItemKeyJsonCodec))
}
