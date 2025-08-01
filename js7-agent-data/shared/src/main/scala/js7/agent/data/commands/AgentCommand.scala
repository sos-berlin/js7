package js7.agent.data.commands

import io.circe.generic.semiauto.{deriveCodec, deriveEncoder}
import io.circe.{Codec, Decoder, Encoder, Json, JsonObject}
import js7.agent.data.AgentState
import js7.agent.data.AgentState.{inventoryItemKeyJsonCodec, signableItemJsonCodec, unsignedSimpleItemJsonCodec}
import js7.base.circeutils.CirceUtils.{deriveConfiguredCodec, singletonCodec}
import js7.base.circeutils.ScalaJsonCodecs.*
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.crypt.Signed
import js7.base.io.process.ProcessSignal
import js7.base.log.CorrelIdWrapped
import js7.base.problem.Checked
import js7.base.problem.Checked.*
import js7.base.problem.Checked.implicits.{checkedJsonDecoder, checkedJsonEncoder}
import js7.base.utils.Big
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.agent.{AgentPath, AgentRunId}
import js7.data.command.{CommonCommand, IsEventEmittingCommand}
import js7.data.controller.{ControllerId, ControllerRunId}
import js7.data.event.{EventId, ItemContainer}
import js7.data.item.{InventoryItemKey, SignableItem, UnsignedItem}
import js7.data.order.{Order, OrderId, OrderMark}
import js7.data.subagent.SubagentId

/**
 * @author Joacim Zschimmer
 */
sealed trait AgentCommand extends CommonCommand:
  type Response <: AgentCommand.Response


object AgentCommand extends CommonCommand.Companion:
  protected type Command = AgentCommand

  trait Response
  object Response:
    type Accepted = Accepted.type
    case object Accepted extends Response:
      implicit val jsonCodec: Codec[Accepted] = singletonCodec(Accepted)


  sealed trait IsEventEmittingAgentCommand extends AgentCommand, IsEventEmittingCommand


  final case class Batch(commands: Seq[CorrelIdWrapped[AgentCommand]])
  extends AgentCommand, CommonBatch, Big:
    type Response = Batch.Response
  object Batch:
    final case class Response(responses: Seq[Checked[AgentCommand.Response]])
    extends AgentCommand.Response, Big:
      override def toString: String =
        val succeeded = responses count (_.isRight)
        s"Batch($succeeded succeeded and ${responses.size - succeeded} failed)"


  final case class MarkOrder(orderId: OrderId, mark: OrderMark) extends IsOrderCommand:
    type Response = Response.Accepted


  final case class EmergencyStop(restart: Boolean = false) extends AgentCommand:
    /** The JVM is halted before responding. */
    type Response = Response.Accepted
  object EmergencyStop:
    implicit val jsonEncoder: Encoder.AsObject[EmergencyStop] = o =>
      JsonObject.fromIterable(
        o.restart.thenList("restart" -> Json.True))

    implicit val jsonDecoder: Decoder[EmergencyStop] = c =>
      for
        restart <- c.getOrElse[Boolean]("restart")(false)
      yield EmergencyStop(restart)


  /** Some outer component no longer needs the events until (including) the given `untilEventId`.
    * JS7 may delete these events to reduce the journal,
    * keeping all events after `untilEventId`.
    */
  final case class ReleaseEvents(untilEventId: EventId) extends IsOrderCommand:
    type Response = Response.Accepted


  case object NoOperation extends AgentCommand:
    type Response = Response.Accepted


  /** Registers the Controller identified by current User as a new Controller and couples it.
    * The Agent starts as a new Agent, dedicated to the Controller.
    * Command may be given twice (in case of a sudden restart).
    */
  final case class DedicateAgentDirector(
    directors: Seq[SubagentId],
    controllerId: ControllerId,
    controllerRunId: ControllerRunId,
    agentPath: AgentPath)
  extends AgentCommand:
    type Response = DedicateAgentDirector.Response

  object DedicateAgentDirector:
    /**
      * @param agentRunId Use the value for `CoupleController`. */
    final case class Response(agentRunId: AgentRunId, agentEventId: EventId)
    extends AgentCommand.Response

    implicit val jsonEncoder: Encoder.AsObject[DedicateAgentDirector] = deriveEncoder
    implicit val jsonDecoder: Decoder[DedicateAgentDirector] =
      c => for
        directors <- c.get[Option[SubagentId]]("subagentId").flatMap:
          case None => c.get[Option[Seq[SubagentId]]]("directors").map(_.toVector.flatten)
          case Some(subagentId) => Right(Seq(subagentId))
        controllerId <- c.get[ControllerId]("controllerId")
        controllerRunId <- c.get[ControllerRunId]("controllerRunId")
        agentPath <- c.get[AgentPath]("agentPath")
      yield DedicateAgentDirector(directors, controllerId, controllerRunId, agentPath)


  /** Couples the registered Controller identified by current User.
    * @param agentRunId Must be the value returned by `DedicateAgentDirector`. */
  final case class CoupleController(
    agentPath: AgentPath,
    agentRunId: AgentRunId,
    eventId: EventId,
    controllerRunId: ControllerRunId)
  extends AgentCommand:
    type Response = CoupleController.Response
  object CoupleController:
    final case class Response(orderIds: Set[OrderId])
    extends AgentCommand.Response, Big:
      override def toString = s"Response(${orderIds.size} Orders attached)"


  final case class Reset(agentRunId: Option[AgentRunId])
  extends AgentCommand

  object Reset:
    type Response = AgentCommand.Response


  type TakeSnapshot = TakeSnapshot.type

  case object TakeSnapshot extends AgentCommand:
    type Response = Response.Accepted


  sealed trait ShutdownOrAbort extends AgentCommand


  /**
    * @param processSignal Whether to kill the local Subagent's processes.
    * @param clusterAction For testing only
    * @param suppressSnapshot For testing only
    * @param restart Terminate program with a special return code which lets the
    *                outer script restart the Director
    * @param restartDirector Restart the Director abd keep the local Subagent running.
    */
  final case class ShutDown(
    processSignal: Option[ProcessSignal] = None,
    clusterAction: Option[ShutDown.ClusterAction] = None,
    suppressSnapshot: Boolean = false,
    restart: Boolean = false,
    restartDirector: Boolean = false)
  extends ShutdownOrAbort:
    type Response = Response.Accepted

    def isFailOrSwitchover: Boolean =
      isFailover || isSwitchover

    def isFailover: Boolean =
      clusterAction contains ShutDown.ClusterAction.Failover

    def isSwitchover: Boolean =
      clusterAction contains ShutDown.ClusterAction.Switchover
  object ShutDown:
    sealed trait ClusterAction
    object ClusterAction:
      case object Switchover extends ClusterAction

      case object Failover extends ClusterAction

      implicit val jsonCodec: TypedJsonCodec[ClusterAction] = TypedJsonCodec[ClusterAction](
        Subtype(Switchover),
        Subtype(Failover))

    implicit val jsonCodec: Codec.AsObject[ShutDown] =
      deriveConfiguredCodec[ShutDown]


  sealed trait IsOrderCommand extends IsEventEmittingAgentCommand
  sealed trait IsItemCommand extends AgentCommand


  final case class AttachItem(item: UnsignedItem)
  extends IsItemCommand:
    type Response = Response.Accepted

    override def toShortString =
      s"AttachItem(${item.key}${item.itemRevision.fold("")(o => "~" + o.number)})"

  final case class AttachSignedItem(signed: Signed[SignableItem])
  extends IsItemCommand:
    type Response = Response.Accepted
    override def toShortString = s"AttachSignedItem(${signed.value.key})"
    override def toString: String = toShortString
  object AttachSignedItem:
    // Same serialization as SignedItemAdded event
    implicit val jsonEncoder: Encoder.AsObject[AttachSignedItem] =
      o => SignableItem.signedEncodeJson(o.signed.signedString, o.signed.value.itemRevision)

    implicit def jsonDecoder[S: ItemContainer.Companion]
    : Decoder[AttachSignedItem] =
      c => SignableItem.signedJsonDecoder.decodeJson(c.value).map(AttachSignedItem(_))


  final case class DetachItem(key: InventoryItemKey)
  extends IsItemCommand:
    type Response = Response.Accepted


  sealed trait AttachOrDetachOrder extends IsOrderCommand


  final case class AttachOrder(order: Order[Order.IsFreshOrReady])
  extends AttachOrDetachOrder, Big:
    order.workflowId.requireNonAnonymous()
    order.attached.orThrow

    type Response = Response.Accepted

    override def toShortString: String =
      s"AttachOrder(${order.id.string}, ${order.workflowPosition}, " +
        s"${order.state.getClass.simpleScalaName}))"

  object AttachOrder:
    def apply(order: Order[Order.IsFreshOrReady], agentPath: AgentPath) =
      new AttachOrder(order.copy(attachedState = Some(Order.Attached(agentPath))))


  final case class DetachOrder(orderId: OrderId)
  extends AttachOrDetachOrder:
    type Response = Response.Accepted


  final case class ResetSubagent(subagentId: SubagentId, force: Boolean)
  extends AgentCommand:
    type Response = Response.Accepted


  type ClusterSwitchOver = ClusterSwitchOver.type

  case object ClusterSwitchOver
    extends AgentCommand:
    type Response = Response.Accepted


  implicit val jsonCodec: TypedJsonCodec[AgentCommand] =
    import AgentState.{implicitItemContainer, unsignedItemJsonCodec}
    intelliJuseImport(unsignedItemJsonCodec)
    TypedJsonCodec[AgentCommand](
      Subtype(deriveCodec[Batch]),
      Subtype(deriveCodec[MarkOrder]),
      Subtype[EmergencyStop],
      Subtype(deriveCodec[ReleaseEvents]),
      Subtype(NoOperation),
      Subtype[DedicateAgentDirector],
      Subtype(deriveCodec[CoupleController]),
      Subtype(deriveCodec[Reset]),
      Subtype[ShutDown],
      Subtype(deriveCodec[AttachItem]),
      Subtype[AttachSignedItem],
      Subtype(deriveCodec[DetachItem]),
      Subtype(deriveCodec[AttachOrder]),
      Subtype(deriveCodec[DetachOrder]),
      Subtype(TakeSnapshot),
      Subtype(deriveCodec[ResetSubagent]),
      Subtype(ClusterSwitchOver))

  given TypedJsonCodec[AgentCommand.Response] =
    TypedJsonCodec[AgentCommand.Response](
      Subtype.named(deriveCodec[CoupleController.Response], "CoupleController.Response"),
      Subtype.named(deriveCodec[Batch.Response], "Batch.Response"),
      Subtype(Response.Accepted),
      Subtype.named(deriveCodec[DedicateAgentDirector.Response], "DedicateAgentDirector.Response"))

  intelliJuseImport((FiniteDurationJsonDecoder,
    checkedJsonEncoder[Int], checkedJsonDecoder[Int],
    signableItemJsonCodec, unsignedSimpleItemJsonCodec, inventoryItemKeyJsonCodec))
