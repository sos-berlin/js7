package com.sos.jobscheduler.data.order

import com.sos.jobscheduler.base.circeutils.CirceCodec
import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCirceCodec
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.ScalaUtils.{RichJavaClass, implicitClass}
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.order.Order._
import com.sos.jobscheduler.data.order.OrderEvent._
import com.sos.jobscheduler.data.workflow.{NodeId, NodeKey, WorkflowPath}
import io.circe.generic.JsonCodec
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
final case class Order[+S <: Order.State](
  id: OrderId,
  nodeKey: NodeKey,
  state: S,
  attachedTo: Option[AttachedTo] = None,
  payload: Payload = Payload.empty)
{
  def toLean = LeanOrder(id, payload)

  def withLean(lean: LeanOrder): Order[Order.State] = {
    val LeanOrder(id_, payload_) = lean
    copy(
      id = id_,
      payload = payload_)
  }

  def workflowPath: WorkflowPath =
    nodeKey.workflowPath

  def nodeId: NodeId =
    nodeKey.nodeId

  def update(event: OrderEvent.OrderCoreEvent): Order[State] =
    event match {
      //case OrderAdded(nodeKey_, state_, variables_, outcome_) ⇒
      //  copy(nodeKey = nodeKey_, state = state_, variables = variables_, outcome = outcome_)
      //
      //case OrderAttached(nodeKey_, state_, variables_, outcome_) ⇒
      //  copy(nodeKey = nodeKey_, state = state_, variables = variables_, outcome = outcome_)

      case OrderMovedToAgent(o) ⇒ copy(
        attachedTo = Some(AttachedTo.Agent(o)))

      case OrderMovedToMaster ⇒ copy(
        attachedTo = None)

      case OrderProcessingStarted ⇒ copy(
        state = InProcess)

      case OrderProcessed(diff, outcome_) ⇒ copy(
        state = Processed,
        payload = Payload(
          variables = diff.applyTo(payload.variables),
          outcome = outcome_))

      case OrderTransitioned(toNodeId) ⇒ copy(
        state = Ready,
        nodeKey = NodeKey(workflowPath, toNodeId))

      case OrderDetachable ⇒
        attachedTo match {
          case None ⇒
            throw new IllegalStateException(s"Event OrderDetachable but '$id' is AttachedTo.$attachedTo")
          case Some(AttachedTo.Agent(agentPath)) ⇒
            copy(attachedTo = Some(AttachedTo.Detachable(agentPath)))
          case Some(AttachedTo.Detachable(_)) ⇒
            this
        }

      case OrderDetached ⇒ copy(
        attachedTo = None)

      case OrderFinished ⇒ copy(
        state = Finished)
    }

  def variables = payload.variables

  def outcome = payload.outcome

  def castAfterEvent(event: OrderProcessingStarted.type): Order[Order.InProcess.type] =
    castState[Order.InProcess.type]

  def castAfterEvent(event: OrderProcessed.type): Order[Order.Processed.type] =
    castState[Order.Processed.type]

  def castState[T <: State: ClassTag]: Order[T] = {
    val cls = implicitClass[T]
    if (!cls.isAssignableFrom(state.getClass))
      throw new IllegalStateException(s"'$id' is expected to be ${cls.simpleScalaName}, but is $state")
    this.asInstanceOf[Order[T]]
  }

  def attachedToAgent: Either[IllegalStateException, AgentPath] =
    attachedTo match {
      case Some(AttachedTo.Agent(agentPath)) ⇒
        Right(agentPath)
      case o ⇒
        Left(new IllegalStateException(s"'$id' is expected to be AttachedTo.Agent, but not: $o"))
    }

  def detachableFromAgent: Either[IllegalStateException, AgentPath] =
    attachedTo match {
      case Some(AttachedTo.Detachable(agentPath)) ⇒
        Right(agentPath)
      case o ⇒
        Left(new IllegalStateException(s"'$id' is expected to be AttachedTo.Detachable, but not: $o"))
    }
}

object Order {
  def fromOrderAdded(id: OrderId, event: OrderAdded): Order[Idle] =
    Order(id, event.nodeKey, event.state, None, event.payload)

  def fromOrderAttached(id: OrderId, event: OrderAttached): Order[Idle] =
    Order(id, event.nodeKey, event.state, Some(AttachedTo.Agent(event.agentPath)), event.payload)

  sealed trait AttachedTo
  object AttachedTo {
    sealed trait AgentOrDetachable extends AttachedTo {
      val agentPath: AgentPath
    }
    object AgentOrDetachable {
      def unapply(o: AttachedTo): Option[AgentPath] =
        o match {
          case o: AgentOrDetachable ⇒ Some(o.agentPath)
          case _ ⇒ None
        }
    }

    @JsonCodec
    final case class Agent(agentPath: AgentPath) extends AttachedTo

    @JsonCodec
    final case class Detachable(agentPath: AgentPath) extends AgentOrDetachable

    implicit val JsonCodec = TypedJsonCodec[AttachedTo](
      Subtype[Agent],
      Subtype[Detachable])
  }

  sealed trait State
  sealed trait Idle extends State
  sealed trait NotStarted extends Idle
  sealed trait Started extends State
  final case class Scheduled(at: Timestamp) extends NotStarted
  final case object StartNow extends NotStarted
  case object Ready extends Started with Idle
  case object InProcess extends Started
  case object Processed extends Started
  case object Finished extends State

  implicit val NotStartedJsonCodec: TypedJsonCodec[NotStarted] = TypedJsonCodec[NotStarted](
    Subtype(deriveCirceCodec[Scheduled]),
    Subtype(StartNow))
  implicit val NotStartedOrderJsonCodec: CirceCodec[Order[NotStarted]] = deriveCirceCodec[Order[NotStarted]]

  implicit val IdleJsonCodec: TypedJsonCodec[Idle] = TypedJsonCodec[Idle](
    Subtype[NotStarted],
    Subtype(Ready))
  implicit val IdleOrderJsonCodec: CirceCodec[Order[Idle]] = deriveCirceCodec[Order[Idle]]

  implicit val StateJsonCodec: TypedJsonCodec[State] = TypedJsonCodec(
    Subtype[Idle],
    Subtype(InProcess),
    Subtype(Processed),
    Subtype(Finished))
  implicit val JsonCodec: CirceCodec[Order[State]] = deriveCirceCodec[Order[State]]
}
