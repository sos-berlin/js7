package com.sos.jobscheduler.data.order

import com.sos.jobscheduler.base.circeutils.CirceCodec
import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCirceCodec
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.ScalaUtils.implicitClass
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
  agentPath: Option[AgentPath] = None,
  payload: Payload = Payload.empty)
{
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
        agentPath = Some(o))

      case OrderMovedToMaster ⇒ copy(
        state = Detached,
        agentPath = None)

      case OrderProcessingStarted ⇒ copy(
        state = InProcess)

      case OrderProcessed(diff, outcome_) ⇒ copy(
        state = Processed,
        payload = Payload(variables = diff.applyTo(variables), outcome = outcome_))

      case OrderTransitioned(toNodeId) ⇒ copy(
        state = Ready,
        nodeKey = NodeKey(workflowPath, toNodeId))

      case OrderDetachable ⇒ copy(
        state = Detachable)

      case OrderDetached ⇒ copy(
        state = Detached)

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
    if (!implicitClass[T].isAssignableFrom(state.getClass))
      throw new ClassCastException(s"Order '$id': Order[${state.getClass.getSimpleName stripSuffix "$"}] cannot be cast to Order[${implicitClass[T].getSimpleName stripSuffix "$"}]")
    this.asInstanceOf[Order[T]]
  }
}

object Order {
  def fromOrderAdded(id: OrderId, event: OrderAdded): Order[Idle] =
    Order(id, event.nodeKey, event.state, payload = event.payload)

  def fromOrderAttached(id: OrderId, event: OrderAttached): Order[Idle] =
    Order(id, event.nodeKey, event.state, payload = event.payload)

  sealed trait State
  sealed trait Idle extends State
  sealed trait NotStarted extends Idle
  sealed trait Started extends State
  final case class Scheduled(at: Timestamp) extends NotStarted
  final case object StartNow extends NotStarted
  case object Ready extends Started with Idle
  case object InProcess extends Started
  case object Processed extends Started
  case object Detachable extends Started
  case object Detached extends Started with Idle
  case object Finished extends State

  implicit val NotStartedJsonCodec: TypedJsonCodec[NotStarted] = TypedJsonCodec[NotStarted](
    Subtype(deriveCirceCodec[Scheduled]),
    Subtype(StartNow))
  implicit val NotStartedOrderJsonCodec: CirceCodec[Order[NotStarted]] = deriveCirceCodec[Order[NotStarted]]

  implicit val IdleJsonCodec: TypedJsonCodec[Idle] = TypedJsonCodec[Idle](
    Subtype[NotStarted],
    Subtype(Ready),
    Subtype(Detached))
  implicit val IdleOrderJsonCodec: CirceCodec[Order[Idle]] = deriveCirceCodec[Order[Idle]]

  implicit val StateJsonCodec: TypedJsonCodec[State] = TypedJsonCodec(
    Subtype[Idle],
    Subtype(InProcess),
    Subtype(Processed),
    Subtype(Detachable),
    Subtype(Finished))
  implicit val JsonCodec: CirceCodec[Order[State]] = deriveCirceCodec[Order[State]]
}
