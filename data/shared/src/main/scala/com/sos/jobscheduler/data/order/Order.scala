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
  variables: Map[String, String] = Map(),
  outcome: Outcome = InitialOutcome,
  agentPath: Option[AgentPath] = None)
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
        variables = diff.applyTo(variables),
        outcome = outcome_)

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

  def payload = Payload(variables, outcome)

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
  val InitialOutcome = Good(true)

  def fromOrderAdded(id: OrderId, event: OrderAdded): Order[Idle] =
    Order(id, event.nodeKey, event.state, event.variables, event.outcome)

  def fromOrderAttached(id: OrderId, event: OrderAttached): Order[Idle] =
    Order(id, event.nodeKey, event.state, event.variables, event.outcome)

  final case class Payload(variables: Map[String, String], outcome: Order.Outcome = InitialOutcome) {
    override def toString = s"Payload($outcome ${(for (k ← variables.keys.toVector.sorted) yield s"$k=${variables(k)}") mkString ", "}".trim + ")"
  }

  object Good {
    private val False = new Good(false)
    private val True = new Good(true)

    def apply(returnValue: Boolean) = if (returnValue) True else False
  }

  @JsonCodec
  final case class Bad(reason: Bad.Reason) extends Outcome {
    def isSuccess = false
  }

  object Bad {
    def apply(message: String): Bad =
      Bad(Other(message))

    sealed trait Reason {
      def message: String
    }
    final case object AgentAborted extends Reason {
      def message = "Agent aborted while order was InProcess"
    }

    @JsonCodec
    final case class Other(message: String) extends Reason

    object Reason {
      implicit val JsonCodec = TypedJsonCodec[Reason](
        Subtype(AgentAborted),
        Subtype[Other])
    }
  }

  sealed trait Outcome {
    def isSuccess: Boolean
  }

  object Outcome {
    implicit val JsonCodec = TypedJsonCodec[Outcome](
      Subtype[Good],
      Subtype[Bad])
  }

  @JsonCodec
  final case class Good private(returnValue: Boolean) extends Outcome {
    def isSuccess = returnValue
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
  case object Detachable extends Started
  case object Detached extends Started with Idle
  case object Finished extends State

  implicit val IdleJsonCodec: CirceCodec[Idle] = TypedJsonCodec[Idle](
    Subtype(deriveCirceCodec[Scheduled]),
    Subtype(StartNow),
    Subtype(Detached),
    Subtype(Ready))

  implicit val StateJsonCodec: CirceCodec[State] = TypedJsonCodec(
    Subtype[Idle],
    Subtype(InProcess),
    Subtype(Processed),
    Subtype(Detachable),
    Subtype(Finished))

  implicit val JsonCodec: CirceCodec[Order[State]] = deriveCirceCodec[Order[State]]

  implicit val IdleOrderJsonCodec: CirceCodec[Order[Idle]] = deriveCirceCodec[Order[Idle]]
}
