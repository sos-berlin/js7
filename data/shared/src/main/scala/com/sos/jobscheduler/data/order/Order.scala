package com.sos.jobscheduler.data.order

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.circeutils.CirceObjectCodec
import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.ScalaUtils.{RichJavaClass, implicitClass}
import com.sos.jobscheduler.base.utils.ScalazStyle.OptionRichBoolean
import com.sos.jobscheduler.data.agent.AgentId
import com.sos.jobscheduler.data.order.Order._
import com.sos.jobscheduler.data.order.OrderEvent._
import com.sos.jobscheduler.data.workflow.WorkflowId
import com.sos.jobscheduler.data.workflow.position.{InstructionNr, Position, WorkflowPosition}
import io.circe.generic.JsonCodec
import scala.collection.immutable.Seq
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
final case class Order[+S <: Order.State](
  id: OrderId,
  workflowPosition: WorkflowPosition,
  state: S,
  attachedTo: Option[AttachedTo] = None,
  parent: Option[OrderId] = None,
  payload: Payload = Payload.empty)
{
  def newForkedOrders(event: OrderForked): Seq[Order[Order.Ready]] =
    for (child ← event.children) yield
      Order(
        child.orderId,
        workflowPosition.copy(position = workflowPosition.position / child.branchId / InstructionNr.First),
        Ready,
        attachedTo,
        parent = Some(id),
        Payload(child.variablesDiff.applyTo(payload.variables)))

  def newPublishedOrder(event: OrderOffered): Order[Offered] = copy(
    event.orderId,
    state = Offered(event.until),
    parent = None)

  def workflowId: WorkflowId =
    workflowPosition.workflowId

  def position: Position =
    workflowPosition.position

  def forceUpdate(event: OrderEvent.OrderCoreEvent): Order[State] =
    update(event).orThrow

  def update(event: OrderEvent.OrderCoreEvent): Checked[Order[State]] = {
    def inapplicable = Invalid(Problem(s"Order '${id.string}' in state '${state.getClass.simpleScalaName}' ($attachedToString) has received an inapplicable event: " + event))
    def if_[A](okay: Boolean)(updated: ⇒ Order[State]) = if (okay && !isDetachable) Valid(updated) else inapplicable
    def ifState[A <: State: ClassTag](updated: ⇒ Order[State]) = if_(isState[A])(updated)

    event match {
      case _: OrderAdded | _: OrderAttached ⇒
        Problem(throw new IllegalArgumentException("OrderAdded and OrderAttached are not handled by the Order itself"))

      case OrderProcessingStarted ⇒
        ifState[Idle](copy(
          state = InProcess))

      case OrderProcessed(diff, outcome_) ⇒
        ifState[InProcess](copy(
          state = Processed(outcome_),
          payload = Payload(diff.applyTo(payload.variables))))

      case OrderForked(children) ⇒
        ifState[Idle](copy(
          state = Forked(children)))

      case OrderJoined(variablesDiff, outcome_) ⇒
        if (isDetachable)
          inapplicable
        else
          state match {
            case _: Forked | _: Awaiting  ⇒
              Valid(copy(
                state = Processed(outcome_),
                payload = Payload(variablesDiff applyTo variables)))
            case _ ⇒
              inapplicable
          }

      case _: OrderOffered ⇒
        ifState[Idle](copy(
          state = Processed(Outcome.succeeded)))

      case OrderAwaiting(orderId) ⇒
        ifState[Idle](copy(
          state = Awaiting(orderId)))

      case OrderMoved(to) ⇒
        ifState[Processed](
          withPosition(to).copy(state = Ready))

      case OrderStopped(message) ⇒
        ifState[Processed](copy(
          state = Stopped(message)))

      case OrderFinished ⇒
        ifState[Idle](
          position.dropChild match {
            case Some(position) ⇒
              copy(workflowPosition = workflowPosition.copy(position = position))
            case None ⇒
              copy(state = Finished)
          })

      case OrderTransferredToAgent(o) ⇒
        if (attachedTo.isEmpty)
          Valid(copy(attachedTo = Some(AttachedTo.Agent(o))))
        else
          inapplicable

      case OrderTransferredToMaster ⇒
        if (detachableFromAgent.isValid)
          Valid(copy(attachedTo = None))
        else
          inapplicable

      case OrderDetachable ⇒
        state match {
          case _: Idle | _: Forked ⇒
            attachedTo match {
              case None ⇒
                inapplicable
              case Some(AttachedTo.Agent(agentId)) ⇒
                Valid(copy(attachedTo = Some(AttachedTo.Detachable(agentId))))
              case Some(AttachedTo.Detachable(_)) ⇒
                inapplicable  // Allow duplicate event?
            }
          case _ ⇒
            inapplicable
        }

      case OrderDetached ⇒
        state match {
          case _: Idle | _: Forked ⇒
            if (!isDetachable)
              inapplicable
            else
              Valid(copy(attachedTo = None))
          case _ ⇒
            inapplicable
        }
    }
  }

  def withInstructionNr(to: InstructionNr): Order[S] =
    withPosition(position.copy(nr = to))

  def withPosition(to: Position): Order[S] = copy(
    workflowPosition = workflowPosition.copy(position = to))

  def variables = payload.variables

  def castAfterEvent(event: OrderProcessingStarted): Order[Order.InProcess] =
    castState[Order.InProcess]

  def castAfterEvent(event: OrderProcessed): Order[Order.Processed] =
    castState[Order.Processed]

  def castState[A <: State: ClassTag]: Order[A] =
    checkedState[A].orThrow

  def checkedState[A <: State: ClassTag]: Checked[Order[A]] =
    Checked.fromOption(ifState[A], Problem(s"'$id' should be in state ${implicitClass[A].simpleScalaName}, but is in state $state"))

  def ifState[A <: State: ClassTag]: Option[Order[A]] =
    isState[A] ? this.asInstanceOf[Order[A]]

  private def isState[A <: State: ClassTag] =
    implicitClass[A] isAssignableFrom state.getClass

  def attachedToString: String =
    attachedTo match {
      case None ⇒ "on Master"
      case Some(AttachedTo.Agent(agentId)) ⇒ s"attached to $agentId"
      case Some(AttachedTo.Detachable(agentId)) ⇒ s"detachable from $agentId"
    }

  def isAttachedToAgent = attachedToAgent.isValid

  def attachedToAgent: Checked[AgentId] =
    attachedTo match {
      case Some(AttachedTo.Agent(agentId)) ⇒
        Valid(agentId)
      case o ⇒
        Invalid(Problem(s"'$id' should be AttachedTo.Agent, but is $o"))
    }

  private def isDetachable: Boolean =
    attachedTo match {
      case Some(_: AttachedTo.Detachable) ⇒ true
      case _ ⇒ false
    }

  def detachableFromAgent: Checked[AgentId] =
    attachedTo match {
      case Some(AttachedTo.Detachable(agentId)) ⇒
        Valid(agentId)
      case o ⇒
        Invalid(Problem(s"'$id' should be AttachedTo.Detachable, but is $o"))
    }
}

object Order {
  def fromOrderAdded(id: OrderId, event: OrderAdded): Order[Idle] =
    Order(id, event.workflowId, Fresh(event.scheduledAt), payload = event.payload)

  def fromOrderAttached(id: OrderId, event: OrderAttached): Order[Idle] =
    Order(id, event.workflowPosition, event.state, Some(AttachedTo.Agent(event.agentId)), payload = event.payload)

  sealed trait AttachedTo
  object AttachedTo {
    sealed trait AgentOrDetachable extends AttachedTo {
      val agentId: AgentId
    }

    @JsonCodec
    final case class Agent(agentId: AgentId) extends AgentOrDetachable

    @JsonCodec
    final case class Detachable(agentId: AgentId) extends AgentOrDetachable

    implicit val jsonCodec = TypedJsonCodec[AttachedTo](
      Subtype[Agent],
      Subtype[Detachable])
  }

  sealed trait State

  sealed trait Idle extends State

  @JsonCodec
  final case class Fresh(scheduledAt: Option[Timestamp] = None) extends Idle
  object Fresh {
    val StartImmediately = Fresh(None)
  }

  sealed trait Started extends State

  sealed trait Transitionable extends Started

  sealed trait Ready extends Started with Idle
  case object Ready extends Ready

  @JsonCodec
  final case class Stopped(outcome: Outcome.NotSucceeded) extends Started //TODO with Idle: Idle auf Ready einengen!

  sealed trait InProcess extends Started
  case object InProcess extends InProcess

  @JsonCodec
  final case class Processed(outcome: Outcome) extends Transitionable

  @JsonCodec
  final case class Forked(children: Seq[Forked.Child]) extends Transitionable {
    def childOrderIds = children map (_.orderId)
  }
  object Forked {
    type Child = OrderForked.Child
    val Child = OrderForked.Child
  }

  @JsonCodec
  final case class Offered(until: Timestamp)
  extends Started

  @JsonCodec
  final case class Awaiting(offeredOrderId: OrderId) extends Transitionable

  sealed trait Finished extends State
  case object Finished extends Finished

  implicit val IdleJsonCodec: TypedJsonCodec[Idle] = TypedJsonCodec[Idle](
    Subtype[Fresh],
    Subtype(Ready))

  implicit val StateJsonCodec: TypedJsonCodec[State] = TypedJsonCodec(
    Subtype[Idle],
    Subtype[Stopped],
    Subtype(InProcess),
    Subtype[Processed],
    Subtype[Forked],
    Subtype[Offered],
    Subtype[Awaiting],
    Subtype(Finished))

  implicit val IdleOrderJsonCodec: CirceObjectCodec[Order[Idle]] = deriveCodec[Order[Idle]]
  implicit val jsonCodec: CirceObjectCodec[Order[State]] = deriveCodec[Order[State]]
}
