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

  def newPublishedOrder(event: OrderOffered): Order[Offering] = copy(
    event.orderId,
    state = Offering(event.until),
    parent = None)

  def workflowId: WorkflowId =
    workflowPosition.workflowId

  def position: Position =
    workflowPosition.position

  def update(event: OrderEvent.OrderCoreEvent): Checked[Order[State]] = {
    def inapplicable = Invalid(Problem(
      s"Order '${id.string}' in state '${state.getClass.simpleScalaName}' ($attachedToString) has received an inapplicable event: " + event))

    def check[A](okay: Boolean)(updated: A) =
      if (okay) Valid(updated) else inapplicable

    event match {
      case _: OrderAdded | _: OrderAttached ⇒
        Invalid(Problem("OrderAdded and OrderAttached are not handled by the Order itself"))

      case OrderProcessingStarted ⇒
        check(isState[Idle] && isAttachedToAgent)(copy(
          state = Processing))

      case OrderProcessed(diff, outcome_) ⇒
        check(isState[Processing] && isAttachedToAgent)(copy(
          state = Processed(outcome_),
          payload = Payload(diff.applyTo(payload.variables))))

      case OrderStopped(message) ⇒
        check(isState[Processed] && isAttachedToAgent)(copy(
          state = Stopped(message)))

      case OrderForked(children) ⇒
        check(isState[Idle] && (isOnMaster || isAttachedToAgent))(copy(
          state = Forked(children)))

      case OrderJoined(variablesDiff, outcome_) ⇒
        check((isState[Forked] || isState[Awaiting]) && isOnMaster)(copy(
          state = Processed(outcome_),
          payload = Payload(variablesDiff applyTo variables)))

      case _: OrderOffered ⇒
        check(isState[Idle] && isOnMaster)(copy(
          state = Processed(Outcome.succeeded)))

      case OrderAwaiting(orderId) ⇒
        check(isState[Idle] && isOnMaster)(copy(
          state = Awaiting(orderId)))

      case OrderMoved(to) ⇒
        check(isState[Processed] && (isOnMaster || isAttachedToAgent))(
          withPosition(to).copy(state = Ready))

      case OrderBroken(message) ⇒
        // No state check ???
        Valid(copy(
          state = Broken(message)))

      case OrderFinished ⇒
        check(isState[Idle] && isOnMaster)(
          position.dropChild match {
            case Some(position) ⇒ copy(
              workflowPosition = workflowPosition.copy(position = position))
            case None ⇒ copy(
              state = Finished)
          })

      case OrderTransferredToAgent(o) ⇒
        check(isState[Attachable] && isOnMaster)(copy(
          attachedTo = Some(AttachedTo.Agent(o))))

      case OrderTransferredToMaster ⇒
        check(isState[Detachable] && isDetachable)(copy(
          attachedTo = None))

      case OrderDetachable ⇒
        attachedTo match {
          case Some(AttachedTo.Agent(agentId)) if isState[Detachable] ⇒
            Valid(copy(
              attachedTo = Some(AttachedTo.Detachable(agentId))))
          case _ ⇒
            inapplicable
        }

      case OrderDetached ⇒
        check(isState[Detachable] && isDetachable)(copy(
          attachedTo = None))
    }
  }

  def withInstructionNr(to: InstructionNr): Order[S] =
    withPosition(position.copy(nr = to))

  def withPosition(to: Position): Order[S] = copy(
    workflowPosition = workflowPosition.copy(position = to))

  def variables = payload.variables

  def castAfterEvent(event: OrderProcessingStarted): Order[Order.Processing] =
    castState[Order.Processing]

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

  def isOnMaster: Boolean =
    attachedTo.isEmpty

  def isAttachedToAgent: Boolean =
    attachedTo exists (_.isInstanceOf[AttachedTo.Agent])

  def attachedToAgent: Checked[AgentId] =
    attachedTo match {
      case Some(AttachedTo.Agent(agentId)) ⇒
        Valid(agentId)
      case o ⇒
        Invalid(Problem(s"'$id' should be AttachedTo.Agent, but is $o"))
    }

  private def isDetachable: Boolean =
    attachedTo exists (_.isInstanceOf[AttachedTo.Detachable])

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

  sealed trait Attachable extends State

  sealed trait Detachable extends State

  sealed trait Idle extends State with Attachable with Detachable

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
  final case class Stopped(outcome: Outcome.NotSucceeded) extends Started with Detachable //TODO with Idle: Idle auf Ready einengen!

  @JsonCodec
  final case class Broken(problem: Problem) extends Detachable

  sealed trait Processing extends Started
  case object Processing extends Processing

  @JsonCodec
  final case class Processed(outcome: Outcome) extends Transitionable

  @JsonCodec
  final case class Forked(children: Seq[Forked.Child]) extends Transitionable with Attachable with Detachable {
    def childOrderIds = children map (_.orderId)
  }
  object Forked {
    type Child = OrderForked.Child
    val Child = OrderForked.Child
  }

  @JsonCodec
  final case class Offering(until: Timestamp)
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
    Subtype(Processing),
    Subtype[Processed],
    Subtype[Stopped],
    Subtype[Forked],
    Subtype[Offering],
    Subtype[Awaiting],
    Subtype(Finished),
    Subtype[Broken])

  implicit val IdleOrderJsonCodec: CirceObjectCodec[Order[Idle]] = deriveCodec[Order[Idle]]
  implicit val jsonCodec: CirceObjectCodec[Order[State]] = deriveCodec[Order[State]]
}
