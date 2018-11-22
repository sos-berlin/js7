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
import com.sos.jobscheduler.data.agent.{AgentId, AgentPath}
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
  attachedState: Option[AttachedState] = None,
  parent: Option[OrderId] = None,
  payload: Payload = Payload.empty)
{
  def newForkedOrders(event: OrderForked): Seq[Order[Order.Ready]] =
    for (child ← event.children) yield
      Order(
        child.orderId,
        workflowPosition.copy(position = workflowPosition.position / child.branchId / InstructionNr.First),
        Ready,
        attachedState,
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

    def check[A](okay: Boolean, updated: A) =
      if (okay) Valid(updated) else inapplicable

    event match {
      case _: OrderAdded | _: OrderAttached ⇒
        Invalid(Problem("OrderAdded and OrderAttached are not handled by the Order itself"))

      case OrderStarted ⇒
        check(isState[Fresh] && (isDetached || isAttached),
          copy(state = Ready))

      case OrderProcessingStarted ⇒
        check(isState[Ready] && isAttached,
          copy(state = Processing))

      case OrderProcessed(diff, outcome_) ⇒
        check(isState[Processing] && isAttached,
          copy(
            state = Processed(outcome_),
            payload = Payload(diff.applyTo(payload.variables))))

      case OrderStopped(message) ⇒
        check(isState[Processed] && isAttached,
          copy(state = Stopped(message)))

      case OrderForked(children) ⇒
        check(isState[Ready] && (isDetached || isAttached),
          copy(state = Forked(children)))

      case OrderJoined(variablesDiff, outcome_) ⇒
        check((isState[Forked] || isState[Awaiting]) && isDetached,
          copy(
            state = Processed(outcome_),
            payload = Payload(variablesDiff applyTo variables)))

      case _: OrderOffered ⇒
        check(isState[Ready] && isDetached,
          copy(state = Processed(Outcome.succeeded)))

      case OrderAwaiting(orderId) ⇒
        check(isState[Ready] && isDetached,
          copy(state = Awaiting(orderId)))

      case OrderMoved(to) ⇒
        check(isState[Processed] && (isDetached || isAttached),
          withPosition(to).copy(state = Ready))

      case OrderFinished ⇒
        check(isState[Ready] && isDetached,
          position.dropChild match {
            case Some(position) ⇒ copy(workflowPosition = workflowPosition.copy(position = position))
            case None ⇒ copy(state = Finished)
          })

      case OrderBroken(message) ⇒
        // No state check ???
        Valid(copy(state = Broken(message)))

      case OrderAttachable(agentPath) ⇒
        check(isDetached && (isState[Fresh] || isState[Ready] || isState[Forked]),
          copy(attachedState = Some(Attaching(agentPath))))

      case OrderTransferredToAgent(agentId) ⇒
        check(isAttaching && (isState[Fresh] || isState[Ready] || isState[Forked]),
          copy(attachedState = Some(Attached(agentId))))

      case OrderDetachable ⇒
        attachedState match {
          case Some(Attached(agentId))
            if isState[Ready] || isState[Forked] || isState[Stopped] || isState[Broken] ⇒
              Valid(copy(attachedState = Some(Detaching(agentId))))
          case _ ⇒
            inapplicable
        }

      case OrderDetached ⇒
        check(isDetaching && (isState[Ready] || isState[Forked] || isState[Stopped] || isState[Broken]),
          copy(attachedState = None))

      case OrderTransferredToMaster ⇒
        check(isDetaching && (isState[Ready] || isState[Forked] || isState[Stopped] || isState[Broken]),
          copy(attachedState = None))
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

  def isState[A <: State: ClassTag] =
    implicitClass[A] isAssignableFrom state.getClass

  def attachedToString: String =
    attachedState match {
      case None ⇒ "on Master"
      case Some(Attaching(agentId)) ⇒ s"attachable to $agentId"
      case Some(Attached(agentId)) ⇒ s"attached to $agentId"
      case Some(Detaching(agentId)) ⇒ s"detaching from $agentId"
    }

  /** `true` iff order is going to be attached to an Agent.. */
  def isAttaching: Boolean =
    attachedState exists (_.isInstanceOf[Attaching])

  /** `true` iff order is attached to and processable on an Agent. */
  def isAttached: Boolean =
    attachedState exists (_.isInstanceOf[Attached])

  /** `true` iff order is going to be detached from an Agent. */
  def isDetaching: Boolean =
    attachedState exists (_.isInstanceOf[Detaching])

  /** `true` iff order is processable on Master.. */
  def isDetached: Boolean =
    attachedState.isEmpty

  def attached: Checked[AgentId] =
    attachedState match {
      case Some(Attached(agentId)) ⇒
        Valid(agentId)
      case o ⇒
        Invalid(Problem(s"'$id' should be Agent, but is $o"))
    }

  def detaching: Checked[AgentId] =
    attachedState match {
      case Some(Detaching(agentId)) ⇒
        Valid(agentId)
      case o ⇒
        Invalid(Problem(s"'$id' should be Detaching, but is $o"))
    }
}

object Order {
  def fromOrderAdded(id: OrderId, event: OrderAdded): Order[Fresh] =
    Order(id, event.workflowId, Fresh(event.scheduledAt), payload = event.payload)

  def fromOrderAttached(id: OrderId, event: OrderAttached): Order[FreshOrReady] =
    Order(id, event.workflowPosition, event.state, Some(Attached(event.agentId)), payload = event.payload)

  sealed trait AttachedState
  object AttachedState {
    implicit val jsonCodec = TypedJsonCodec[AttachedState](
      Subtype(deriveCodec[Attaching]),
      Subtype(deriveCodec[Attached]),
      Subtype(deriveCodec[Detaching]))
  }
  sealed trait AgentOrDetachable extends AttachedState {
    val agentId: AgentId
  }
  /** Order is going to be attached to an Agent. */
  final case class Attaching(agentPath: AgentPath) extends AttachedState
  /** Order is attached to an Agent. */
  final case class Attached(agentId: AgentId) extends AgentOrDetachable
  /** Order is going to be detached from Agent. */
  final case class Detaching(agentId: AgentId) extends AgentOrDetachable

  sealed trait State

  sealed trait FreshOrReady extends State

  @JsonCodec
  final case class Fresh(scheduledAt: Option[Timestamp] = None) extends FreshOrReady
  object Fresh {
    val StartImmediately = Fresh(None)
  }

  sealed trait Started extends State

  sealed trait Transitionable extends Started

  sealed trait Ready extends Started with FreshOrReady
  case object Ready extends Ready

  @JsonCodec
  final case class Stopped(outcome: Outcome.NotSucceeded) extends Started

  @JsonCodec
  final case class Broken(problem: Problem) extends Started/*!!!*/

  sealed trait Processing extends Started
  case object Processing extends Processing

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
  final case class Offering(until: Timestamp)
  extends Started

  @JsonCodec
  final case class Awaiting(offeredOrderId: OrderId) extends Transitionable

  sealed trait Finished extends State
  case object Finished extends Finished

  implicit val FreshOrReadyJsonCodec: TypedJsonCodec[FreshOrReady] = TypedJsonCodec[FreshOrReady](
    Subtype[Fresh],
    Subtype(Ready))

  implicit val StateJsonCodec: TypedJsonCodec[State] = TypedJsonCodec(
    Subtype[FreshOrReady],
    Subtype(Processing),
    Subtype[Processed],
    Subtype[Stopped],
    Subtype[Forked],
    Subtype[Offering],
    Subtype[Awaiting],
    Subtype(Finished),
    Subtype[Broken])

  implicit val FreshOrReadyOrderJsonCodec: CirceObjectCodec[Order[FreshOrReady]] = deriveCodec[Order[FreshOrReady]]
  implicit val jsonCodec: CirceObjectCodec[Order[State]] = deriveCodec[Order[State]]
}
