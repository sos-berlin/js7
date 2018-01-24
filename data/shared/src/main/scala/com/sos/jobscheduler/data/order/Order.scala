package com.sos.jobscheduler.data.order

import com.sos.jobscheduler.base.circeutils.CirceCodec
import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCirceCodec
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.ScalaUtils.{RichJavaClass, implicitClass}
import com.sos.jobscheduler.base.utils.ScalazStyle.OptionRichBoolean
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.order.Order._
import com.sos.jobscheduler.data.order.OrderEvent._
import com.sos.jobscheduler.data.workflow.{InstructionNr, Position, WorkflowPath, WorkflowPosition}
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
  payload: Payload = Payload.empty,
  parent: Option[OrderId] = None)
{
  def newForkedOrders(event: OrderForked): Seq[Order[Order.Ready]] =
    for (child ← event.children) yield
      Order(child.orderId, workflowPosition.copy(position = workflowPosition.position / child.branchId / InstructionNr.First), Ready, attachedTo,
        Payload(child.variablesDiff.applyTo(payload.variables)),
        parent = Some(id))

  def newPublishedOrder(event: OrderOffered): Order[Offered] = copy(
    event.orderId,
    state = Offered(event.until),
    parent = None)

  def workflowPath: WorkflowPath =
    workflowPosition.workflowPath

  def position: Position =
    workflowPosition.position

  def update(event: OrderEvent.OrderCoreEvent): Order[State] =
    event match {
      case OrderTransferredToAgent(o) ⇒
        copy(
          attachedTo = Some(AttachedTo.Agent(o)))

      case OrderTransferredToMaster ⇒ copy(
        attachedTo = None)

      case OrderProcessingStarted ⇒ copy(
        state = InProcess)

      case OrderProcessed(diff, outcome_) ⇒ copy(
        state = Processed,
        payload = Payload(
          variables = diff.applyTo(payload.variables),
          outcome = outcome_))

      case OrderForked(children) ⇒ copy(
        state = Join(children map (_.orderId)))

      case OrderJoined(variablesDiff, outcome_) ⇒
        copy(
          state = Processed,
          payload = Payload(variablesDiff applyTo variables, outcome_))

      case _: OrderOffered ⇒ copy(
        state = Processed)

      case OrderAwaiting(orderId) ⇒ copy(
        state = Awaiting(orderId))

      case OrderMoved(to) ⇒
        withPosition(to).copy(state = Ready)

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

      case OrderFinished ⇒
        position.dropChild match {
          case Some(position) ⇒
            copy(workflowPosition = workflowPosition.copy(position = position))
          case None ⇒
            copy(state = Finished)
        }
    }

  def withInstructionNr(to: InstructionNr): Order[S] =
    withPosition(position.copy(nr = to))

  def withPosition(to: Position): Order[S] = copy(
    workflowPosition = workflowPosition.copy(position = to))

  def variables = payload.variables

  def outcome = payload.outcome

  def castAfterEvent(event: OrderProcessingStarted): Order[Order.InProcess] =
    castState[Order.InProcess]

  def castAfterEvent(event: OrderProcessed): Order[Order.Processed] =
    castState[Order.Processed]

  def castState[A <: State: ClassTag]: Order[A] =
    ifState[A] getOrElse (
      throw new IllegalStateException(s"'$id' is expected to be ${implicitClass[A].simpleScalaName}, but is $state"))

  def ifState[A <: State: ClassTag]: Option[Order[A]] = {
    val cls = implicitClass[A]
    cls.isAssignableFrom(state.getClass) option
      this.asInstanceOf[Order[A]]
  }

  def isAttachedToAgent = attachedToAgent.isRight

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
    Order(id, event.workflowPath, event.state, None, event.payload)

  def fromOrderAttached(id: OrderId, event: OrderAttached): Order[Idle] =
    Order(id, event.workflowPosition, event.state, Some(AttachedTo.Agent(event.agentPath)), event.payload)

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

    implicit val jsonCodec = TypedJsonCodec[AttachedTo](
      Subtype[Agent],
      Subtype[Detachable])
  }

  sealed trait State
  sealed trait Idle extends State
  sealed trait NotStarted extends Idle
  sealed trait Started extends State
  sealed trait Transitionable extends Started

  @JsonCodec
  final case class Scheduled(at: Timestamp) extends NotStarted

  final case object StartNow extends NotStarted

  sealed trait Ready extends Started with Idle
  case object Ready extends Ready

  sealed trait InProcess extends Started
  case object InProcess extends InProcess

  sealed trait Processed extends Transitionable
  case object Processed extends Processed

  @JsonCodec
  final case class Join(joinOrderIds: Seq[OrderId]) extends Transitionable

  @JsonCodec
  final case class Offered(until: Timestamp)
  extends Started

  @JsonCodec
  final case class Awaiting(offeredOrderId: OrderId) extends Transitionable

  sealed trait Finished extends State
  case object Finished extends Finished

  implicit val NotStartedJsonCodec: TypedJsonCodec[NotStarted] = TypedJsonCodec[NotStarted](
    Subtype[Scheduled],
    Subtype(StartNow))

  implicit val IdleJsonCodec: TypedJsonCodec[Idle] = TypedJsonCodec[Idle](
    Subtype[NotStarted],
    Subtype(Ready))

  implicit val StateJsonCodec: TypedJsonCodec[State] = TypedJsonCodec(
    Subtype[Idle],
    Subtype(InProcess),
    Subtype(Processed),
    Subtype[Join],
    Subtype[Offered],
    Subtype[Awaiting],
    Subtype(Finished))

  implicit val NotStartedOrderJsonCodec: CirceCodec[Order[NotStarted]] = deriveCirceCodec[Order[NotStarted]]
  implicit val IdleOrderJsonCodec: CirceCodec[Order[Idle]] = deriveCirceCodec[Order[Idle]]
  implicit val jsonCodec: CirceCodec[Order[State]] = deriveCirceCodec[Order[State]]
}
