package com.sos.jobscheduler.data.order

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.ScalaUtils.{RichJavaClass, implicitClass}
import com.sos.jobscheduler.base.utils.ScalazStyle.OptionRichBoolean
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.command.CancelMode
import com.sos.jobscheduler.data.order.Order._
import com.sos.jobscheduler.data.order.OrderEvent._
import com.sos.jobscheduler.data.workflow.WorkflowId
import com.sos.jobscheduler.data.workflow.position.{InstructionNr, Position, WorkflowPosition}
import io.circe.generic.JsonCodec
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, DecodingFailure, JsonObject, ObjectEncoder}
import scala.collection.immutable.Seq
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
final case class Order[+S <: Order.State](
  id: OrderId,
  workflowPosition: WorkflowPosition,
  state: S,
  arguments: Map[String, String] = Map.empty,
  historicOutcomes: Seq[HistoricOutcome] = Nil,
  attachedState: Option[AttachedState] = None,
  parent: Option[OrderId] = None,
  cancel: Option[CancelMode] = None)
{
  def newForkedOrders(event: OrderForked): Seq[Order[Order.Ready]] =
    for (child <- event.children) yield
      Order(
        child.orderId,
        workflowPosition.copy(position = workflowPosition.position / child.branchId.toBranchId % InstructionNr.First),
        Ready,
        arguments,
        historicOutcomes,
        attachedState,
        parent = Some(id))

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
      s"Order '${id.string}' at position '$workflowPosition' in state '${state.getClass.simpleScalaName}' ($attachedStateString) has received an inapplicable event: $event"))

    def check[A](okay: Boolean, updated: A) =
      if (okay) Valid(updated) else inapplicable

    event match {
      case _: OrderAdded | _: OrderAttached =>
        Invalid(Problem("OrderAdded and OrderAttached are not handled by the Order itself"))

      case OrderStarted =>
        check(isState[Fresh] && (isDetached || isAttached),
          copy(state = Ready))

      case OrderProcessingStarted =>
        check(isState[Ready] && isAttached,
          copy(state = Processing))

      case OrderProcessed(outcome_) =>
        check(isState[Processing] && isAttached,
          copy(
            state = Processed,
            historicOutcomes = historicOutcomes :+ HistoricOutcome(position, outcome_)))

      case OrderFailed(outcome_) =>
        check(isState[Ready] && isDetached,
          copy(
            state = Failed(outcome_),
            historicOutcomes = historicOutcomes :+ HistoricOutcome(position, outcome_)))

      case OrderFailedInFork(outcome_) =>
        check(isState[Ready] && (isDetached || isAttached),
          copy(
            state = FailedInFork(outcome_),
            historicOutcomes = historicOutcomes :+ HistoricOutcome(position, outcome_)))

      case OrderFailedCatchable(_) =>
        inapplicable

      case OrderStopped(outcome_) =>
        check(isOrderStoppedApplicable,
          copy(
            state = if (isState[Fresh]) StoppedWhileFresh else Stopped,
            historicOutcomes = historicOutcomes :+ HistoricOutcome(position, outcome_)))

      case OrderCatched(outcome_, movedTo) =>
        check(isState[Ready] && (isDetached || isAttached)  ||  isState[Processed] && isAttached,
          copy(
            state = Ready,
            workflowPosition = workflowPosition.copy(position = movedTo),
            historicOutcomes = historicOutcomes :+ HistoricOutcome(position, outcome_)))

      case OrderRetrying(to, maybeDelayUntil) =>
        check(isState[Ready] && (isDetached || isAttached),
          maybeDelayUntil.fold[Order[State]](this/*Ready*/)(o => copy(state = DelayedAfterError(o)))
            .withPosition(to))

      case OrderAwoke =>
        check(isState[DelayedAfterError] && isAttached,
          copy(state = Ready))

      case OrderForked(children) =>
        check(isState[Ready] && (isDetached || isAttached),
          copy(state = Forked(children)))

      case OrderJoined(outcome_) =>
        check((isState[Forked] || isState[Awaiting]) && isDetached,
          copy(
            state = Processed,
            historicOutcomes = historicOutcomes :+ HistoricOutcome(position, outcome_)))

      case _: OrderOffered =>
        check(isState[Ready] && isDetached,
          copy(
            state = Processed,
            historicOutcomes = historicOutcomes :+ HistoricOutcome(position, Outcome.succeeded)))

      case OrderAwaiting(orderId) =>
        check(isState[Ready] && isDetached,
          copy(state = Awaiting(orderId)))

      case OrderMoved(to) =>
        check((isState[FreshOrReady]/*before TryInstruction*/ || isState[Processed]) && (isDetached || isAttached),
          withPosition(to).copy(
            state = if (isState[Fresh]) state else Ready))

      case OrderFinished =>
        check(isState[Ready] && isDetached,
          position.dropChild match {
            case Some(position) => copy(workflowPosition = workflowPosition.copy(position = position))
            case None => copy(state = Finished)
          })

      case OrderBroken(message) =>
        check(!isDeleted,
          copy(state = Broken(message)))

      case OrderAttachable(agentRefPath) =>
        check(isDetached && (isState[Fresh] || isState[Ready] || isState[Forked]),
          copy(attachedState = Some(Attaching(agentRefPath))))

      case OrderTransferredToAgent(agentRefPath) =>
        check(isAttaching && (isState[Fresh] || isState[Ready] || isState[Forked]),
          copy(attachedState = Some(Attached(agentRefPath))))

      case OrderDetachable =>
        attachedState match {
          case Some(Attached(agentRefPath))
            if isState[Fresh] || isState[Ready] || isState[Forked] ||
               isState[StoppedWhileFresh] || isState[Stopped] || isState[FailedInFork] || isState[Broken] =>
              Valid(copy(attachedState = Some(Detaching(agentRefPath))))
          case _ =>
            inapplicable
        }

      case OrderDetached =>
        check(isDetaching && (isState[Fresh] || isState[Ready] || isState[Forked] ||
                              isState[StoppedWhileFresh] || isState[Stopped] || isState[FailedInFork] || isState[Broken]),
          copy(attachedState = None))

      case OrderTransferredToMaster =>
        check(isDetaching && (isState[Fresh] || isState[Ready] || isState[Forked] ||
                              isState[StoppedWhileFresh] || isState[Stopped] || isState[FailedInFork] || isState[Broken]),
          copy(attachedState = None))

      case OrderCancelationMarked(mode) =>
        check(!isState[Canceled] && !isDetaching && !isState[Finished],
          copy(cancel = Some(mode)))

      case OrderCanceled =>
        check((isState[FreshOrReady] || isState[DelayedAfterError] || isState[StoppedWhileFresh] || isState[Stopped] || isState[Broken])
            && isDetached,
          copy(state = Canceled))
    }
  }

  def isDeleted = state match {
    case _: Finished | _: Canceled | _: Failed => true
    case _ => false
  }

  def isOrderStoppedApplicable =
    isState[FreshOrReady] && (isDetached || isAttached)  ||
    isState[Processed] && isAttached

  def withInstructionNr(to: InstructionNr): Order[S] =
    withPosition(position.copy(nr = to))

  def withPosition(to: Position): Order[S] = copy(
    workflowPosition = workflowPosition.copy(position = to))

  def lastOutcome: Outcome =
    historicOutcomes.lastOption.map(_.outcome) getOrElse Outcome.succeeded

  def keyValues: Map[String, String] = historicOutcomes
    .collect { case HistoricOutcome(_, o: Outcome.Undisrupted) => o.keyValues }
    .fold(arguments)((a, b) => a ++ b)

  def isStarted = isState[Started]

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

  def attachedStateString: String =
    attachedState match {
      case None => "on Master"
      case Some(Attaching(agentRefPath)) => s"attachable to $agentRefPath"
      case Some(Attached(agentRefPath)) => s"attached to $agentRefPath"
      case Some(Detaching(agentRefPath)) => s"detaching from $agentRefPath"
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

  def attached: Checked[AgentRefPath] =
    attachedState match {
      case Some(Attached(agentRefPath)) =>
        Valid(agentRefPath)
      case o =>
        Invalid(Problem(s"'$id' should be 'Attached', but is $o"))
    }

  def detaching: Checked[AgentRefPath] =
    attachedState match {
      case Some(Detaching(agentRefPath)) =>
        Valid(agentRefPath)
      case o =>
        Invalid(Problem(s"'$id' should be Detaching, but is $o"))
    }
}

object Order {
  def fromOrderAdded(id: OrderId, event: OrderAdded): Order[Fresh] = {
    Order(id, event.workflowId, Fresh(event.scheduledFor), event.arguments)
  }

  def fromOrderAttached(id: OrderId, event: OrderAttached): Order[FreshOrReady] =
    Order(id, event.workflowPosition, event.state, event.arguments, event.historicOutcomes, Some(Attached(event.agentRefPath)))

  sealed trait AttachedState
  object AttachedState {
    sealed trait HasAgentRefPath extends AttachedState {
      def agentRefPath: AgentRefPath
    }
    implicit val jsonCodec = TypedJsonCodec[AttachedState](
      Subtype(deriveCodec[Attaching]),
      Subtype(deriveCodec[Attached]),
      Subtype(deriveCodec[Detaching]))
  }
  /** Order is going to be attached to an Agent. */
  final case class Attaching(agentRefPath: AgentRefPath) extends AttachedState.HasAgentRefPath {
    override def toString = s"Attaching(${agentRefPath.string})"
  }
  /** Order is attached to an Agent. */
  final case class Attached(agentRefPath: AgentRefPath) extends AttachedState.HasAgentRefPath {
    override def toString = s"Attached(${agentRefPath.string})"
  }
  /** Order is going to be detached from Agent. */
  final case class Detaching(agentRefPath: AgentRefPath) extends AttachedState.HasAgentRefPath {
    override def toString = s"Detaching(${agentRefPath.string})"
  }

  sealed trait State {
    def maybeDelayedUntil: Option[Timestamp] = None
  }

  sealed trait FreshOrReady extends State

  sealed trait DelayedUntil {
    def delayedUntil: Timestamp
  }

  @JsonCodec
  final case class Fresh(scheduledFor: Option[Timestamp] = None) extends FreshOrReady {
    override def maybeDelayedUntil = scheduledFor
  }
  object Fresh {
    val StartImmediately = Fresh(None)
  }

  sealed trait Started extends State

  sealed trait Ready extends Started with FreshOrReady
  case object Ready extends Ready

  @JsonCodec
  final case class DelayedAfterError(until: Timestamp) extends Started {
    override def maybeDelayedUntil = Some(until)
  }

  sealed trait StoppedWhileFresh extends Started
  case object StoppedWhileFresh extends StoppedWhileFresh

  sealed trait Stopped extends Started
  case object Stopped extends Stopped

  @JsonCodec
  final case class Broken(problem: Problem) extends Started/*!!!*/

  sealed trait Processing extends Started
  case object Processing extends Processing

  sealed trait Processed extends Started
  case object Processed extends Processed

  @JsonCodec
  final case class Forked(children: Seq[Forked.Child]) extends Started {
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
  final case class Awaiting(offeredOrderId: OrderId) extends Started

  @JsonCodec
  final case class Failed(outcome: Outcome.NotSucceeded) extends Finished

  @JsonCodec
  final case class FailedInFork(outcome: Outcome.NotSucceeded) extends Finished

  sealed trait Finished extends Started
  case object Finished extends Finished

  sealed trait Canceled extends State
  case object Canceled extends Canceled

  implicit val FreshOrReadyJsonCodec: TypedJsonCodec[FreshOrReady] = TypedJsonCodec[FreshOrReady](
    Subtype[Fresh],
    Subtype(Ready))

  implicit val StateJsonCodec: TypedJsonCodec[State] = TypedJsonCodec(
    Subtype[FreshOrReady],
    Subtype(Processing),
    Subtype(Processed),
    Subtype[DelayedAfterError],
    Subtype(StoppedWhileFresh),
    Subtype(Stopped),
    Subtype[Forked],
    Subtype[Offering],
    Subtype[Awaiting],
    Subtype[Failed],
    Subtype[FailedInFork],
    Subtype(Finished),
    Subtype(Canceled),
    Subtype[Broken])

  implicit val jsonEncoder: ObjectEncoder[Order[State]] = order =>
    JsonObject(
      "id" -> order.id.asJson,
      "arguments" -> (order.arguments.nonEmpty ? order.arguments).asJson,
      "workflowPosition" -> order.workflowPosition.asJson,
      "state" -> order.state.asJson,
      "attachedState" -> order.attachedState.asJson,
      "parent" -> order.parent.asJson,
      "historicOutcomes" -> order.historicOutcomes.asJson,
      "cancel" -> order.cancel.asJson)

  implicit val jsonDecoder: Decoder[Order[State]] = cursor =>
    for {
      id <- cursor.get[OrderId]("id")
      arguments <- cursor.get[Option[Map[String, String]]]("arguments") map (_ getOrElse Map.empty)
      workflowPosition <- cursor.get[WorkflowPosition]("workflowPosition")
      state <- cursor.get[State]("state")
      attachedState <- cursor.get[Option[AttachedState]]("attachedState")
      parent <- cursor.get[Option[OrderId]]("parent")
      historicOutcomes <- cursor.get[Seq[HistoricOutcome]]("historicOutcomes")
      cancel <- cursor.get[Option[CancelMode]]("cancel")
    } yield
      Order(id, workflowPosition, state, arguments, historicOutcomes, attachedState, parent, cancel)

  implicit val FreshOrReadyOrderJsonEncoder: ObjectEncoder[Order[FreshOrReady]] = o => jsonEncoder.encodeObject(o)
  implicit val FreshOrReadyOrderJsonDecoder: Decoder[Order[FreshOrReady]] = cursor =>
    jsonDecoder(cursor) flatMap {
      o => o.ifState[FreshOrReady] match {
        case None => Left(DecodingFailure(s"Order is not Fresh or Ready, but: ${o.state.getClass.simpleScalaName}", Nil))
        case Some(x) => Right(x)
      }
    }
}
