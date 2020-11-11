package js7.data.order

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, DecodingFailure, Encoder, JsonObject}
import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.problem.Checked.Ops
import js7.base.problem.{Checked, Problem}
import js7.base.time.Timestamp
import js7.base.utils.ScalaUtils._
import js7.base.utils.ScalaUtils.syntax._
import js7.data.agent.AgentName
import js7.data.command.{CancelMode, SuspendMode}
import js7.data.order.Order._
import js7.data.order.OrderEvent._
import js7.data.workflow.WorkflowId
import js7.data.workflow.position.{InstructionNr, Position, WorkflowPosition}
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
  mark: Option[OrderMark] = None,
  isSuspended: Boolean = false,
  removeWhenTerminated: Boolean = false)
{
  def newForkedOrders(event: OrderForked): Seq[Order[Ready]] =
    for (child <- event.children) yield
      Order(
        child.orderId,
        workflowPosition.copy(position = workflowPosition.position / child.branchId.toBranchId % InstructionNr.First),
        Ready,
        arguments,
        historicOutcomes,
        attachedState,
        parent = Some(id))

  def newOfferedOrder(event: OrderOffered): Order[Offering] = copy(
    event.orderId,
    state = Offering(event.until),
    parent = None)

  def workflowId: WorkflowId =
    workflowPosition.workflowId

  def position: Position =
    workflowPosition.position

  def forkPosition: Checked[Position] = {
    val reversed = position.forkBranchReversed
    if (reversed.isEmpty)
      Left(Problem.pure(s"Order '${id.string}' is in state FailedInFork but not below a Fork instruction"))
    else
      Right(reversed.tail.reverse % reversed.head.nr)
  }

  def update(event: OrderEvent.OrderCoreEvent): Checked[Order[State]] = {
    def inapplicable = Left(Problem(
      s"Order '${id.string}' at position '$workflowPosition' in state '${state.getClass.simpleScalaName}' (${markString.fold("")(o => s"$o, ")}$attachedStateString) has received an inapplicable event: $event"))

    def check[A](okay: Boolean, updated: A) =
      if (okay) Right(updated) else inapplicable

    if (isState[Removed])
      inapplicable
    else
      event match {
        case _: OrderAdded | _: OrderAttachedToAgent =>
          Left(Problem("OrderAdded and OrderAttachedToAgent events are not handled by the Order itself"))

        case OrderStarted =>
          check(isState[Fresh] && !isSuspended && (isDetached || isAttached),
            copy(state = Ready))

        case OrderProcessingStarted =>
          check(isState[Ready] && !isSuspended && isAttached,
            copy(
              state = Processing,
              mark = cleanMark))

        case OrderProcessed(outcome_) =>
          check(isState[Processing] && !isSuspended && isAttached,
            copy(
              state = Processed,
              historicOutcomes = historicOutcomes :+ HistoricOutcome(position, outcome_)))

        case OrderProcessingKilled =>
          check(isState[Processed] && !isSuspended && isAttached,
            copy(state = ProcessingKilled))

        case OrderFailed(outcome_) =>
          check(isOrderFailedApplicable,
            copy(
              state = if (isState[Fresh]) FailedWhileFresh else Failed(outcome_),
              historicOutcomes = historicOutcomes :+ HistoricOutcome(position, outcome_)))

        case OrderFailedInFork(outcome_) =>
          check((isState[Ready] || isState[Processed]) && !isSuspended && (isDetached || isAttached),
            copy(
              state = FailedInFork(outcome_),
              historicOutcomes = historicOutcomes :+ HistoricOutcome(position, outcome_)))

        case OrderFailedCatchable(_) =>
          inapplicable

        case OrderCatched(outcome_, movedTo) =>
          check((isState[Ready] || isState[Processed]) && !isSuspended && (isAttached | isDetached),
            copy(
              state = Ready,
              workflowPosition = workflowPosition.copy(position = movedTo),
              historicOutcomes = historicOutcomes :+ HistoricOutcome(position, outcome_)))

        case OrderRetrying(to, maybeDelayUntil) =>
          check(isState[Ready] && !isSuspended && (isDetached || isAttached),
            maybeDelayUntil.fold[Order[State]](this/*Ready*/)(o => copy(state = DelayedAfterError(o)))
              .withPosition(to))

        case OrderAwoke =>
          check(isState[DelayedAfterError] && !isSuspended && isAttached,
            copy(state = Ready))

        case OrderForked(children) =>
          check(isState[Ready] && !isSuspended && (isDetached || isAttached),
            copy(
              state = Forked(children),
              mark = cleanMark))

        case OrderJoined(outcome_) =>
          check((isState[Forked] || isState[Awaiting]) && !isSuspended && isDetached,
            copy(
              state = Processed,
              historicOutcomes = historicOutcomes :+ HistoricOutcome(position, outcome_)))

        case _: OrderOffered =>
          check(isState[Ready] && !isSuspended && isDetached,
            copy(
              state = Processed,
              historicOutcomes = historicOutcomes :+ HistoricOutcome(position, Outcome.succeeded)))

        case OrderAwaiting(orderId) =>
          check(isState[Ready] && !isSuspended && isDetached,
            copy(
              state = Awaiting(orderId),
              mark = cleanMark))

        case OrderMoved(to) =>
          check((isState[IsFreshOrReady]/*before TryInstruction*/ || isState[Processed]) &&
            (isDetached || isAttached),
            withPosition(to).copy(
              state = if (isState[Fresh]) state else Ready))

        case OrderFinished =>
          check(isState[Ready] && !isSuspended && isDetached,
            position.dropChild match {
              case Some(position) =>
                copy(workflowPosition = workflowPosition.copy(position = position))
              case None =>
                copy(
                  state = Finished,
                  mark = None)
            })

        case OrderRemoveMarked =>
          check(!isState[IsTerminated] && parent.isEmpty,
            copy(removeWhenTerminated = true))

        case OrderRemoved =>
          check(isState[IsTerminated] && isDetached && parent.isEmpty,
            copy(state = Removed))

        case OrderBroken(message) =>
          check(!isState[IsTerminated],
            copy(state = Broken(message)))

        case OrderAttachable(agentName) =>
          check((isState[Fresh] || isState[Ready] || isState[Forked]) && isDetached,
            copy(attachedState = Some(Attaching(agentName))))

        case OrderAttached(agentName) =>
          attachedState match {
            case Some(Attaching(`agentName`)) =>
              check((isState[Fresh] || isState[Ready] || isState[Forked]) && isAttaching,
                copy(attachedState = Some(Attached(agentName))))
            case _ => inapplicable
          }

        case OrderDetachable =>
          attachedState match {
            case Some(Attached(agentName))
              if isState[Fresh] || isState[Ready] || isState[Forked] || isState[ProcessingKilled] ||
                 isState[FailedWhileFresh] || isState[Failed] || isState[FailedInFork] || isState[Broken] =>
                Right(copy(attachedState = Some(Detaching(agentName))))
            case _ =>
              inapplicable
          }

        case OrderDetached =>
          check(isDetaching && (isState[Fresh] || isState[Ready] || isState[Forked] || isState[ProcessingKilled] ||
                                isState[FailedWhileFresh] || isState[Failed] || isState[FailedInFork] || isState[Broken]),
            copy(attachedState = None))

        case OrderCancelMarked(mode) =>
          check(parent.isEmpty && isMarkable,
            copy(mark = Some(OrderMark.Cancelling(mode))))

        case OrderCancelled =>
          check(isCancelable && isDetached,
            copy(
              state = Cancelled,
              mark = None))

        case OrderSuspendMarked(kill) =>
          check(isMarkable,
            copy(mark = Some(OrderMark.Suspending(kill))))

        case OrderSuspended =>
          check(isSuspendible && (isDetached || isSuspended/*already Suspended, to clean Resuming mark*/),
            copy(
              isSuspended = true,
              mark = None,
              state = if (isSuspendingWithKill && isState[ProcessingKilled]) Ready else state))

        case OrderResumeMarked(position) =>
          if (isMarkable)
            if (isSuspended)
              Right(copy(mark = Some(OrderMark.Resuming(position))))
            else position match {
              case Some(_) =>
                // Inhibited because we cannot be sure weather order will pass a fork barrier
                inapplicable
              case None =>
                if (!isSuspended && isSuspending)
                  Right(copy(mark = None/*revert OrderSuspendMarked*/))
                else
                  Right(copy(mark = Some(OrderMark.Resuming(None))))
            }
          else
            inapplicable

        case OrderResumed(maybePosition) =>
          check(isResumable,
            copy(
              isSuspended = false,
              mark = None,
              state = if (isState[Broken]) Ready else state
            ).withPosition(maybePosition getOrElse position))
      }
  }

  def isOrderFailedApplicable =
    !isSuspended &&
      (isDetached || isAttached) &&
      (isState[IsFreshOrReady] || isState[Processed])

  def withInstructionNr(to: InstructionNr): Order[S] =
    withPosition(position.copy(nr = to))

  def withPosition(to: Position): Order[S] = copy(
    workflowPosition = workflowPosition.copy(position = to))

  def lastOutcome: Outcome =
    historicOutcomes.lastOption.map(_.outcome) getOrElse Outcome.succeeded

  def keyValues: Map[String, String] = historicOutcomes
    .collect { case HistoricOutcome(_, o: Outcome.Completed) => o.keyValues }
    .fold(arguments)((a, b) => a ++ b)

  def isStarted = isState[IsStarted]

  def castState[A <: State: ClassTag]: Order[A] =
    checkedState[A].orThrow

  def checkedState[A <: State: ClassTag]: Checked[Order[A]] =
    Checked.fromOption(ifState[A], Problem(s"'$id' is expected to be in state ${implicitClass[A].simpleScalaName}, but is in state $state"))

  def ifState[A <: State: ClassTag]: Option[Order[A]] =
    isState[A] ? this.asInstanceOf[Order[A]]

  def isState[A <: State: ClassTag] =
    implicitClass[A] isAssignableFrom state.getClass

  def markString: Option[String] =
    mark.map(o => s"marked as $o")

  def attachedStateString: String =
    attachedState match {
      case None => "on Controller"
      case Some(Attaching(agentName)) => s"attachable to $agentName"
      case Some(Attached(agentName)) => s"attached to $agentName"
      case Some(Detaching(agentName)) => s"detaching from $agentName"
    }

  /** `true` iff order is going to be attached to an Agent.. */
  def isAttaching: Boolean =
    attachedState.exists(_.isInstanceOf[Attaching])

  /** `true` iff order is attached to and processable on an Agent. */
  def isAttached: Boolean =
    attachedState.exists(_.isInstanceOf[Attached])

  /** `true` iff order is going to be detached from an Agent. */
  def isDetaching: Boolean =
    attachedState.exists(_.isInstanceOf[Detaching])

  /** `true` iff order is processable on Controller.. */
  def isDetached: Boolean =
    attachedState.isEmpty

  def attached: Checked[AgentName] =
    attachedState match {
      case Some(Attached(agentName)) =>
        Right(agentName)
      case o =>
        Left(Problem(s"'$id' should be 'Attached', but is $o"))
    }

  def detaching: Checked[AgentName] =
    attachedState match {
      case Some(Detaching(agentName)) =>
        Right(agentName)
      case o =>
        Left(Problem(s"'$id' should be Detaching, but is $o"))
    }

  def isMarkable =
    !isState[IsTerminated] ||
      isState[FailedInFork]/*when asynchronously marked on Agent*/

  def isCancelable =
    parent.isEmpty &&
      (isState[IsFreshOrReady] ||
       isState[ProcessingKilled] ||
       isState[FailedWhileFresh] ||
       isState[DelayedAfterError] ||
       isState[Failed] ||
       isState[Broken]) &&
      (isDetached || isAttached)

  def isCancelling =
    mark.exists(_.isInstanceOf[OrderMark.Cancelling])

  private def cleanMark: Option[OrderMark] =
    mark match {
      case Some(OrderMark.Cancelling(CancelMode.FreshOnly)) if isStarted => None
      case o => o
    }

  def isSuspendingOrSuspended = isSuspending || isSuspended

  def isSuspendible =
    (isState[IsFreshOrReady] /*|| isState[DelayedAfterError]*/ || isState[ProcessingKilled] && isSuspendingWithKill) &&
    (isDetached || isAttached)

  def isSuspending =
    mark.exists(_.isInstanceOf[OrderMark.Suspending])

  private[order] def isSuspendingWithKill = mark match {
    case Some(OrderMark.Suspending(SuspendMode(Some(_: CancelMode.Kill)))) => true
    case _ => false
  }

  def isResuming =
    mark.exists(_.isInstanceOf[OrderMark.Resuming])

  def isResumable =
    ((isState[IsFreshOrReady] && isSuspended /*|| isState[Failed]*/) ||
      isState[Broken]) &&
      (isDetached || isAttached)

  def isProcessable =
    isState[IsFreshOrReady] && !isSuspendingOrSuspended
}

object Order
{
  def fromOrderAdded(id: OrderId, event: OrderAdded): Order[Fresh] = {
    Order(id, event.workflowId, Fresh(event.scheduledFor), event.arguments)
  }

  def fromOrderAttached(id: OrderId, event: OrderAttachedToAgent): Order[IsFreshOrReady] =
    Order(id, event.workflowPosition, event.state, event.arguments, event.historicOutcomes, Some(Attached(event.agentName)),
      event.parent, event.mark, isSuspended = event.isSuspended, removeWhenTerminated = event.removeWhenTerminated)

  sealed trait AttachedState
  object AttachedState {
    sealed trait HasAgentName extends AttachedState {
      def agentName: AgentName
    }
    implicit val jsonCodec = TypedJsonCodec[AttachedState](
      Subtype(deriveCodec[Attaching]),
      Subtype(deriveCodec[Attached]),
      Subtype(deriveCodec[Detaching]))
  }
  /** Order is going to be attached to an Agent. */
  final case class Attaching(agentName: AgentName) extends AttachedState.HasAgentName {
    override def toString = s"Attaching(${agentName.string})"
  }
  /** Order is attached to an Agent. */
  final case class Attached(agentName: AgentName) extends AttachedState.HasAgentName {
    override def toString = s"Attached(${agentName.string})"
  }
  /** Order is going to be detached from Agent. */
  final case class Detaching(agentName: AgentName) extends AttachedState.HasAgentName {
    override def toString = s"Detaching(${agentName.string})"
  }

  sealed trait State {
    def maybeDelayedUntil: Option[Timestamp] = None
  }

  /** OrderStarted occurred. */
  sealed trait IsStarted extends State

  sealed trait IsFreshOrReady extends State

  /** Terminal state — the order can only be removed. */
  sealed trait IsTerminated extends State

  sealed trait DelayedUntil {
    def delayedUntil: Timestamp
  }

  final case class Fresh(scheduledFor: Option[Timestamp] = None) extends IsFreshOrReady {
    override def maybeDelayedUntil = scheduledFor
  }
  object Fresh {
    val StartImmediately = Fresh(None)
  }

  type Ready = Ready.type
  case object Ready extends IsStarted with IsFreshOrReady

  final case class DelayedAfterError(until: Timestamp) extends IsStarted {
    override def maybeDelayedUntil = Some(until)
  }

  type FailedWhileFresh = FailedWhileFresh.type
  case object FailedWhileFresh extends State

  final case class Broken(problem: Problem) extends IsStarted/*!!!*/

  type Processing = Processing.type
  case object Processing extends IsStarted

  type Processed = Processed.type
  case object Processed extends IsStarted

  type ProcessingKilled = ProcessingKilled.type
  case object ProcessingKilled extends IsStarted

  final case class Forked(children: Seq[Forked.Child]) extends IsStarted {
    def childOrderIds = children.map(_.orderId)
  }
  object Forked {
    type Child = OrderForked.Child
    val Child = OrderForked.Child
  }

  final case class Offering(until: Timestamp)
  extends IsStarted

  final case class Awaiting(offeredOrderId: OrderId) extends IsStarted

  // TODO Redundant outcome ?
  final case class Failed(outcome: Outcome.NotSucceeded) extends IsStarted

  final case class FailedInFork(outcome: Outcome.NotSucceeded) extends IsStarted with IsTerminated

  type Finished = Finished.type
  case object Finished extends IsStarted with IsTerminated

  type Cancelled = Cancelled.type
  case object Cancelled extends IsTerminated

  type Removed = Removed.type
  case object Removed extends State

  implicit val FreshOrReadyJsonCodec: TypedJsonCodec[IsFreshOrReady] = TypedJsonCodec[IsFreshOrReady](
    Subtype(deriveCodec[Fresh]),
    Subtype(Ready))

  implicit val StateJsonCodec: TypedJsonCodec[State] = TypedJsonCodec(
    Subtype[IsFreshOrReady],
    Subtype(Processing),
    Subtype(Processed),
    Subtype(ProcessingKilled),
    Subtype(deriveCodec[DelayedAfterError]),
    Subtype(FailedWhileFresh),
    Subtype(deriveCodec[Forked]),
    Subtype(deriveCodec[Offering]),
    Subtype(deriveCodec[Awaiting]),
    Subtype(deriveCodec[Failed]),
    Subtype(deriveCodec[FailedInFork]),
    Subtype(Finished),
    Subtype(Cancelled),
    Subtype(Removed),
    Subtype(deriveCodec[Broken]))

  implicit val jsonEncoder: Encoder.AsObject[Order[State]] = order =>
    JsonObject(
      "id" -> order.id.asJson,
      "arguments" -> (order.arguments.nonEmpty ? order.arguments).asJson,
      "workflowPosition" -> order.workflowPosition.asJson,
      "state" -> order.state.asJson,
      "attachedState" -> order.attachedState.asJson,
      "parent" -> order.parent.asJson,
      "historicOutcomes" -> order.historicOutcomes.asJson,
      "mark" -> order.mark.asJson,
      "isSuspended" -> (order.isSuspended ? true).asJson,
      "removeWhenTerminated" -> (order.removeWhenTerminated ? true).asJson)

  implicit val jsonDecoder: Decoder[Order[State]] = cursor =>
    for {
      id <- cursor.get[OrderId]("id")
      arguments <- cursor.get[Option[Map[String, String]]]("arguments").map(_ getOrElse Map.empty)
      workflowPosition <- cursor.get[WorkflowPosition]("workflowPosition")
      state <- cursor.get[State]("state")
      attachedState <- cursor.get[Option[AttachedState]]("attachedState")
      parent <- cursor.get[Option[OrderId]]("parent")
      historicOutcomes <- cursor.get[Seq[HistoricOutcome]]("historicOutcomes")
      mark <- cursor.get[Option[OrderMark]]("mark")
      isSuspended <- cursor.get[Option[Boolean]]("isSuspended").map(_ getOrElse false)
      removeWhenTerminated <- cursor.get[Option[Boolean]]("removeWhenTerminated").map(_ getOrElse false)
    } yield
      Order(id, workflowPosition, state, arguments, historicOutcomes, attachedState, parent, mark,
        isSuspended, removeWhenTerminated)

  implicit val FreshOrReadyOrderJsonEncoder: Encoder.AsObject[Order[IsFreshOrReady]] = o => jsonEncoder.encodeObject(o)
  implicit val FreshOrReadyOrderJsonDecoder: Decoder[Order[IsFreshOrReady]] = cursor =>
    jsonDecoder(cursor) flatMap {
      o => o.ifState[IsFreshOrReady] match {
        case None => Left(DecodingFailure(s"Order is not Fresh or Ready, but: ${o.state.getClass.simpleScalaName}", cursor.history))
        case Some(x) => Right(x)
      }
    }
}
