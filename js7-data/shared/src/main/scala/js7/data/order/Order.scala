package js7.data.order

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, DecodingFailure, Encoder, JsonObject}
import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.problem.Checked.{CheckedOption, Ops}
import js7.base.problem.{Checked, Problem}
import js7.base.time.Timestamp
import js7.base.utils.ScalaUtils._
import js7.base.utils.ScalaUtils.syntax._
import js7.base.utils.typeclasses.IsEmpty.syntax._
import js7.data.agent.AgentPath
import js7.data.command.{CancellationMode, SuspensionMode}
import js7.data.job.JobKey
import js7.data.order.Order._
import js7.data.order.OrderEvent._
import js7.data.orderwatch.ExternalOrderKey
import js7.data.value.{NamedValues, Value}
import js7.data.workflow.position.{BranchId, InstructionNr, Position, WorkflowPosition}
import js7.data.workflow.{Workflow, WorkflowId}
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
final case class Order[+S <: Order.State](
  id: OrderId,
  workflowPosition: WorkflowPosition,
  state: S,
  arguments: NamedValues = Map.empty,
  scheduledFor: Option[Timestamp] = None,
  externalOrderKey: Option[ExternalOrderKey] = None,
  historicOutcomes: Seq[HistoricOutcome] = Vector.empty,
  attachedState: Option[AttachedState] = None,
  parent: Option[OrderId] = None,
  mark: Option[OrderMark] = None,
  isSuspended: Boolean = false,
  deleteWhenTerminated: Boolean = false)
{
  // Accelerate usage in Set[Order], for example in AgentDriver's CommandQueue
  override def hashCode = id.hashCode

  def newForkedOrders(event: OrderForked): Seq[Order[Ready]] =
    for (child <- event.children) yield
      Order(
        child.orderId,
        workflowPosition.copy(position = workflowPosition.position / child.branchId.toBranchId % InstructionNr.First),
        Ready,
        arguments,
        scheduledFor = scheduledFor,
        historicOutcomes = historicOutcomes,
        attachedState = attachedState,
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

  def applyEvent(event: OrderEvent.OrderCoreEvent): Checked[Order[State]] =
    applyEvent2(event, force = false)

  /** Force the application of the `event` even if the the state does not match.
    * Can be used when some events like `OrderAttachable` and `OrderDetachable` are left out.
    * May still return `Left(Problem)` in some cases.
    */
  def forceEvent(event: OrderEvent.OrderCoreEvent): Checked[Order[State]] =
    applyEvent2(event, force = true)

  private def applyEvent2(event: OrderEvent.OrderCoreEvent, force: Boolean): Checked[Order[State]] = {
    def inapplicable = Left(InapplicableOrderEventProblem(event, this))

    def check[A](okay: => Boolean, updated: A) =
      if (force || okay) Right(updated) else inapplicable

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

      case OrderFailed(movedTo, outcome_, _) =>
        check(isOrderFailedApplicable,
          copy(
            state = if (isState[Fresh]) FailedWhileFresh else Failed,
            workflowPosition = workflowPosition.copy(position = movedTo),
            historicOutcomes = outcome_.fold(historicOutcomes)(o => historicOutcomes :+ HistoricOutcome(position, o))))

      case OrderFailedInFork(movedTo, outcome_, _) =>
        check((isState[Ready] || isState[Processed]) && !isSuspended && (isDetached || isAttached),
          copy(
            state = FailedInFork,
            workflowPosition = workflowPosition.copy(position = movedTo),
            historicOutcomes = outcome_.fold(historicOutcomes)(o => historicOutcomes :+ HistoricOutcome(position, o))))

      case OrderFailedIntermediate_(_, _) =>
        inapplicable  // Intermediate event, internal only

      case OrderCatched(movedTo, outcome_, _) =>
        check((isState[Ready] || isState[Processed]) && !isSuspended && (isAttached | isDetached),
          copy(
            state = Ready,
            workflowPosition = workflowPosition.copy(position = movedTo),
            historicOutcomes = outcome_.fold(historicOutcomes)(o => historicOutcomes :+ HistoricOutcome(position, o))))

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

      case OrderDeletionMarked =>
        check(parent.isEmpty,
          copy(deleteWhenTerminated = true))

      case OrderDeleted =>
        check(isState[IsTerminated] && isDetached && parent.isEmpty,
          copy(state = Deleted))

      case OrderBroken(message) =>
        check(!isState[IsTerminated],
          copy(state = Broken(message)))

      case OrderAttachable(agentPath) =>
        check((isState[Fresh] || isState[Ready] || isState[Forked]) && isDetached,
          copy(attachedState = Some(Attaching(agentPath))))

      case OrderAttached(agentPath) =>
        attachedState match {
          case Some(Attaching(`agentPath`)) =>
            check((isState[Fresh] || isState[Ready] || isState[Forked]) && isAttaching,
              copy(attachedState = Some(Attached(agentPath))))
          case _ =>
            if (force)
              Right(copy(
                attachedState = Some(Attached(agentPath))))
            else
              inapplicable
        }

      case OrderDetachable =>
        attachedState match {
          case Some(Attached(agentPath)) if isInDetachableState =>
            Right(copy(attachedState = Some(Detaching(agentPath))))
          case _ =>
            inapplicable
        }

      case OrderDetached =>
        check(!isDetached && isInDetachableState,
          copy(attachedState = None))

      case OrderCancellationMarked(mode) =>
        check(parent.isEmpty && isMarkable,
          copy(mark = Some(OrderMark.Cancelling(mode))))

      case OrderCancellationMarkedOnAgent =>
        Right(this)

      case OrderCancelled =>
        check(isCancelable && isDetached,
          copy(
            state = Cancelled,
            mark = None))

      case OrderSuspensionMarked(kill) =>
        check(isMarkable,
          copy(mark = Some(OrderMark.Suspending(kill))))

      case OrderSuspensionMarkedOnAgent =>
        Right(this)

      case OrderSuspended =>
        check(isSuspendible && (isDetached || isSuspended/*already Suspended, to clean Resuming mark*/),
          copy(
            isSuspended = true,
            mark = None,
            state = if (isSuspendingWithKill && isState[ProcessingKilled]) Ready else state))

      case OrderResumptionMarked(position, historicOutcomes) =>
        if (!force && !isMarkable)
          inapplicable
        else if (isSuspended)
          Right(copy(mark = Some(OrderMark.Resuming(position, historicOutcomes))))
        else if (!force && (position.isDefined || historicOutcomes.isDefined))
            // Inhibited because we cannot be sure wether order will pass a fork barrier
          inapplicable
        else if (!isSuspended && isSuspending)
          Right(copy(mark = None/*revert OrderSuspensionMarked*/))
        else
          Right(copy(mark = Some(OrderMark.Resuming(None))))

      case OrderResumed(maybePosition, maybeHistoricOutcomes) =>
        check(isResumable,
          copy(
            isSuspended = false,
            mark = None,
            state = if (isState[Broken] || isState[Failed]) Ready else state,
            historicOutcomes = maybeHistoricOutcomes getOrElse historicOutcomes
          ).withPosition(maybePosition getOrElse position))

      case _: OrderLockAcquired =>
        // LockState handles this event, too
        check(isDetached && (isState[Ready] || isState[WaitingForLock]),
          copy(state = Ready).withPosition(position / BranchId.Lock % 0))

      case _: OrderLockReleased =>
        // LockState handles this event, too
        if (force || isDetached && isState[Ready])
          position.dropChild
            .toChecked(Problem(s"OrderLockReleased event but position=$workflowPosition"))
            .map(pos => withPosition(pos.increment))
        else
          inapplicable

      case _: OrderLockQueued =>
        check(isDetached && isState[Ready],
          copy(
            state = WaitingForLock))

      case OrderPrompted(question) =>
        check(isDetached && isState[Ready],
          copy(state = Prompting(question)))

      case OrderPromptAnswered() =>
        check(isDetached && isState[Prompting],
          copy(
            state = Ready))
            //historicOutcomes = historicOutcomes :+ HistoricOutcome(position, outcome)))
    }
  }

  def isOrderFailedApplicable =
    !isSuspended &&
      isDetached &&
      (isState[IsFreshOrReady] || isState[Processed])

  def withInstructionNr(to: InstructionNr): Order[S] =
    withPosition(position.copy(nr = to))

  def withPosition(to: Position): Order[S] = copy(
    workflowPosition = workflowPosition.copy(position = to))

  def maybeDelayedUntil: Option[Timestamp] =
    if (isState[Fresh])
      scheduledFor
    else
      state.maybeDelayedUntil

  def lastOutcome: Outcome =
    historicOutcomes.lastOption.map(_.outcome) getOrElse Outcome.succeeded

  // Test in OrderScopesTest
  /** The named values as seen at the current workflow position. */
  def namedValues(defaultArguments: Map[String, Value]): NamedValues =
    historicOutcomes.view
      .collect { case HistoricOutcome(_, o: Outcome.Completed) => o.namedValues }
      .flatten
      .concat(defaultArguments)
      .concat(arguments)
      .toMap

  /** In JobScheduler 1, job results overwrote order arguments. */
  def v1CompatibleNamedValues(defaultArguments: Map[String, Value]): NamedValues =
    defaultArguments.view
      .concat(arguments)
      .concat(
        historicOutcomes.view
          .collect { case HistoricOutcome(_, o: Outcome.Completed) => o.namedValues }
          .flatten)
      .toMap

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
      case None => "at Controller"
      case Some(Attaching(agentPath)) => s"attachable to $agentPath"
      case Some(Attached(agentPath)) => s"attached to $agentPath"
      case Some(Detaching(agentPath)) => s"detaching from $agentPath"
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

  /** `true` iff order is processable at Controller.. */
  def isDetached: Boolean =
    attachedState.isEmpty

  def attached: Checked[AgentPath] =
    attachedState match {
      case Some(Attached(agentPath)) =>
        Right(agentPath)
      case o =>
        Left(Problem(s"'$id' should be 'Attached', but is $o"))
    }

  def detaching: Checked[AgentPath] =
    attachedState match {
      case Some(Detaching(agentPath)) =>
        Right(agentPath)
      case o =>
        Left(Problem(s"'$id' should be Detaching, but is $o"))
    }

  def isInDetachableState =
    isState[Fresh] || isState[Ready] || isState[Forked] || isState[Processed] || isState[ProcessingKilled] ||
    isState[FailedWhileFresh] || isState[Failed] || isState[FailedInFork] || isState[Broken]

  def isMarkable =
    !isState[IsTerminated] && !isState[Deleted] ||
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
      case Some(OrderMark.Cancelling(CancellationMode.FreshOnly)) if isStarted => None
      case o => o
    }

  def isMarked =
    mark.isDefined

  def isSuspendible =
    (isState[IsFreshOrReady] /*|| isState[DelayedAfterError]*/ || isState[ProcessingKilled] && isSuspendingWithKill) &&
    (isDetached || isAttached)

  def isSuspending =
    mark.exists(_.isInstanceOf[OrderMark.Suspending])

  private[order] def isSuspendingWithKill = mark match {
    case Some(OrderMark.Suspending(SuspensionMode(Some(_: CancellationMode.Kill)))) => true
    case _ => false
  }

  def isResuming =
    mark.exists(_.isInstanceOf[OrderMark.Resuming])

  def isResumable =
    (isState[IsFreshOrReady] && isSuspended ||
      isState[Failed] && !isSuspended/*strict for test*/ && isDetached ||
      isState[Broken]
    ) &&
      (isDetached || isAttached)

  def isProcessable =
    isState[IsFreshOrReady] && !isSuspended && !isMarked

  /** Number of executions for this job (starting with 1). */
  def historicJobExecutionCount(jobKey: JobKey, workflow: Workflow): Int = {
    val x = Right(jobKey)
    historicOutcomes.view
      .map(o => workflow.positionToJobKey(o.position))
      .count(_ == x)
  }
}

object Order
{
  def fromOrderAdded(id: OrderId, event: OrderAdded): Order[Fresh] =
    Order(id, event.workflowId, Fresh, event.arguments ++ event.variables,
      event.scheduledFor, event.externalOrderKey)

  def fromOrderAttached(id: OrderId, event: OrderAttachedToAgent): Order[IsFreshOrReady] =
    Order(id, event.workflowPosition, event.state, event.arguments,
      event.scheduledFor,
      event.externalOrderKey,
      historicOutcomes = event.historicOutcomes,
      Some(Attached(event.agentPath)),
      event.parent, event.mark,
      isSuspended = event.isSuspended,
      deleteWhenTerminated = event.deleteWhenTerminated)

  sealed trait AttachedState
  object AttachedState {
    sealed trait HasAgentPath extends AttachedState {
      def agentPath: AgentPath
    }
    object HasAgentPath {
      def unapply(o: HasAgentPath) = Some(o.agentPath)
    }
    implicit val jsonCodec = TypedJsonCodec[AttachedState](
      Subtype(deriveCodec[Attaching]),
      Subtype(deriveCodec[Attached]),
      Subtype(deriveCodec[Detaching]))
  }
  /** Order is going to be attached to an Agent. */
  final case class Attaching(agentPath: AgentPath) extends AttachedState.HasAgentPath {
    override def toString = s"Attaching(${agentPath.string})"
  }
  /** Order is attached to an Agent. */
  final case class Attached(agentPath: AgentPath) extends AttachedState.HasAgentPath {
    override def toString = s"Attached(${agentPath.string})"
  }
  /** Order is going to be detached from Agent. */
  final case class Detaching(agentPath: AgentPath) extends AttachedState.HasAgentPath {
    override def toString = s"Detaching(${agentPath.string})"
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

  type Fresh = Fresh.type
  case object Fresh extends IsFreshOrReady

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

  type WaitingForLock = WaitingForLock.type
  case object WaitingForLock
  extends IsStarted

  final case class Prompting(question: Value)
  extends IsStarted

  final case class Offering(until: Timestamp)
  extends IsStarted

  final case class Awaiting(offeredOrderId: OrderId) extends IsStarted

  type Failed = Failed.type
  final case object Failed extends IsStarted

  type FailedInFork = FailedInFork.type
  final case object FailedInFork extends IsStarted with IsTerminated

  type Finished = Finished.type
  case object Finished extends IsStarted with IsTerminated

  type Cancelled = Cancelled.type
  case object Cancelled extends IsTerminated

  type Deleted = Deleted.type
  case object Deleted extends State

  implicit val FreshOrReadyJsonCodec: TypedJsonCodec[IsFreshOrReady] = TypedJsonCodec[IsFreshOrReady](
    Subtype(Fresh),
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
    Subtype(WaitingForLock),
    Subtype(Failed),
    Subtype(FailedInFork),
    Subtype(Finished),
    Subtype(Cancelled),
    Subtype(Deleted),
    Subtype(deriveCodec[Prompting]),
    Subtype(deriveCodec[Broken]))

  implicit val jsonEncoder: Encoder.AsObject[Order[State]] = order =>
    JsonObject(
      "id" -> order.id.asJson,
      "workflowPosition" -> order.workflowPosition.asJson,
      "state" -> order.state.asJson,
      "arguments" -> order.arguments.??.asJson,
      "scheduledFor" -> order.scheduledFor.asJson,
      "externalOrderKey" -> order.externalOrderKey.asJson,
      "attachedState" -> order.attachedState.asJson,
      "parent" -> order.parent.asJson,
      "mark" -> order.mark.asJson,
      "isSuspended" -> order.isSuspended.?.asJson,
      "deleteWhenTerminated" -> order.deleteWhenTerminated.?.asJson,
      "historicOutcomes" -> order.historicOutcomes.??.asJson)

  implicit val jsonDecoder: Decoder[Order[State]] = cursor =>
    for {
      id <- cursor.get[OrderId]("id")
      workflowPosition <- cursor.get[WorkflowPosition]("workflowPosition")
      state <- cursor.get[State]("state")
      arguments <- cursor.getOrElse[NamedValues]("arguments")(NamedValues.empty)
      scheduledFor <- cursor.get[Option[Timestamp]]("scheduledFor")
      externalOrderKey <- cursor.get[Option[ExternalOrderKey]]("externalOrderKey")
      attachedState <- cursor.get[Option[AttachedState]]("attachedState")
      parent <- cursor.get[Option[OrderId]]("parent")
      mark <- cursor.get[Option[OrderMark]]("mark")
      isSuspended <- cursor.getOrElse[Boolean]("isSuspended")(false)
      deleteWhenTerminated <- cursor.getOrElse[Boolean]("deleteWhenTerminated")(false)
      historicOutcomes <- cursor.getOrElse[Vector[HistoricOutcome]]("historicOutcomes")(Vector.empty)
    } yield
      Order(id, workflowPosition, state, arguments, scheduledFor, externalOrderKey, historicOutcomes,
        attachedState, parent, mark, isSuspended, deleteWhenTerminated)

  implicit val FreshOrReadyOrderJsonEncoder: Encoder.AsObject[Order[IsFreshOrReady]] = o => jsonEncoder.encodeObject(o)
  implicit val FreshOrReadyOrderJsonDecoder: Decoder[Order[IsFreshOrReady]] = cursor =>
    jsonDecoder(cursor) flatMap {
      o => o.ifState[IsFreshOrReady] match {
        case None => Left(DecodingFailure(s"Order is not Fresh or Ready, but: ${o.state.getClass.simpleScalaName}", cursor.history))
        case Some(x) => Right(x)
      }
    }

  final case class InapplicableOrderEventProblem(event: OrderEvent, order: Order[State])
  extends Problem.Coded {
    def arguments = Map(
      "orderId" -> order.id.string,
      "event" -> event.toString,
      "workflowPosition" -> order.workflowPosition.toString,
      "state" -> order.state.getClass.simpleScalaName,
      "more" -> (order.markString.fold("")(o => s"$o, ") + order.attachedStateString))
  }
}
