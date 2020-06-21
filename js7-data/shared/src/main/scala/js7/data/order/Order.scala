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
import js7.base.utils.ScalazStyle.OptionRichBoolean
import js7.data.agent.AgentRefPath
import js7.data.command.CancelMode
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

  def newOfferedOrder(event: OrderOffered): Order[Offering] = copy(
    event.orderId,
    state = Offering(event.until),
    parent = None)

  def workflowId: WorkflowId =
    workflowPosition.workflowId

  def position: Position =
    workflowPosition.position

  def forkPosition: Checked[Position] = {
    val forkBranchPath = position.branchPath.reverse.dropWhile(o => !o.branchId.isFork).reverse
    if (forkBranchPath.isEmpty)
      Left(Problem.pure(s"Order '${id.string}' is in state FailedInFork but not below a Fork instruction"))
    else
      Right(forkBranchPath.init % forkBranchPath.last.nr)
  }

  def update(event: OrderEvent.OrderCoreEvent): Checked[Order[State]] = {
    def inapplicable = Left(Problem(
      s"Order '${id.string}' at position '$workflowPosition' in state '${state.getClass.simpleScalaName}' ($attachedStateString) has received an inapplicable event: $event"))

    def check[A](okay: Boolean, updated: A) =
      if (okay) Right(updated) else inapplicable

    event match {
      case _: OrderAdded | _: OrderAttached =>
        Left(Problem("OrderAdded and OrderAttached events are not handled by the Order itself"))

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

      case OrderProcessingCancelled =>
        check(isState[Processed] && isAttached,
          copy(state = ProcessingCancelled))

      case OrderFailed(outcome_) =>
        check(isOrderFailedApplicable,
          copy(
            state = if (isState[Fresh]) FailedWhileFresh else Failed(outcome_),
            historicOutcomes = historicOutcomes :+ HistoricOutcome(position, outcome_)))

      case OrderFailedInFork(outcome_) =>
        check((isState[Ready] || isState[Processed]) && (isDetached || isAttached),
          copy(
            state = FailedInFork(outcome_),
            historicOutcomes = historicOutcomes :+ HistoricOutcome(position, outcome_)))

      case OrderFailedCatchable(_) =>
        inapplicable

      case OrderCatched(outcome_, movedTo) =>
        check(isState[Ready] && (isDetached || isAttached)  ||  isState[Processed] && (isAttached | isDetached),
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
        check((isState[IsFreshOrReady]/*before TryInstruction*/ || isState[Processed]) && (isDetached || isAttached),
          withPosition(to).copy(
            state = if (isState[Fresh]) state else Ready))

      case OrderFinished =>
        check(isState[Ready] && isDetached,
          position.dropChild match {
            case Some(position) => copy(workflowPosition = workflowPosition.copy(position = position))
            case None => copy(state = Finished)
          })

      case OrderBroken(message) =>
        check(!isState[IsFinal],
          copy(state = Broken(message)))

      case OrderAttachable(agentRefPath) =>
        check(isDetached && (isState[Fresh] || isState[Ready] || isState[Forked]),
          copy(attachedState = Some(Attaching(agentRefPath))))

      case OrderTransferredToAgent(agentRefPath) =>
        attachedState match {
          case Some(Attaching(`agentRefPath`)) =>
            check(isAttaching && (isState[Fresh] || isState[Ready] || isState[Forked]),
              copy(attachedState = Some(Attached(agentRefPath))))
          case _ => inapplicable
        }

      case OrderDetachable =>
        attachedState match {
          case Some(Attached(agentRefPath))
            if isState[Fresh] || isState[Ready] || isState[Forked] || isState[ProcessingCancelled] ||
               isState[FailedWhileFresh] || isState[Failed] || isState[FailedInFork] || isState[Broken] =>
              Right(copy(attachedState = Some(Detaching(agentRefPath))))
          case _ =>
            inapplicable
        }

      case OrderDetached =>
        check(isDetaching && (isState[Fresh] || isState[Ready] || isState[Forked] || isState[ProcessingCancelled] ||
                              isState[FailedWhileFresh] || isState[Failed] || isState[FailedInFork] || isState[Broken]),
          copy(attachedState = None))

      case OrderTransferredToController =>
        check(isDetaching && (isState[Fresh] || isState[Ready] || isState[Forked] || isState[ProcessingCancelled] ||
                              isState[FailedWhileFresh] || isState[Failed] || isState[FailedInFork] || isState[Broken]),
          copy(attachedState = None))

      case OrderCancellationMarked(mode) =>
        check(!isState[IsFinal] && !isState[ProcessingCancelled] && !isDetaching,
          copy(cancel = Some(mode)))

      case OrderCancelled =>
        check((isState[IsFreshOrReady] || isState[ProcessingCancelled] ||
               isState[DelayedAfterError] || isState[FailedWhileFresh] || isState[Failed] || isState[Broken])
            && isDetached,
          copy(state = Cancelled))
    }
  }

  def isOrderFailedApplicable =
    isState[IsFreshOrReady] && (isDetached || isAttached)  ||
    isState[Processed] && (isDetached || isAttached)

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
      case None => "on Controller"
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

  /** `true` iff order is processable on Controller.. */
  def isDetached: Boolean =
    attachedState.isEmpty

  def attached: Checked[AgentRefPath] =
    attachedState match {
      case Some(Attached(agentRefPath)) =>
        Right(agentRefPath)
      case o =>
        Left(Problem(s"'$id' should be 'Attached', but is $o"))
    }

  def detaching: Checked[AgentRefPath] =
    attachedState match {
      case Some(Detaching(agentRefPath)) =>
        Right(agentRefPath)
      case o =>
        Left(Problem(s"'$id' should be Detaching, but is $o"))
    }
}

object Order
{
  def fromOrderAdded(id: OrderId, event: OrderAdded): Order[Fresh] = {
    Order(id, event.workflowId, Fresh(event.scheduledFor), event.arguments)
  }

  def fromOrderAttached(id: OrderId, event: OrderAttached): Order[IsFreshOrReady] =
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

  /** OrderStarted occurred. */
  sealed trait IsStarted extends State

  sealed trait IsFreshOrReady extends State

  /** Final state — the order will be deleted immediately. */
  sealed trait IsFinal extends State

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

  type ProcessingCancelled = ProcessingCancelled.type
  case object ProcessingCancelled extends IsStarted

  final case class Forked(children: Seq[Forked.Child]) extends IsStarted {
    def childOrderIds = children map (_.orderId)
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

  final case class FailedInFork(outcome: Outcome.NotSucceeded) extends IsStarted with IsFinal

  type Finished = Finished.type
  case object Finished extends IsStarted with IsFinal

  type Cancelled = Cancelled.type
  case object Cancelled extends IsFinal

  implicit val FreshOrReadyJsonCodec: TypedJsonCodec[IsFreshOrReady] = TypedJsonCodec[IsFreshOrReady](
    Subtype(deriveCodec[Fresh]),
    Subtype(Ready))

  implicit val StateJsonCodec: TypedJsonCodec[State] = TypedJsonCodec(
    Subtype[IsFreshOrReady],
    Subtype(Processing),
    Subtype(Processed),
    Subtype(ProcessingCancelled),
    Subtype(deriveCodec[DelayedAfterError]),
    Subtype(FailedWhileFresh),
    Subtype(deriveCodec[Forked]),
    Subtype(deriveCodec[Offering]),
    Subtype(deriveCodec[Awaiting]),
    Subtype(deriveCodec[Failed]),
    Subtype(deriveCodec[FailedInFork]),
    Subtype(Finished),
    Subtype(Cancelled),
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

  implicit val FreshOrReadyOrderJsonEncoder: Encoder.AsObject[Order[IsFreshOrReady]] = o => jsonEncoder.encodeObject(o)
  implicit val FreshOrReadyOrderJsonDecoder: Decoder[Order[IsFreshOrReady]] = cursor =>
    jsonDecoder(cursor) flatMap {
      o => o.ifState[IsFreshOrReady] match {
        case None => Left(DecodingFailure(s"Order is not Fresh or Ready, but: ${o.state.getClass.simpleScalaName}", cursor.history))
        case Some(x) => Right(x)
      }
    }
}
