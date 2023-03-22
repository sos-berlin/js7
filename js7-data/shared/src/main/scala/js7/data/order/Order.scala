package js7.data.order

import io.circe.generic.semiauto.deriveCodec
import io.circe.syntax.EncoderOps
import io.circe.{Codec, Decoder, DecodingFailure, Encoder, JsonObject}
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.problem.Checked.{CheckedOption, Ops}
import js7.base.problem.{Checked, Problem}
import js7.base.time.Timestamp
import js7.base.utils.Assertions.assertThat
import js7.base.utils.ScalaUtils.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.typeclasses.IsEmpty.syntax.*
import js7.data.agent.AgentPath
import js7.data.board.NoticeId
import js7.data.command.{CancellationMode, SuspensionMode}
import js7.data.event.EventDrivenState.EventNotApplicableProblem
import js7.data.job.JobKey
import js7.data.order.Order.*
import js7.data.order.OrderEvent.*
import js7.data.orderwatch.ExternalOrderKey
import js7.data.subagent.{SubagentId, SubagentSelectionId}
import js7.data.value.{NamedValues, Value}
import js7.data.workflow.position.{BranchId, InstructionNr, Position, PositionOrLabel, WorkflowPosition}
import js7.data.workflow.{Workflow, WorkflowId, WorkflowPath}
import scala.collection.{MapView, View, mutable}
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
  historicOutcomes: Vector[HistoricOutcome] = Vector.empty,
  attachedState: Option[AttachedState] = None,
  parent: Option[OrderId] = None,
  mark: Option[OrderMark] = None,
  isSuspended: Boolean = false,
  isResumed: Boolean = false,
  deleteWhenTerminated: Boolean = false,
  stickySubagents: List[StickySubagent] = Nil,
  stopPositions: Set[PositionOrLabel] = Set.empty)
{
  // Accelerate usage in Set[Order], for example in AgentDriver's CommandQueue
  override def hashCode = id.hashCode

  def newForkedOrders(event: OrderForked): View[Order[Ready]] =
    for (child <- event.children.view) yield
      Order(
        child.orderId,
        workflowPosition.copy(position = workflowPosition.position /
          child.branchId.fold(BranchId.ForkList)(_.toBranchId) % InstructionNr.First),
        Ready,
        arguments ++ child.arguments,
        scheduledFor = scheduledFor,
        historicOutcomes = historicOutcomes,
        attachedState = attachedState,
        parent = Some(id),
        stickySubagents = stickySubagents)

  def workflowId: WorkflowId =
    workflowPosition.workflowId

  def workflowPath: WorkflowPath =
    workflowId.path

  def position: Position =
    workflowPosition.position

  def forkPosition: Checked[Position] = {
    val reversed = position.forkBranchReversed
    if (reversed.isEmpty)
      Left(Problem.pure(s"$id is in state FailedInFork but not below a Fork instruction"))
    else
      Right(reversed.tail.reverse % reversed.head.nr)
  }

  def applyEvents(events: Iterable[OrderEvent.OrderCoreEvent]): Checked[Order[State]] = {
    // TODO Generalize this in ScalaUtils!
    var problem: Problem = null
    var order: Order[Order.State] = this
    val it = events.iterator
    while (it.hasNext && problem == null) {
      val event = it.next()
      order.applyEvent(event) match {
        case Right(o) => order = o
        case Left(prblm) => problem = prblm
      }
    }
    if (problem != null) Left(problem) else Right(order)
  }

  def applyEvent(event: OrderEvent.OrderCoreEvent): Checked[Order[State]] = {
    val force = false
    def inapplicableProblem = InapplicableOrderEventProblem(event, this)
    def inapplicable = Left(inapplicableProblem)

    def check[A](okay: => Boolean, updated: A) =
      if (force || okay) Right(updated) else inapplicable

    event match {
      case _: OrderAdded | _: OrderAttachedToAgent =>
        Left(Problem("OrderAdded and OrderAttachedToAgent events are not handled by the Order itself"))

      case OrderStarted =>
        check(isState[Fresh] && !isSuspendedOrStopped && (isDetached || isAttached),
          copy(state = Ready))

      case OrderProcessingStarted(subagentId, stuckSubagentId) =>
        check(isState[Ready] && !isSuspendedOrStopped && isAttached
          && (!stuckSubagentId || stickySubagents.nonEmpty),
          copy(
            state = Processing(subagentId),
            stickySubagents =
              if (stuckSubagentId)
                stickySubagents.head.copy(stuckSubagentId = subagentId) :: stickySubagents.tail
              else
                stickySubagents,
            mark = cleanMark))

      case OrderProcessed(outcome_) =>
        check(isState[Processing] && !isSuspendedOrStopped && isAttached,
          copy(
            state = Processed,
            historicOutcomes = historicOutcomes :+ HistoricOutcome(position, outcome_)))

      case OrderProcessingKilled =>
        check(isState[Processed] && !isSuspendedOrStopped && isAttached,
          copy(state = ProcessingKilled))

      case OrderOutcomeAdded(outcome) =>
        Right(copy(
          historicOutcomes = historicOutcomes :+ HistoricOutcome(position, outcome)))

      case OrderFailed(movedTo, outcome_) =>
        check(isFailable && isDetached,
          copy(
            state = if (isState[Fresh]) FailedWhileFresh else Failed,
            workflowPosition = workflowPosition.copy(position = movedTo),
            historicOutcomes = outcome_.fold(historicOutcomes)(o => historicOutcomes :+ HistoricOutcome(position, o))))

      case OrderFailedInFork(movedTo, outcome) =>
        check(parent.nonEmpty
          && isFailable
          && !isState[Fresh],
          copy(
            state = FailedInFork,
            workflowPosition = workflowPosition.copy(position = movedTo),
            isResumed = false,
            historicOutcomes =
              outcome.fold(historicOutcomes)(o => historicOutcomes :+ HistoricOutcome(position, o))))

      case OrderFailedIntermediate_(_, _) =>
        inapplicable  // Intermediate event, internal only

      case OrderCatched(movedTo, outcome) =>
        check((isState[Ready] || isState[Processed] || isState[ProcessingKilled]) &&
          !isSuspendedOrStopped &&
          (isAttached | isDetached),
          copy(
            state = Ready,
            workflowPosition = workflowPosition.copy(position = movedTo),
            isResumed = false,
            historicOutcomes =
              outcome.fold(historicOutcomes)(o => historicOutcomes :+ HistoricOutcome(position, o))))

      case OrderCaught(movedTo, outcome) =>
        check((isState[Ready] || isState[Processed] || isState[ProcessingKilled]) &&
          !isSuspendedOrStopped &&
          (isAttached | isDetached), {
          var h = outcome.fold(historicOutcomes)(o => historicOutcomes :+ HistoricOutcome(position, o))
          if (!h.lastOption.exists(_.outcome.isSucceeded))
            h :+= HistoricOutcome(movedTo, Outcome.succeeded)
          copy(
            state = Ready,
            workflowPosition = workflowPosition.copy(position = movedTo),
            isResumed = false,
            historicOutcomes = h)
        })

      case OrderRetrying(to, maybeDelayUntil) =>
        check(isState[Ready] && !isSuspendedOrStopped && (isDetached || isAttached),
          maybeDelayUntil
            .fold[Order[State]](this/*Ready*/)(o => copy(
              state = DelayedAfterError(o)))
            .withPosition(to))

      case OrderAwoke =>
        check(isState[DelayedAfterError] && !isSuspendedOrStopped && (isDetached || isAttached),
          copy(state = Ready))

      case OrderForked(children) =>
        check(isState[Ready] && !isSuspendedOrStopped && (isDetached || isAttached),
          copy(
            state = Forked(children),
            mark = cleanMark))

      case OrderJoined(outcome) =>
        check(isState[Forked] && !isSuspendedOrStopped && isDetached,
          copy(
            state = Processed,
            historicOutcomes = historicOutcomes :+ HistoricOutcome(position, outcome)))

      case OrderMoved(to, _) =>
        check((isState[IsFreshOrReady]/*before Try*/ || isState[Processed] || isState[BetweenCycles])
          && (isDetached || isAttached),
          withPosition(to).copy(
            isResumed = false,
            state = if (isState[Fresh]) state else Ready))

      case OrderFinished(maybeOutcome) =>
        check(isState[Ready] && isDetached && !isSuspendedOrStopped,
          copy(
            state = Finished,
            historicOutcomes = maybeOutcome.fold(historicOutcomes)(o =>
              historicOutcomes :+ HistoricOutcome(position, o)),
            mark = None,
            isResumed = false))

      case OrderDeletionMarked =>
        check(parent.isEmpty,
          copy(deleteWhenTerminated = true))

      case OrderDeleted =>
        check(isState[IsTerminated] && isDetached && parent.isEmpty,
          copy(state = Deleted))

      case OrderBroken(maybeProblem) =>
        check(!isState[IsTerminated],
          copy(
            state = Broken(maybeProblem),
            historicOutcomes = maybeProblem.fold(historicOutcomes)(problem =>
              historicOutcomes :+ HistoricOutcome(position, Outcome.Disrupted(problem)))))

      case OrderAttachable(agentPath) =>
        check((isState[Fresh] || isState[Ready] || isState[Forked]) && isDetached,
          copy(attachedState = Some(Attaching(agentPath))))

      case OrderAttached(agentPath) =>
        attachedState match {
          case Some(Attaching(`agentPath`)) =>
            check(isState[IsFreshOrReady] || isState[Forked],
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
        check(isMarkable,
          copy(mark = Some(OrderMark.Cancelling(mode))))

      case OrderCancellationMarkedOnAgent =>
        Right(this)

      case OrderOperationCancelled =>
        // Event is followed by OrderCancelled in the same transaction,
        // maybe after some block-leaving events which rely on state == Ready.
        check(state.isOperationCancelable && isDetached,
          copy(
            state = Ready,
            mark = None))

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

      case OrderStopped =>
        check(isFailable && isDetached,
          copy(
            state = if (isState[Fresh]) StoppedWhileFresh else Stopped))

      case OrderResumptionMarked(position, historyOperations, asSucceeded) =>
        if (!force && !isMarkable)
          inapplicable
        else if (isSuspended)
          Right(copy(
            mark = Some(OrderMark.Resuming(position, historyOperations, asSucceeded)),
            isResumed = true))
        else if (!force && (position.isDefined || historyOperations.nonEmpty))
            // Inhibited because we cannot be sure whether order will pass a fork barrier
          inapplicable
        else if (!isSuspended && isSuspending)
          Right(copy(
            mark = None/*revert OrderSuspensionMarked*/,
            isResumed = true))
        else
          Right(copy(
            mark = Some(OrderMark.Resuming(None, Vector.empty, false)),
            isResumed = true))

      case OrderResumed(maybePosition, historyOps, asSucceeded) =>
        import OrderResumed.{AppendHistoricOutcome, DeleteHistoricOutcome, InsertHistoricOutcome, ReplaceHistoricOutcome}
        val updatedHistoryOutcomes =
          if (maybePosition.isEmpty && historyOps.isEmpty)
            Right(historicOutcomes)
          else {
            // historyOutcomes positions should be unique, but is this sure?
            final class Entry(h: Option[HistoricOutcome]) {
              val inserted = mutable.Buffer.empty[HistoricOutcome]
              var current: Option[HistoricOutcome] = h
              def result = inserted.view ++ current
            }
            var positionFound = false
            val array = historicOutcomes.view
              .map { h =>
                for (pos <- maybePosition) {
                  if (!positionFound && (h.position == pos || h.position.normalized == pos)) {
                    positionFound = true
                  }
                }
                new Entry(!positionFound ? h)
              }
              .concat(new Entry(None) :: Nil)
              .toArray
            val append = mutable.Buffer.empty[HistoricOutcome]
            val pToi = historicOutcomes.view
              .zipWithIndex
              .map { case (h, i) => h.position -> i }
              .toMap
            var checked: Checked[Unit] = Right(())

            def get(pos: Position): Option[Int] =
              pToi.checked(pos) match {
                case Left(problem) =>
                  checked = Left(problem)
                  None
                case Right(i) =>
                  Some(i)
              }

            historyOps foreach {
              case ReplaceHistoricOutcome(pos, o) =>
                for (i <- get(pos)) {
                  array(i).current = Some(HistoricOutcome(pos, o))
                }

              case InsertHistoricOutcome(pos, newPos, o) =>
                for (i <- get(pos)) {
                  array(i).inserted += HistoricOutcome(newPos, o)
                }

              case AppendHistoricOutcome(pos, o) =>
                append += HistoricOutcome(pos, o)

              case DeleteHistoricOutcome(pos) =>
                for (i <- get(pos)) {
                  array(i).current = None
                }
            }
            checked.map(_ =>
              array.view
                .flatMap(_.result)
                .concat(append)
                .toVector)
          }

        updatedHistoryOutcomes.flatMap { updatedHistoricOutcomes =>
          val maybeSucceeded =
            (asSucceeded && !historicOutcomes.lastOption.forall(_.outcome.isSucceeded)) ?
              HistoricOutcome(position, Outcome.succeeded)
          check(isResumable,
            withPosition(maybePosition getOrElse position)
              .copy(
                isSuspended = false,
                isResumed = true,
                mark = None,
                state =
                  if (/*isState[FailedWhileFresh] ||*/ isState[StoppedWhileFresh]) // ???
                    Fresh
                  else if (isState[Failed] || isState[Stopped] || isState[Broken])
                    Ready
                  else
                    state,
                historicOutcomes = updatedHistoricOutcomes ++ maybeSucceeded))
        }

      case _: OrderLocksAcquired =>
        // LockState handles this event, too
        check(isDetached && (isState[Ready] || isState[WaitingForLock]),
          withPosition(position / BranchId.Lock % 0)
            .copy(
              state = Ready))

      case _: OrderLocksReleased =>
        // LockState handles this event, too
        if (force || isDetached /*&& isOrderFailedApplicable/*because it may come with OrderFailed*/*/)
          position
            .checkedParent
            .map(pos => withPosition(pos.increment))
        else
          inapplicable

      case _: OrderLocksQueued =>
        check(isDetached && isState[Ready],
          copy(
            state = WaitingForLock))

      case _: OrderLocksDequeued =>
        check(isDetached && isState[WaitingForLock],
          copy(
            state = Ready))

      case e: LegacyOrderLockEvent =>
        Left(EventNotApplicableProblem(id <-: e, this))

      case _: OrderNoticePostedV2_3 =>
        check(isDetached && isState[Ready] && !isSuspendedOrStopped,
          this)

      case _: OrderNoticePosted =>
        check(isDetached && isState[Ready] && !isSuspendedOrStopped,
          this)

      case OrderNoticeExpected(_) =>
        // ControllerStateBuilder converts this State to OrderNoticesExpected
        throw new NotImplementedError("Order.OrderNoticeExpected")

      case OrderNoticesExpected(expectedSeq) =>
        check(isDetached && isState[Ready] && !isSuspendedOrStopped,
          copy(
            state = ExpectingNotices(expectedSeq)))

      case OrderNoticesRead =>
        check(isDetached && (isState[Ready] || isState[ExpectingNotices]) && !isSuspendedOrStopped,
          copy(
            state = Ready))

      case OrderNoticesConsumptionStarted(_) =>
        check(isDetached && (isState[Ready] || isState[ExpectingNotices]) && !isSuspendedOrStopped,
          withPosition(position / BranchId.ConsumeNotices % 0)
            .copy(
              state = Ready))

      case OrderNoticesConsumed(_) =>
        check(isDetached,
          position.checkedParent.map(parentPos =>
            withPosition(parentPos.increment)
              .copy(
                state = Ready))
        ).flatten

      case OrderStickySubagentEntered(agentPath, subagentSelectionId) =>
        check(isState[IsFreshOrReady]
          && (isAttached || isDetached)
          && !isSuspendedOrStopped
          && !stickySubagents.exists(_.agentPath == agentPath),
          withPosition(position / BranchId.StickySubagent % 0)
            .copy(
              stickySubagents = StickySubagent(agentPath, subagentSelectionId) :: stickySubagents))

      case OrderStickySubagentLeaved =>
        if ((isAttached || isDetached) && stickySubagents.nonEmpty)
          position.parent
            .toChecked(inapplicableProblem)
            .map(stickySubagentPosition =>
              withPosition(stickySubagentPosition.increment)
                .copy(
                  stickySubagents = stickySubagents.tail))
        else
          inapplicable

      case OrderPrompted(question) =>
        check(isDetached && isState[Ready],
          copy(state = Prompting(question)))

      case OrderPromptAnswered() =>
        check(isDetached && isState[Prompting],
          copy(
            state = Ready))
            //historicOutcomes = historicOutcomes :+ HistoricOutcome(position, outcome)))

      case _: OrderOrderAdded =>
        // See also ControllerState, ControllerStateBuilder
        check(isDetached && isState[Ready],
          this)

      case OrderCyclingPrepared(cycleState) =>
        check((isDetached || isAttached)
          & (isState[Ready] || isState[BetweenCycles])
          & !isSuspendedOrStopped,
          copy(
            state = BetweenCycles(Some(cycleState))))

      case OrderCycleStarted =>
        state match {
          case BetweenCycles(Some(cycleState)) =>
            val branchId = BranchId.cycle(
              cycleState.copy(
                next = cycleState.next))
            check((isDetached || isAttached) & !isSuspendedOrStopped,
              withPosition(position / branchId % 0)
                .copy(
                  state = Ready))

          case _ => inapplicable
        }

      case OrderCycleFinished(cycleState/*???*/) =>
        position.parent
          .toChecked(inapplicableProblem)
          .map(cyclePosition =>
            withPosition(cyclePosition)
              .copy(
                state = BetweenCycles(cycleState)))

      case OrderTransferred(workflowPosition) =>
        if (isDetached)
          Right(copy(workflowPosition = workflowPosition))
        else
          inapplicable
    }
  }

  def shouldFail: Boolean =
    isFailed && isFailable

  def isFailed: Boolean =
    lastOutcome match {
      // Do not fail but let ExecuteExecutor repeat the job:
      case Outcome.Disrupted(Outcome.Disrupted.ProcessLost(_)) => false

      // Let ExecuteExecutor handle this case (and fail then):
      case Outcome.Killed(_) => !isState[Processed]

      case o => !o.isSucceeded
    }

  def lastOutcome: Outcome =
    historicOutcomes.lastOption.fold_(Outcome.succeeded, _.outcome)

  def isFailable =
    !isSuspendedOrStopped &&
      (isDetached || isAttached) &&
      (state match {
        case _: IsFreshOrReady => true
        case _: Processed => true
        case _: ProcessingKilled => true
        case _: Broken => true
        case _ => false
      })

  def withPosition(to: Position): Order[S] = copy(
    workflowPosition = workflowPosition.copy(position = to))

  def maybeDelayedUntil: Option[Timestamp] =
    if (isState[Fresh])
      scheduledFor
    else
      state.maybeDelayedUntil

  // Test in OrderScopesTest
  /** The named values as seen at the current workflow position. */
  def namedValues(workflow: Workflow): MapView[String, Value] =
    workflow.orderParameterList.addDefaults(arguments)
      .orElseMapView(historicOutcomeView)

  private def historicOutcomeView: MapView[String, Value] =
    new MapView[String, Value] {
      def get(key: String) =
        historicOutcomes.view
          .reverse
          .collect {
            case HistoricOutcome(_, o: Outcome.Completed) => o.namedValues.get(key)
          }
          .flatten
          .headOption

      private lazy val nameToValue =
        historicOutcomes.view
          .collect {
            case HistoricOutcome(_, o: Outcome.Completed) => o.namedValues
          }
          .flatten
          .toMap

      def iterator = nameToValue.iterator
    }

  /** In JobScheduler 1, job results overwrote order arguments. */
  def v1CompatibleNamedValues(workflow: Workflow): NamedValues =
    historicOutcomeView
      .orElseMapView(workflow.orderParameterList.addDefaults(arguments))
      .toVector
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

  def isAtAgent(agentPath: AgentPath): Boolean =
    attachedState match {
      case Some(Attaching(`agentPath`)) => true
      case Some(Attached(`agentPath`)) => true
      case Some(Detaching(`agentPath`)) => true
      case _ => false
    }

  def isInDetachableState =
    isState[Fresh] ||
      isState[Ready] ||
      isState[Forked] ||
      isState[Processed] ||
      isState[ProcessingKilled] ||
      isState[BetweenCycles] ||
      isState[FailedWhileFresh] ||
      isState[Failed] ||
      isState[FailedInFork] ||
      isState[Broken]

  private def isMarkable =
    !isState[IsTerminated] && !isState[Deleted] ||
      isState[FailedInFork]/*when asynchronously marked on Agent*/

  def isCancelable =
    (isState[IsFreshOrReady] ||
     isState[ProcessingKilled] ||
     isState[WaitingForLock] ||
     isState[Prompting] ||
     isState[ExpectingNotices] ||
     isState[BetweenCycles] ||
     isState[FailedWhileFresh] ||
     isState[DelayedAfterError] ||
     isState[Failed] ||
     isState[Broken]) &&
    (isDetached || isAttached)

  private def cleanMark: Option[OrderMark] =
    mark match {
      case Some(OrderMark.Cancelling(CancellationMode.FreshOnly)) if isStarted => None
      case o => o
    }

  private def isMarked =
    mark.isDefined

  def isSuspendible =
    (isState[IsFreshOrReady] /*|| isState[DelayedAfterError]*/ || isState[ProcessingKilled] && isSuspendingWithKill) &&
    (isDetached || isAttached)

  private def isSuspending =
    mark.exists(_.isInstanceOf[OrderMark.Suspending])

  private[order] def isSuspendingWithKill = mark match {
    case Some(OrderMark.Suspending(SuspensionMode(Some(_: CancellationMode.Kill)))) => true
    case _ => false
  }

  def isSuspendedOrStopped: Boolean =
    isSuspended | isState[Stopped]

  def isResuming =
    mark.exists(_.isInstanceOf[OrderMark.Resuming])

  def isResumable =
    (isState[IsFreshOrReady] && isSuspendedOrStopped ||
      isState[Stopped] ||
      isState[StoppedWhileFresh] ||
      isState[Failed] && !isSuspendedOrStopped/*strict for test*/ && isDetached ||
      isState[Broken]
    ) &&
      (isDetached || isAttached)

  def isProcessable =
    isState[IsFreshOrReady] && !isSuspendedOrStopped && !isMarked

  /** Number of executions for this job (starting with 1). */
  def historicJobExecutionCount(jobKey: JobKey, workflow: Workflow): Int = {
    val x = Right(jobKey)
    historicOutcomes.view
      .map(o => workflow.positionToJobKey(o.position))
      .count(_ == x)
  }

  def agentToStickySubagent(agentPath: AgentPath): Option[StickySubagent] =
    stickySubagents.find(_.agentPath == agentPath)

  def toOrderAttachedToAgent: Checked[OrderAttachedToAgent] =
    checkedState[IsFreshOrReady].flatMap(order =>
      order.attachedState match {
        case Some(Attached(agentPath)) =>
          assertThat(!isState[Stopped])
          Right(OrderAttachedToAgent(
            workflowPosition,
            order.state,
            arguments,
            scheduledFor,
            externalOrderKey,
            historicOutcomes, agentPath, parent, mark,
            isSuspended = isSuspended,
            isResumed = isResumed,
            deleteWhenTerminated = deleteWhenTerminated,
            stickySubagents,
            stopPositions))
        case _ =>
          Left(Problem("OrderAttachedToAgent event requires an Attached order"))
    })
}

object Order
{
  def fromOrderAdded(id: OrderId, event: OrderAddedX): Order[Fresh] =
    Order(id,
      event.workflowId /: event.startPosition.getOrElse(Position.First),
      Fresh,
      event.arguments,
      event.scheduledFor, event.externalOrderKey,
      deleteWhenTerminated = event.deleteWhenTerminated,
      stopPositions = event.stopPositions)

  def fromOrderAttached(id: OrderId, event: OrderAttachedToAgent): Order[IsFreshOrReady] =
    Order(id, event.workflowPosition, event.state, event.arguments,
      event.scheduledFor,
      event.externalOrderKey,
      historicOutcomes = event.historicOutcomes,
      Some(Attached(event.agentPath)),
      event.parent, event.mark,
      isSuspended = event.isSuspended,
      isResumed = event.isResumed,
      deleteWhenTerminated = event.deleteWhenTerminated,
      stickySubagents = event.stickySubagents,
      stopPositions = event.stopPositions)

  sealed trait AttachedState
  object AttachedState {
    sealed trait HasAgentPath extends AttachedState {
      def agentPath: AgentPath
    }
    object HasAgentPath {
      def unapply(o: HasAgentPath) = Some(o.agentPath)
    }
    implicit val jsonCodec: TypedJsonCodec[AttachedState] = TypedJsonCodec(
      Subtype(deriveCodec[Attaching]),
      Subtype(deriveCodec[Attached]),
      Subtype(deriveCodec[Detaching]))

    sealed trait AttachingOrAttached extends HasAgentPath
  }
  /** Order is going to be attached to an Agent. */
  final case class Attaching(agentPath: AgentPath) extends AttachedState.AttachingOrAttached {
    override def toString = s"Attaching to $agentPath"
  }
  /** Order is attached to an Agent. */
  final case class Attached(agentPath: AgentPath) extends AttachedState.AttachingOrAttached {
    override def toString = s"Attached to $agentPath"
  }
  /** Order is going to be detached from Agent. */
  final case class Detaching(agentPath: AgentPath) extends AttachedState.HasAgentPath {
    override def toString = s"Detaching from $agentPath"
  }

  sealed trait State {
    def maybeDelayedUntil: Option[Timestamp] = None

    /** Only if OrderOperationCancellable applies. */
    def isOperationCancelable = false
  }

  object State {
    implicit val jsonCodec: TypedJsonCodec[State] = TypedJsonCodec(
      Subtype[IsFreshOrReady],
      Subtype(deriveCodec[Processing]),
      Subtype(Processed),
      Subtype(ProcessingKilled),
      Subtype(deriveCodec[DelayedAfterError]),
      Subtype(deriveCodec[Forked]),
      Subtype(WaitingForLock),
      Subtype(deriveCodec[ExpectingNotice]), // Is being converted to ExpectingNotices
      Subtype(deriveCodec[ExpectingNotices]),
      Subtype(deriveCodec[BetweenCycles]),
      Subtype(Failed),
      Subtype(FailedWhileFresh),
      Subtype(FailedInFork),
      Subtype(Stopped),
      Subtype(StoppedWhileFresh),
      Subtype(Finished),
      Subtype(Cancelled),
      Subtype(Deleted),
      Subtype(deriveCodec[Prompting]),
      Subtype(deriveCodec[Broken]))
  }

  /** OrderStarted occurred. */
  sealed trait IsStarted extends State

  sealed trait IsFreshOrReady extends State

  /** Terminal state — the order can only be removed. */
  sealed trait IsTerminated extends State

  type Fresh = Fresh.type
  case object Fresh extends IsFreshOrReady

  type Ready = Ready.type
  case object Ready extends IsStarted with IsFreshOrReady

  final case class DelayedAfterError(until: Timestamp) extends IsStarted {
    override def maybeDelayedUntil = Some(until)
  }

  final case class Broken(problem: Option[Problem]) extends IsStarted/*!!!*/
  object Broken {
    // COMPATIBLE with v2.4
    @deprecated("outcome is deprecated", "v2.5")
    def apply(problem: Problem): Broken =
      Broken(Some(problem))

    def apply(): Broken =
      Broken(None)
  }

  final case class Processing(subagentId: Option[SubagentId]) extends IsStarted {
    override def toString =
      s"Processing(${subagentId getOrElse "legacy local Subagent"})"
  }
  object Processing {
    // Since v2.3
    def apply(subagentId: SubagentId): Processing =
      Processing(Some(subagentId))
  }

  type Processed = Processed.type
  case object Processed extends IsStarted

  type ProcessingKilled = ProcessingKilled.type
  case object ProcessingKilled extends IsStarted

  final case class Forked(children: Vector[Forked.Child]) extends IsStarted {
    def childOrderIds = children.view.map(_.orderId)
  }
  object Forked {
    type Child = OrderForked.Child
    val Child = OrderForked.Child
  }

  type WaitingForLock = WaitingForLock.type
  case object WaitingForLock
  extends IsStarted

  // COMPATIBLE with v2.3, only used for JSON deserialization
  final case class ExpectingNotice(noticeId: NoticeId)
  extends IsStarted

  final case class ExpectingNotices(expected: Vector[OrderNoticesExpected.Expected])
  extends IsStarted

  final case class Prompting(question: Value)
  extends IsStarted {
    override def isOperationCancelable = true
  }

  final case class BetweenCycles(cycleState: Option[CycleState])
  extends IsStarted
  {
    override def maybeDelayedUntil =
      cycleState.map(_.next)
  }

  type Failed = Failed.type
  case object Failed extends IsStarted

  type FailedWhileFresh = FailedWhileFresh.type
  case object FailedWhileFresh extends State

  type FailedInFork = FailedInFork.type
  case object FailedInFork extends IsStarted //with IsTerminated

  type Stopped = Stopped.type
  case object Stopped extends IsStarted

  type StoppedWhileFresh = StoppedWhileFresh.type
  case object StoppedWhileFresh extends IsStarted

  type Finished = Finished.type
  case object Finished extends IsStarted with IsTerminated

  type Cancelled = Cancelled.type
  // Position may be in a lock, but the lock has been released.
  // Just in case, Cancelled is being made resumable: Do not resume from within a lock instruction
  // or add position to Cancelled like in Failed!
  case object Cancelled extends IsTerminated

  type Deleted = Deleted.type
  case object Deleted extends State

  implicit val FreshOrReadyJsonCodec: TypedJsonCodec[IsFreshOrReady] = TypedJsonCodec[IsFreshOrReady](
    Subtype(Fresh),
    Subtype(Ready))

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
      "isResumed" -> order.isResumed.?.asJson,
      "stickySubagents" -> (order.stickySubagents.nonEmpty ? order.stickySubagents).asJson,
      "stopPositions" -> (order.stopPositions.nonEmpty ? order.stopPositions).asJson,
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
      isResumed <- cursor.getOrElse[Boolean]("isResumed")(false)
      deleteWhenTerminated <- cursor.getOrElse[Boolean]("deleteWhenTerminated")(false)
      stopPositions <- cursor.getOrElse[Set[PositionOrLabel]]("stopPositions")(Set.empty)
      stickySubagentId <- cursor.getOrElse[List[StickySubagent]]("stickySubagents")(Nil)
      historicOutcomes <- cursor.getOrElse[Vector[HistoricOutcome]]("historicOutcomes")(Vector.empty)
    } yield
      Order(id, workflowPosition, state, arguments, scheduledFor, externalOrderKey, historicOutcomes,
        attachedState, parent, mark,
        isSuspended = isSuspended,
        isResumed = isResumed,
        deleteWhenTerminated = deleteWhenTerminated,
        stickySubagentId,
        stopPositions)

  implicit val FreshOrReadyOrderJsonEncoder: Encoder.AsObject[Order[IsFreshOrReady]] =
    o => jsonEncoder.encodeObject(o)

  implicit val FreshOrReadyOrderJsonDecoder: Decoder[Order[IsFreshOrReady]] = cursor =>
    jsonDecoder(cursor) flatMap {
      o => o.ifState[IsFreshOrReady] match {
        case None => Left(DecodingFailure(
          s"Order is not Fresh or Ready, but: ${o.state.getClass.simpleScalaName}", cursor.history))
        case Some(x) => Right(x)
      }
    }

  implicit val ProcessingOrderJsonEncoder: Encoder.AsObject[Order[Processing]] =
    o => jsonEncoder.encodeObject(o)

  implicit val ProcessingOrderJsonDecoder: Decoder[Order[Processing]] = c =>
    jsonDecoder(c).flatMap {
      o => o.ifState[Processing] match {
        case None => Left(DecodingFailure(
          s"Order is not Fresh or Ready, but: ${o.state.getClass.simpleScalaName}",
          c.history))
        case Some(x) => Right(x)
      }
    }

  final case class StickySubagent(
    agentPath: AgentPath,
    subagentSelectionId: Option[SubagentSelectionId],
    stuckSubagentId: Option[SubagentId] = None)
  object StickySubagent {
    implicit val jsonCodec: Codec[StickySubagent] = deriveCodec
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
