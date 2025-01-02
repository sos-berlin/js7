package js7.data.order

import cats.syntax.option.*
import io.circe.generic.semiauto.deriveCodec
import io.circe.syntax.EncoderOps
import io.circe.{Codec, Decoder, DecodingFailure, Encoder, JsonObject}
import js7.base.circeutils.CirceUtils
import js7.base.circeutils.CirceUtils.{deriveCodecWithDefaults, deriveConfiguredCodec, deriveRenamingCodec, deriveRenamingDecoder}
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.problem.Checked.{CheckedOption, Ops}
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime.ZeroDuration
import js7.base.time.Timestamp
import js7.base.utils.Assertions.assertThat
import js7.base.utils.ScalaUtils.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.typeclasses.IsEmpty.syntax.*
import js7.data.Problems.{GoOrderInapplicableProblem, OrderCannotAttachedToPlanProblem}
import js7.data.agent.AgentPath
import js7.data.board.NoticeId
import js7.data.command.{CancellationMode, SuspensionMode}
import js7.data.event.EventDrivenState.EventNotApplicableProblem
import js7.data.event.{EventDriven, EventDrivenState}
import js7.data.job.JobKey
import js7.data.order.Order.*
import js7.data.order.OrderEvent.{OrderMoved, *}
import js7.data.orderwatch.{ExternalOrderKey, ExternalOrderName, OrderWatchPath}
import js7.data.plan.PlanId
import js7.data.subagent.{SubagentBundleId, SubagentId}
import js7.data.value.{NamedValues, Value}
import js7.data.workflow.position.BranchPath.syntax.*
import js7.data.workflow.position.{BranchId, BranchPath, InstructionNr, Position, PositionOrLabel, WorkflowPosition}
import js7.data.workflow.{Workflow, WorkflowId, WorkflowPath}
import scala.annotation.tailrec
import scala.collection.{MapView, mutable}
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
final case class Order[+S <: Order.State](
  id: OrderId,
  workflowPosition: WorkflowPosition,
  state: S,
  arguments: NamedValues = Map.empty,
  maybePlanId: Option[PlanId] = None,
  scheduledFor: Option[Timestamp] = None,
  externalOrder: Option[ExternalOrderLink] = None,
  historicOutcomes: Vector[HistoricOutcome] = Vector.empty,
  attachedState: Option[AttachedState] = None,
  parent: Option[OrderId] = None,
  mark: Option[OrderMark] = None,
  isSuspended: Boolean = false,
  isResumed: Boolean = false,
  deleteWhenTerminated: Boolean = false,
  forceJobAdmission: Boolean = false,
  stickySubagents: List[StickySubagent] = Nil,
  innerBlock: BranchPath = BranchPath.empty,
  stopPositions: Set[PositionOrLabel] = Set.empty)
extends
  EventDriven[Order[Order.State], OrderCoreEvent],
  MinimumOrder:

  // Accelerate usage in Set[Order], for example in AgentDriver's CommandQueue
  override def hashCode: Int = id.hashCode

  def companion: Order.type = Order

  def newForkedOrders(event: OrderForked): Vector[Order[Ready]] =
    event.children.map: child =>
      Order(
        child.orderId,
        workflowPosition.copy(position = workflowPosition.position /
          child.branchId.fold(BranchId.ForkList)(_.toBranchId) % InstructionNr.First),
        Ready,
        arguments ++ child.arguments,
        maybePlanId,
        scheduledFor = scheduledFor,
        historicOutcomes = historicOutcomes,
        attachedState = attachedState,
        parent = Some(id),
        stickySubagents = stickySubagents,
        forceJobAdmission = forceJobAdmission)

  def planId: PlanId =
    maybePlanId getOrElse PlanId.Global

  def workflowId: WorkflowId =
    workflowPosition.workflowId

  def workflowPath: WorkflowPath =
    workflowId.path

  def position: Position =
    workflowPosition.position

  def forkPosition: Checked[Position] =
    val reversed = position.forkBranchReversed
    if reversed.isEmpty then
      Left(Problem.pure(s"$id is in state FailedInFork but not below a Fork instruction"))
    else
      Right(reversed.tail.reverse % reversed.head.nr)

  def hasNonVanishedExternalOrder: Boolean =
    externalOrder.exists(o => !o.vanished)

  def applyEvents(events: Iterable[OrderEvent.OrderCoreEvent]): Checked[Order[State]] =
    // TODO Generalize this in ScalaUtils!
    var problem: Problem | Null = null
    var order: Order[Order.State] = this
    val it = events.iterator
    while it.hasNext && problem == null do
      val event = it.next()
      order.applyEvent(event) match
        case Right(o) => order = o
        case Left(prblm) => problem = prblm
    problem.toLeftOr(order)

  def applyEvent(event: OrderEvent.OrderCoreEvent): Checked[Order[State]] =
    def inapplicableProblem = InapplicableOrderEventProblem(event, this)
    def inapplicable = Left(inapplicableProblem)

    def check[A](okay: => Boolean, updated: A) =
      if okay then Right(updated) else inapplicable

    event match
      case _: OrderAdded | _: OrderAttachedToAgent =>
        Left(Problem:
          "OrderAdded and OrderAttachedToAgent events are not handled by the Order itself")

      case OrderStarted =>
        check(isState[Fresh] && !isSuspendedOrStopped && isDetachedOrAttached,
          copy(state = Ready))

      case OrderProcessingStarted(subagentId, subagentBundleId, stick) =>
        check(isState[Ready] && !isSuspendedOrStopped && isAttached
          && (!stick || stickySubagents.nonEmpty),
          copy(
            state = Processing(subagentId, subagentBundleId),
            stickySubagents =
              if stick then
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
        check((isFailable || isState[BetweenCycles]/*for ResetAgent*/) && isDetached,
          copy(
            state = if isState[Fresh] then FailedWhileFresh else Failed,
            workflowPosition = workflowPosition.copy(position = movedTo),
            mark = mark match
              case Some(_: OrderMark.Suspending) => None
              case o => o,
            historicOutcomes = outcome_.fold(historicOutcomes): outcome =>
              historicOutcomes :+ HistoricOutcome(position, outcome)))

      case OrderFailedInFork(movedTo, outcome) =>
        check(parent.nonEmpty
          && isFailable
          && !isState[Fresh],
          copy(
            state = FailedInFork,
            workflowPosition = workflowPosition.copy(position = movedTo),
            //Same as for OrderFailed? How to suspend a failed child order???
            // See SuspendResumeOrdersTest
            // mark = mark match {
            //   case Some(_: OrderMark.Suspending) => None
            //   case o => o
            // },
            isResumed = false,
            historicOutcomes = outcome.fold(historicOutcomes): outcome =>
              historicOutcomes :+ HistoricOutcome(position, outcome)))

      case OrderFailedIntermediate_(_) =>
        inapplicable  // Intermediate event, internal only

      case OrderCatched(movedTo, outcome) =>
        check((isState[Ready] || isState[Processed] || isState[ProcessingKilled]) &&
          !isSuspendedOrStopped &&
          isDetachedOrAttached,
          copy(
            state = Ready,
            workflowPosition = workflowPosition.copy(position = movedTo),
            isResumed = false,
            historicOutcomes = outcome.fold(historicOutcomes): outcome =>
              historicOutcomes :+ HistoricOutcome(position, outcome)))

      case OrderCaught(movedTo, outcome) =>
        check((isState[Ready] || isState[Processed] || isState[ProcessingKilled]) &&
          !isSuspendedOrStopped &&
          isDetachedOrAttached,
          locally:
            var h = outcome.fold(historicOutcomes): outcome =>
              historicOutcomes :+ HistoricOutcome(position, outcome)
            if !h.lastOption.exists(_.outcome.isSucceeded) then
              h :+= HistoricOutcome(movedTo, OrderOutcome.Caught)
            copy(
              state = Ready,
              workflowPosition = workflowPosition.copy(position = movedTo),
              isResumed = false,
              historicOutcomes = h))

      case OrderRetrying(maybeDelayUntil, movedTo) =>
        check(isState[Ready] && !isSuspendedOrStopped && isDetachedOrAttached,
          maybeDelayUntil
            .fold[Order[State]](this/*Ready*/)(o => copy(
              state = DelayingRetry(o)))
            .pipeMaybe(movedTo):
              _.withPosition(_))

      case OrderAwoke =>
        check(
          (isState[Sleeping] || isState[DelayingRetry] || isState[DelayedAfterError])
            && !isSuspendedOrStopped
            && isDetachedOrAttached,
          copy(state = Ready))

      case OrderForked(children) =>
        check(isState[Ready] && !isSuspendedOrStopped && isDetachedOrAttached,
          copy(
            state = Forked(children),
            mark = cleanMark))

      case OrderJoined(outcome) =>
        check(isState[Forked] && !isSuspendedOrStopped && isDetached,
          copy(
            state = Processed,
            historicOutcomes = historicOutcomes :+ HistoricOutcome(position, outcome)))

      case OrderMoved(to, _) =>
        check(
          (isState[IsFreshOrReady] || isState[Processed] || isState[BetweenCycles] || isState[Sleeping])
            && isDetachedOrAttached,
          withPosition(to).copy(
            isResumed = false,
            state = if isState[Fresh] then state else Ready))

      case OrderFinished(maybeOutcome) =>
        check(isState[Ready] && isDetached && !isSuspendedOrStopped,
          copy(
            state = Finished,
            historicOutcomes = maybeOutcome.fold(historicOutcomes): outcome =>
              historicOutcomes :+ HistoricOutcome(position, outcome),
            mark = None,
            isResumed = false))

      case OrderDeletionMarked =>
        check(parent.isEmpty,
          copy(deleteWhenTerminated = true))

      case OrderExternalVanished =>
        check(parent.isEmpty,
          copy(
            externalOrder = externalOrder.map(_.copy(
              vanished = true))))

      case OrderDeleted =>
        check(isState[IsTerminated] && isDetached && parent.isEmpty,
          copy(state = Deleted))

      case OrderBroken(maybeProblem) =>
        check(!isState[IsTerminated],
          copy(
            state = Broken(maybeProblem),
            historicOutcomes = maybeProblem.fold(historicOutcomes): problem =>
              historicOutcomes :+ HistoricOutcome(position, OrderOutcome.Disrupted(problem))))

      case OrderAttachable(agentPath) =>
        check((isState[Fresh] || isState[Ready] || isState[Forked]) && isDetached,
          copy(attachedState = Some(Attaching(agentPath))))

      case OrderAttached(agentPath) =>
        attachedState match
          case Some(Attaching(`agentPath`)) =>
            check(isState[IsFreshOrReady] || isState[Forked],
              copy(attachedState = Some(Attached(agentPath))))
          case _ =>
            inapplicable

      case OrderDetachable =>
        attachedState match
          case Some(Attached(agentPath)) if state.isDetachable =>
            Right(copy(attachedState = Some(Detaching(agentPath))))
          case _ =>
            inapplicable

      case OrderDetached =>
        check(!isDetached && state.isDetachable,
          copy(attachedState = None))

      case OrderCancellationMarked(mode) =>
        check(isMarkable,
          copy(mark = Some(OrderMark.Cancelling(mode))))

      case OrderCancellationMarkedOnAgent =>
        Right(this)

      case OrderStateReset =>
        // Event precedes OrderCancelled in the same transaction,
        // maybe before some block-leaving events which rely on state == Ready.
        check(state.isInstanceOf[IsResettable],
          copy(
            state = Ready,
            mark = None))

      case OrderCancelled =>
        check(isCancelable && isDetached,
          copy(
            state = Cancelled,
            isSuspended = false,
            mark = None))

      case OrderSuspensionMarked(mode) =>
        check(isMarkable,
          copy(mark = Some(OrderMark.Suspending(mode))))

      case OrderSuspensionMarkedOnAgent =>
        Right(this)

      case OrderSuspended =>
        check(isSuspendibleNow && (isDetached || isSuspended/*already Suspended, to clean Resuming mark*/),
          copy(
            isSuspended = true,
            mark = None,
            state = if isSuspendingWithKill && isState[ProcessingKilled] then Ready else state))

      case OrderStopped =>
        check(isFailable && isDetached,
          copy(
            state = if isState[Fresh] then StoppedWhileFresh else Stopped))

      case OrderGoMarked(position) =>
        if isGoCommandable(position) && isAttached then
          Right(copy(
            mark = Some(OrderMark.Go(position))))
        else
          inapplicable

      case OrderGoes =>
        if isGoCommandable then
          Right:
            mark match
              case Some(_: OrderMark.Go) => copy(mark = None)
              case _ => this
        else
          inapplicable

      case OrderResumptionMarked(position, historyOperations, asSucceeded) =>
        if !isMarkable then
          inapplicable
        else if isSuspended then
          Right(copy(
            mark = Some(OrderMark.Resuming(position, historyOperations, asSucceeded)),
            isResumed = true))
        else if position.isDefined || historyOperations.nonEmpty then
            // Inhibited because we cannot be sure whether order will pass a fork barrier
          inapplicable
        else if !isSuspended && isSuspending then
          Right(copy(
            mark = None/*revert OrderSuspensionMarked*/,
            isResumed = true))
        else
          Right(copy(
            mark = Some(OrderMark.Resuming()),
            isResumed = true))

      case OrderResumed(maybePosition, historyOps, asSucceeded) =>
        import OrderResumed.{AppendHistoricOutcome, DeleteHistoricOutcome, InsertHistoricOutcome, ReplaceHistoricOutcome}
        val updatedHistoryOutcomes =
          if maybePosition.isEmpty && historyOps.isEmpty then
            Right(historicOutcomes)
          else
            // historyOutcomes positions should be unique, but is this sure?
            final class Entry(h: Option[HistoricOutcome]):
              val inserted = mutable.Buffer.empty[HistoricOutcome]
              var current = h
              def result = inserted.view ++ current
            var positionFound = false
            val array = historicOutcomes.view
              .map: h =>
                for pos <- maybePosition do
                  if !positionFound && (h.position == pos || h.position.normalized == pos) then
                    positionFound = true
                new Entry(!positionFound ? h)
              .concat(new Entry(None) :: Nil)
              .toArray
            val append = mutable.Buffer.empty[HistoricOutcome]
            val pToi = historicOutcomes.view
              .zipWithIndex
              .map { (h, i) => h.position -> i }
              .toMap
            var checked: Checked[Unit] = Right(())

            def get(pos: Position): Option[Int] =
              pToi.checked(pos) match
                case Left(problem) =>
                  checked = Left(problem)
                  None
                case Right(i) =>
                  Some(i)

            historyOps foreach:
              case ReplaceHistoricOutcome(pos, o) =>
                for i <- get(pos) do
                  array(i).current = Some(HistoricOutcome(pos, o))

              case InsertHistoricOutcome(pos, newPos, o) =>
                for i <- get(pos) do
                  array(i).inserted += HistoricOutcome(newPos, o)

              case AppendHistoricOutcome(pos, o) =>
                append += HistoricOutcome(pos, o)

              case DeleteHistoricOutcome(pos) =>
                for i <- get(pos) do
                  array(i).current = None
            checked.map: _ =>
              array.view
                .flatMap(_.result)
                .concat(append)
                .toVector

        updatedHistoryOutcomes.flatMap: updatedHistoricOutcomes =>
          val maybeSucceeded =
            (asSucceeded && !historicOutcomes.lastOption.forall(_.outcome.isSucceeded)) ?
              HistoricOutcome(position, OrderOutcome.succeeded)
          check(isResumableNow,
            withPosition(maybePosition getOrElse position)
              .copy(
                isSuspended = false,
                isResumed = true,
                mark = None,
                state =
                  if /*isState[FailedWhileFresh] ||*/ isState[StoppedWhileFresh] then // ???
                    Fresh
                  else if isState[Failed] || isState[Stopped] || isState[Broken] then
                    Ready
                  else
                    state,
                historicOutcomes = updatedHistoricOutcomes ++ maybeSucceeded))

      case _: OrderLocksAcquired =>
        // LockState handles this event, too
        check(isDetached && (isState[Ready] || isState[WaitingForLock]),
          withPosition(position / BranchId.Lock % 0)
            .copy(
              state = Ready))

      case _: OrderLocksReleased =>
        // LockState handles this event, too
        if isDetached /*&& isOrderFailedApplicable/*because it may come with OrderFailed*/*/ then
          position
            .checkedParent
            .map(pos => withPosition(pos.increment))
        else
          inapplicable

      case _: OrderLocksQueued =>
        check(isDetached && isState[Ready],
          copy(
            state = WaitingForLock))

      case e: LegacyOrderLockEvent =>
        Left(EventNotApplicableProblem(id <-: e, this))

      case _: OrderNoticeAnnounced =>
        check(isDetached && isState[Fresh],
          this)

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
          position.checkedParent.map: parentPos =>
            withPosition(parentPos.increment)
              .copy(
                state = Ready)
        ).flatten

      case OrderStickySubagentEntered(agentPath, subagentBundleId) =>
        check(isState[IsFreshOrReady]
          && isDetachedOrAttached
          && !isSuspendedOrStopped
          && !stickySubagents.exists(_.agentPath == agentPath),
          withPosition(position / BranchId.StickySubagent % 0)
            .copy(
              stickySubagents = StickySubagent(agentPath, subagentBundleId) :: stickySubagents))

      case OrderStickySubagentLeaved =>
        if isDetachedOrAttached && stickySubagents.nonEmpty then
          position.parent
            .toChecked(inapplicableProblem)
            .map: stickySubagentPosition =>
              withPosition(stickySubagentPosition.increment)
                .copy(
                  stickySubagents = stickySubagents.tail)
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
        check(isDetachedOrAttached
          & (isState[Ready] || isState[BetweenCycles])
          & !isSuspendedOrStopped,
          copy(
            state = BetweenCycles(Some(cycleState))))

      case OrderCycleStarted(maybeSkipped) =>
        state match
          case BetweenCycles(Some(cycleState)) =>
            val branchId = BranchId.cycle(
              cycleState.copy(
                next = cycleState.next + maybeSkipped.getOrElse(ZeroDuration)))
            check(isDetachedOrAttached & !isSuspendedOrStopped,
              withPosition(position / branchId % 0)
                .copy(
                  state = Ready))

          case _ => inapplicable

      case OrderCycleFinished(cycleState) =>
        position.parent
          .toChecked(inapplicableProblem)
          .map: cyclePosition =>
            withPosition(cyclePosition)
              .copy(
                state = BetweenCycles(cycleState))

      case OrderSleeping(until) =>
        check(isState[Ready] && isDetachedOrAttached,
          copy(
            state = Sleeping(until)))

      case OrderTransferred(workflowPosition) =>
        if isDetached then
          Right(copy(workflowPosition = workflowPosition))
        else
          inapplicable

      case OrderPlanAttached(planId) =>
        val ok = isDetached
          && !isState[ExpectingNotices]
          && !isState[ExpectingNotice]
          && position.branchPath.forall(_.branchId != BranchId.ConsumeNotices)
          && this.planId.isGlobal
          && !planId.isGlobal
        if !ok then
          Left(OrderCannotAttachedToPlanProblem(id))
        else
          Right(copy(
            maybePlanId = planId.some))

  /** An Order being transferred back to Controller, should fail after failure. */
  def shouldFail: Boolean =
    isFailed && isFailable

  def isFailed: Boolean =
    lastOutcome match
      // Do not fail but let ExecuteExecutor repeat the job:
      case OrderOutcome.Disrupted(OrderOutcome.Disrupted.ProcessLost(_), _) => false

      // Let ExecuteExecutor handle this case (and fail then):
      case OrderOutcome.Killed(_) => !isState[Processed]

      case o => !o.isSucceeded

  /** Looks through OrderCaught. */
  def hasTimedOut: Boolean =
    lastOutcome match
      case OrderOutcome.Caught =>
        historicOutcomes.get(historicOutcomes.length - 2)
          .exists(_._2.isInstanceOf[OrderOutcome.TimedOut])
      case outcome =>
        outcome.isInstanceOf[OrderOutcome.TimedOut]

  def lastOutcome: OrderOutcome =
    historicOutcomes.lastOption.fold_(OrderOutcome.succeeded, _.outcome)

  def isFailable: Boolean =
    !isSuspendedOrStopped &&
      isDetachedOrAttached &&
      state.match
        case _: IsFreshOrReady => true
        case _: Processed => true
        case _: ProcessingKilled => true
        case _: Broken => true
        case _ => false

  def withPosition(to: Position): Order[S] = copy(
    workflowPosition = workflowPosition.copy(position = to))

  /** Whether WorkflowPathControl skip is applicable. */
  def isSkippable(now: Timestamp): Boolean =
    state.match
      case Fresh => !isDelayed(now)
      case Ready => true
      case _: DelayingRetry | _: DelayedAfterError => !isDelayed(now)
      case _ => false
    && !isSuspended

  def isDelayed(now: Timestamp): Boolean =
    maybeDelayedUntil.exists(now < _)

  def maybeDelayedUntil: Option[Timestamp] =
    if isState[Fresh] then
      scheduledFor
    else
      state.maybeDelayedUntil

  // Test in OrderScopesTest
  /** The named values as seen at the current workflow position. */
  def namedValues(workflow: Workflow): MapView[String, Value] =
    workflow.orderParameterList.addDefaults(arguments)
      .orElseMapView(historicOutcomeView)

  private def historicOutcomeView: MapView[String, Value] =
    new MapView[String, Value]:
      def get(key: String) =
        historicOutcomes.view
          .reverse
          .collect:
            case HistoricOutcome(_, o: OrderOutcome.Completed) => o.namedValues.get(key)
          .flatten
          .headOption

      private lazy val nameToValue =
        historicOutcomes.view
          .collect:
            case HistoricOutcome(_, o: OrderOutcome.Completed) => o.namedValues
          .flatten
          .toMap

      def iterator = nameToValue.iterator

  /** In JobScheduler 1, job results overwrote order arguments. */
  def v1CompatibleNamedValues(workflow: Workflow): NamedValues =
    historicOutcomeView
      .orElseMapView(workflow.orderParameterList.addDefaults(arguments))
      .toVector
      .toMap

  def isStarted: Boolean =
    isState[IsStarted]

  def castState[A <: State: ClassTag]: Order[A] =
    checkedState[A].orThrow

  def checkedState[A <: State: ClassTag]: Checked[Order[A]] =
    Checked.fromOption(ifState[A], Problem(s"'$id' is expected to be in state ${implicitClass[A].simpleScalaName}, but is in state $state"))

  def ifState[A <: State: ClassTag]: Option[Order[A]] =
    isState[A] ? this.asInstanceOf[Order[A]]

  def isState[A <: State: ClassTag]: Boolean =
    implicitClass[A].isAssignableFrom(state.getClass)

  def markString: Option[String] =
    mark.map(o => s"marked as $o")

  def attachedStateString: String =
    attachedState match
      case None => "at Controller"
      case Some(Attaching(agentPath)) => s"attachable to $agentPath"
      case Some(Attached(agentPath)) => s"attached to $agentPath"
      case Some(Detaching(agentPath)) => s"detaching from $agentPath"

  def isDetachedOrAttached: Boolean =
    isDetached || isAttached

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

  def checkIsDetached: Checked[Unit] =
    attachedState.fold(Checked.unit): attached =>
      Left(Problem(s"$id is required to be detached, but it is $attached"))

  def attached: Checked[AgentPath] =
    attachedState match
      case Some(Attached(agentPath)) =>
        Right(agentPath)
      case o =>
        Left(Problem(s"'$id' should be 'Attached', but is $o"))

  def detaching: Checked[AgentPath] =
    attachedState match
      case Some(Detaching(agentPath)) =>
        Right(agentPath)
      case o =>
        Left(Problem(s"'$id' should be Detaching, but is $o"))

  def isAtAgent(agentPath: AgentPath): Boolean =
    attachedState match
      case Some(Attaching(`agentPath`)) => true
      case Some(Attached(`agentPath`)) => true
      case Some(Detaching(`agentPath`)) => true
      case _ => false

  def isDetachedOrDetachable: Boolean =
    isDetached || isDetachable

  def isDetachable: Boolean =
    isAttached && state.isDetachable

  /** OrderGoMarked and OrderGoes are applicable only if Order is in specific waiting states. */
  def isGoCommandable(position: Position): Boolean =
    isGoCommandable && this.position == position

  /** OrderGoMarked and OrderGoes are applicable only if Order is in specific waiting states. */
  def isGoCommandable: Boolean =
    isDetachedOrAttached &&
      isState[BetweenCycles]
      || isState[DelayingRetry]
      || isState[DelayedAfterError]
      || isState[Sleeping]
      || (isState[Fresh] && maybeDelayedUntil.isDefined)

  private def isMarkable =
    !isState[IsTerminated] && !isState[Deleted] ||
      isState[FailedInFork]/*when asynchronously marked on Agent*/

  def isCancelable(mode: CancellationMode): Boolean =
    isCancelable &&
      (mode != CancellationMode.FreshOnly ||
        isState[Order.Fresh] ||
        isState[Order.StoppedWhileFresh])

  def isCancelable: Boolean =
    (isState[IsFreshOrReady]
      || isState[ProcessingKilled]
      || isState[WaitingForLock]
      || isState[Prompting]
      || isState[ExpectingNotices]
      || isState[BetweenCycles]
      || isState[FailedWhileFresh]
      || isState[DelayingRetry]
      || isState[DelayedAfterError]
      || isState[Stopped]
      || isState[Failed]
      || isState[Broken]
    ) && isDetachedOrAttached

  private def cleanMark: Option[OrderMark] =
    mark match
      case Some(OrderMark.Cancelling(CancellationMode.FreshOnly)) if isStarted => None
      case o => o

  private def isMarked =
    mark.isDefined

  def isSuspendible: Boolean =
    !isState[IsTerminated] && !isState[IsFailed] &&
      mark.match
        case None => true
        case Some(_: OrderMark.Suspending | _: OrderMark.Resuming | _: OrderMark.Go) => true
        case Some(_: OrderMark.Cancelling) => false

  /** Order is immediately suspendible (no OrderMark Suspending required).
   * <p>
   * ❗️ Also true when isAttached and an OrderDetachable event may be required first.
   */
  def isSuspendibleNow: Boolean =
    isDetachedOrAttached &&
      (isState[IsFreshOrReady] || isState[ProcessingKilled] && isSuspendingWithKill)

  private def isSuspending =
    mark.exists(_.isInstanceOf[OrderMark.Suspending])

  private[order] def isSuspendingWithKill: Boolean =
    mark match
      case Some(OrderMark.Suspending(SuspensionMode(_, Some(_: CancellationMode.Kill)))) => true
      case _ => false

  def isSuspendedOrStopped: Boolean =
    (isSuspended
      && !isState[Cancelled]/*COMPATIBLE Before v2.6 OrderCancelled did not reset isSuspended*/
      || isState[Stopped])

  def isResumable: Boolean =
    isState[IsFreshOrReady] && isSuspendedOrStopped
      || isState[Stopped]

  def isResuming: Boolean =
    mark.exists(_.isInstanceOf[OrderMark.Resuming])

  def isResumableNow: Boolean =
    (isState[IsFreshOrReady] && isSuspendedOrStopped
      || isState[Stopped]
      || isState[StoppedWhileFresh]
      || isState[Failed] && !isSuspendedOrStopped/*strict for test*/ && isDetached
      || isState[Broken]
    ) && isDetachedOrAttached

  def isProcessable: Boolean =
    isState[IsFreshOrReady] && !isSuspendedOrStopped && !isMarked

  def isInOutermostBlock: Boolean =
    position.branchPath == innerBlock

  def tryDelete: Option[OrderDeleted] =
    ((deleteWhenTerminated || externalOrder.exists(_.vanished))
      && isState[IsTerminated]
      && parent.isEmpty
    ) ? OrderDeleted

  /** Number of executions for this job (starting with 1). */
  def historicJobExecutionCount(jobKey: JobKey, workflow: Workflow): Int =
    val x = Right(jobKey)
    historicOutcomes.view
      .map(o => workflow.positionToJobKey(o.position))
      .count(_ == x)

  def agentToStickySubagent(agentPath: AgentPath): Option[StickySubagent] =
    stickySubagents.find(_.agentPath == agentPath)

  def toOrderAttachedToAgent: Checked[OrderAttachedToAgent] =
    checkedState[IsFreshOrReady].flatMap: order =>
      order.attachedState match
        case Some(Attached(agentPath)) =>
          assertThat(!isState[Stopped])
          Right(OrderAttachedToAgent(
            workflowPosition,
            order.state,
            maybePlanId,
            arguments,
            scheduledFor,
            externalOrder,
            historicOutcomes, agentPath, parent, mark,
            isSuspended = isSuspended,
            isResumed = isResumed,
            deleteWhenTerminated = deleteWhenTerminated,
            forceJobAdmission = forceJobAdmission,
            stickySubagents,
            innerBlock, stopPositions))
        case _ =>
          Left(Problem("OrderAttachedToAgent event requires an Attached order"))

  def go: Checked[List[OrderActorEvent]] =
    ifState[IsGoCommandable].filter(_ => isDetachedOrAttached).map: order =>
      order.state.go(order.asInstanceOf[Order[order.state.Self]])
    .getOrElse:
      Left(GoOrderInapplicableProblem(id))

  def forceDetach(outcome: OrderOutcome.Disrupted)
  : Checked[(Vector[OrderCoreEvent], Order[Order.State])] =
    var _order = Checked(this: Order[State])
    val _events = Vector.newBuilder[OrderCoreEvent]

    for order <- _order do
      order.ifState[Order.Processing].foreach: _ =>
        val event = OrderProcessed(outcome)
        _events += event
        _order = order.applyEvent(event)

    for order <- _order do
      val event = OrderDetached
      _events += event
      _order = order.applyEvent(event)

    // Reset state to allow to fail the order
    for order <- _order do
      val events = order.resetState
      _events ++= events
      _order = order.applyEvents(events)

    for order <- _order do
      order.ifState[Order.DelayedAfterError].foreach: _ =>
        val event = OrderAwoke
        _events += event
        _order = order.applyEvent(event)

    _order.map(o => (_events.result(), o))

  /** Reset the Order's State if possible. */
  def resetState: List[OrderStateReset] =
    state match
      case state: IsResettable if isDetachedOrAttached =>
        state.reset
      case _ =>
        Nil

  def transfer(from: Workflow, to: Workflow): Checked[Seq[OrderActorEvent]] =
    @tailrec def checkSameParentInstructions(pos: Position): Checked[Unit] =
      pos.parent match
        case None => Checked.unit
        case Some(pos) =>
          if !from.instruction(pos).isSameCore(to.instruction(pos)) then
            Left(Problem:
              s"$id is not transferable because the surrounding ${
                from.instruction(pos).instructionName} instruction at position $pos has changed")
          else
            checkSameParentInstructions(pos)

    for
      _ <- to.checkPosition(position)
      _ <- checkIsDetached
      _ <- checkSameParentInstructions(position)
      prepare <- state.prepareTransfer(this, from, to)
    yield
      prepare :+ OrderTransferred(to.id /: position)

  override def toString =
    s"Order($id · $state${isSuspended ?? " · suspended"
    }${attachedState.fold("")(o => s" · $o")
    }${mark.fold("")(o => s" · $o")
    }${if stickySubagents.isEmpty then "" else s" · sticky=${stickySubagents.mkString}"
    } · $workflowPosition · ${historicOutcomes.size} outcomes: $lastOutcome)"


object Order extends EventDriven.Companion[Order[Order.State], OrderCoreEvent]:

  def fromOrderAdded(id: OrderId, event: OrderAddedX): Order[Fresh] =
    Order(id,
      event.workflowId /: event.startPosition.getOrElse(event.innerBlock % 0),
      Fresh,
      event.arguments,
      event.planId,
      event.scheduledFor,
      event.externalOrderKey.map(ExternalOrderLink.added),
      deleteWhenTerminated = event.deleteWhenTerminated,
      forceJobAdmission = event.forceJobAdmission,
      innerBlock = event.innerBlock,
      stopPositions = event.stopPositions)

  def fromOrderAttached(id: OrderId, event: OrderAttachedToAgent): Order[IsFreshOrReady] =
    Order(id, event.workflowPosition, event.state,
      event.arguments,
      event.planId,
      event.scheduledFor,
      event.externalOrder,
      historicOutcomes = event.historicOutcomes,
      Some(Attached(event.agentPath)),
      event.parent, event.mark,
      isSuspended = event.isSuspended,
      isResumed = event.isResumed,
      deleteWhenTerminated = event.deleteWhenTerminated,
      forceJobAdmission = event.forceJobAdmission,
      stickySubagents = event.stickySubagents,
      innerBlock = event.innerBlock,
      stopPositions = event.stopPositions)

  given Ordering[Order[? <: Order.State]] = Ordering.by(_.id)


  extension (order: Order[IsDelayingRetry])
    def awokeEventsIfRipe(ts: Timestamp): Checked[List[OrderAwoke | OrderMoved]] =
      if order.state.until <= ts then
        awokeEvents
      else
        Right(Nil)

    def awokeEvents: Checked[List[OrderAwoke | OrderMoved]] =
      if !order.isDetachedOrAttached then
        Left(InapplicableOrderEventProblem(OrderAwoke, order))
      else
        order.state match
          case _: DelayingRetry =>
            order.position.nextRetryPosition.map: pos =>
              OrderAwoke :: OrderMoved(pos) :: Nil
          case _: DelayedAfterError =>
            Right(OrderAwoke :: Nil)


  sealed trait AttachedState

  object AttachedState:
    sealed trait HasAgentPath extends AttachedState:
      def agentPath: AgentPath
    object HasAgentPath:
      def unapply(o: HasAgentPath): Some[AgentPath] = Some(o.agentPath)
    implicit val jsonCodec: TypedJsonCodec[AttachedState] = TypedJsonCodec(
      Subtype(deriveCodec[Attaching]),
      Subtype(deriveCodec[Attached]),
      Subtype(deriveCodec[Detaching]))

    sealed trait AttachingOrAttached extends HasAgentPath


  /** Order is going to be attached to an Agent. */
  final case class Attaching(agentPath: AgentPath) extends AttachedState.AttachingOrAttached:
    override def toString = s"Attaching to $agentPath"


  /** Order is attached to an Agent. */
  final case class Attached(agentPath: AgentPath) extends AttachedState.AttachingOrAttached:
    override def toString = s"Attached to $agentPath"


  /** Order is going to be detached from Agent. */
  final case class Detaching(agentPath: AgentPath) extends AttachedState.HasAgentPath:
    override def toString = s"Detaching from $agentPath"


  sealed trait State:
    private[Order] def isDetachable: Boolean

    private[Order] def maybeDelayedUntil: Option[Timestamp] = None

    private[Order] def prepareTransfer(order: Order[State], from: Workflow, to: Workflow)
    : Checked[Seq[OrderActorEvent]]


  object State:
    implicit val jsonCodec: TypedJsonCodec[State] = TypedJsonCodec(
      Subtype[IsFreshOrReady],
      Subtype(deriveCodec[Processing]),
      Subtype(Processed),
      Subtype(ProcessingKilled),
      Subtype(deriveCodec[DelayingRetry]),
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
      Subtype(deriveCodec[Sleeping]),
      Subtype(deriveCodec[Broken]))

  sealed trait IsDetachable extends State:
    private[Order] final def isDetachable = true

  sealed trait IsNotDetachable extends State:
    private[Order] final def isDetachable = false

  sealed trait IsControllerOnly extends IsNotDetachable


  sealed trait IsGoCommandable extends State:
    type Self <: State

    def go(order: Order[Self])
    : Checked[List[OrderGoes | OrderStarted | OrderCycleStarted | OrderAwoke | OrderMoved]]


  sealed trait IsResettable extends State:
    this: IsStarted => // Only for IsStarted states. Order will become Ready.

    private[Order] final def reset: List[OrderStateReset] =
      OrderStateReset :: Nil


  sealed trait NotTransferable extends State:
    private[Order] final def prepareTransfer(order: Order[State], from: Workflow, to: Workflow) =
      Left(Problem:
        s"${order.id} in ${getClass.simpleScalaName} state (${
          from.instruction(order.position).getClass.simpleScalaName
        } instruction) cannot be transferred")


  sealed trait IsTransferableOnlyIfInstructionUnchanged extends State:
    private[Order] final def prepareTransfer(order: Order[State], from: Workflow, to: Workflow) =
      if !from.instruction(order.position).isSameCore(to.instruction(order.position)) then
        Left(Problem:
          s"${order.id} in ${getClass.simpleScalaName} state cannot be transferred because statement has changed")
      else
        Right(Nil)


  sealed trait IsTransferableButResetChangedInstruction:
    this: IsResettable =>

    private[Order] final def prepareTransfer(order: Order[Order.State], from: Workflow, to: Workflow) =
      Right:
        if !from.instruction(order.position).isSameCore(to.instruction(order.position)) then
          reset
        else
          Nil


  sealed trait IsTransferable extends State:
    private[Order] final def prepareTransfer(order: Order[State], from: Workflow, to: Workflow) =
      Right(Nil)


  /** OrderStarted occurred. */
  sealed trait IsStarted extends State

  sealed trait IsFreshOrReady extends State

  /** Terminal state — the order can only be removed. */
  sealed trait IsTerminated extends IsControllerOnly

  sealed trait IsFailed extends IsControllerOnly


  type Fresh = Fresh.type
  case object Fresh extends IsFreshOrReady, IsDetachable, IsGoCommandable, IsTransferable:
    type Self = Fresh

    def go(order: Order[Fresh])
    : Either[GoOrderInapplicableProblem, List[OrderGoes | OrderStarted]] =
      if order.maybeDelayedUntil.isDefined then
        Right:
          OrderGoes :: OrderStarted :: Nil
      else
        Left(GoOrderInapplicableProblem(order.id))


  type Ready = Ready.type
  case object Ready extends IsStarted, IsFreshOrReady, IsDetachable, IsTransferable


  // COMPATIBLE with v2.7.1
  sealed trait IsDelayingRetry extends IsStarted, IsDetachable, IsGoCommandable, IsTransferable:
    def until: Timestamp


  // COMPATIBLE with v2.7.1
  final case class DelayedAfterError(until: Timestamp) extends IsDelayingRetry:
    type Self = DelayedAfterError

    override private[Order] def maybeDelayedUntil = Some(until)

    def go(order: Order[DelayedAfterError]): Right[Nothing, List[OrderGoes | OrderAwoke]] =
      Right:
        OrderGoes :: OrderAwoke :: Nil


  final case class DelayingRetry(until: Timestamp) extends IsDelayingRetry, IsResettable:
    type Self = DelayingRetry

    override private[Order] def maybeDelayedUntil = Some(until)

    def go(order: Order[DelayingRetry]): Checked[List[OrderGoes | OrderAwoke | OrderMoved]] =
      order.awokeEvents.map:
        _.whenNonEmpty:
          OrderGoes :: _

  final case class Broken(problem: Option[Problem])
  extends IsStarted/*!!!*/, IsDetachable, IsTransferable

  object Broken:
    // COMPATIBLE with v2.4
    @deprecated("outcome is deprecated", "v2.5")
    def apply(problem: Problem): Broken =
      Broken(Some(problem))

    def apply(): Broken =
      Broken(None)


  final case class Processing(
    subagentId: Option[SubagentId],
    subagentBundleId: Option[SubagentBundleId])
  extends IsStarted, IsNotDetachable, IsTransferableOnlyIfInstructionUnchanged:
    override def toString = s"Processing(${subagentId getOrElse
        "legacy local Subagent"}${subagentBundleId.fold("")(o => s" $o")})"

  object Processing:
    def legacy: Processing = new Processing(None, None)

    // Since v2.3
    def apply(subagentId: SubagentId, bundleId: Option[SubagentBundleId] = None): Processing =
      Processing(Some(subagentId), bundleId)


  type Processed = Processed.type
  case object Processed extends IsStarted, IsDetachable, IsTransferable


  type ProcessingKilled = ProcessingKilled.type
  case object ProcessingKilled extends IsStarted, IsDetachable, IsTransferable


  final case class Forked(children: Vector[Forked.Child])
  extends IsStarted, IsDetachable, IsTransferableOnlyIfInstructionUnchanged:
    def childOrderIds: Vector[OrderId] = children.map(_.orderId)

  object Forked:
    type Child = OrderForked.Child
    val Child: OrderForked.Child.type = OrderForked.Child


  type WaitingForLock = WaitingForLock.type
  case object WaitingForLock
  extends IsStarted, IsControllerOnly, IsResettable, IsTransferableButResetChangedInstruction


  // COMPATIBLE with v2.3, only used for JSON deserialization
  final case class ExpectingNotice(noticeId: NoticeId)
  extends IsStarted, IsControllerOnly, NotTransferable

  final case class ExpectingNotices(expected: Vector[OrderNoticesExpected.Expected])
  extends IsStarted, IsControllerOnly, IsResettable, IsTransferableButResetChangedInstruction


  final case class Prompting(question: Value)
  extends IsStarted, IsControllerOnly, IsResettable, IsTransferableButResetChangedInstruction


  final case class BetweenCycles(cycleState: Option[CycleState])
  extends IsStarted, IsDetachable, IsGoCommandable, IsResettable, IsTransferableButResetChangedInstruction:
    type Self = BetweenCycles

    override private[Order] def maybeDelayedUntil =
      cycleState.map(_.next)

    def go(order: Order[BetweenCycles]) =
      Right:
        OrderGoes :: OrderCycleStarted() :: Nil


  final case class Sleeping(until: Timestamp)
  extends IsStarted, IsDetachable, IsGoCommandable, IsResettable, IsTransferable:
    type Self = Sleeping

    override private[Order] def maybeDelayedUntil = Some(until)

    def go(order: Order[Sleeping]): Right[Problem, List[OrderGoes | OrderAwoke | OrderMoved]] =
      Right:
        List(OrderGoes, OrderAwoke, OrderMoved(order.position.increment))


  type Failed = Failed.type
  case object Failed extends IsStarted, IsFailed, IsTransferable


  type FailedWhileFresh = FailedWhileFresh.type
  case object FailedWhileFresh extends IsFailed, IsTransferable


  type FailedInFork = FailedInFork.type
  case object FailedInFork
  extends IsStarted, IsFailed, IsTransferable //, IsTerminated


  type Stopped = Stopped.type
  case object Stopped extends IsStarted, IsControllerOnly, IsTransferable


  type StoppedWhileFresh = StoppedWhileFresh.type
  case object StoppedWhileFresh extends IsStarted, IsControllerOnly, IsTransferable


  type Finished = Finished.type
  case object Finished extends IsStarted, IsTerminated, IsTransferable


  type Cancelled = Cancelled.type
  // Position may be in a lock, but the lock has been released.
  // Just in case, Cancelled is being made resumable: Do not resume from within a lock instruction
  // or add position to Cancelled like in Failed!
  case object Cancelled extends IsTerminated, IsTransferable


  type Deleted = Deleted.type
  case object Deleted extends IsControllerOnly, IsTransferable


  implicit val FreshOrReadyJsonCodec: TypedJsonCodec[IsFreshOrReady] = TypedJsonCodec[IsFreshOrReady](
    Subtype(Fresh),
    Subtype(Ready))

  implicit val jsonEncoder: Encoder.AsObject[Order[State]] = order =>
    JsonObject(
      "id" -> order.id.asJson,
      "workflowPosition" -> order.workflowPosition.asJson,
      "state" -> order.state.asJson,
      "arguments" -> order.arguments.??.asJson,
      "planId" -> order.maybePlanId.asJson,
      "scheduledFor" -> order.scheduledFor.asJson,
      "externalOrder" -> order.externalOrder.asJson,
      "attachedState" -> order.attachedState.asJson,
      "parent" -> order.parent.asJson,
      "mark" -> order.mark.asJson,
      "isSuspended" -> order.isSuspended.?.asJson,
      "deleteWhenTerminated" -> order.deleteWhenTerminated.?.asJson,
      "forceJobAdmission" -> order.forceJobAdmission.?.asJson,
      "isResumed" -> order.isResumed.?.asJson,
      "stickySubagents" -> (order.stickySubagents.nonEmpty ? order.stickySubagents).asJson,
      "innerBlock" -> (order.innerBlock.nonEmpty ? order.innerBlock).asJson,
      "stopPositions" -> (order.stopPositions.nonEmpty ? order.stopPositions).asJson,
      "historicOutcomes" -> order.historicOutcomes.??.asJson)

  implicit val jsonDecoder: Decoder[Order[State]] = cursor =>
    for
      id <- cursor.get[OrderId]("id")
      workflowPosition <- cursor.get[WorkflowPosition]("workflowPosition")
      state <- cursor.get[State]("state")
      arguments <- cursor.getOrElse[NamedValues]("arguments")(NamedValues.empty)
      planId <- cursor.get[Option[PlanId]]("planId")
      scheduledFor <- cursor.get[Option[Timestamp]]("scheduledFor")
      externalOrder <- cursor.get[Option[ExternalOrderLink]]("externalOrder")
        .flatMap:
          case Some(id) => Right(Some(id))
          case None => cursor.get[Option[ExternalOrderLink]]("externalOrderKey") // COMPATIBLE with v2.7.1
      attachedState <- cursor.get[Option[AttachedState]]("attachedState")
      parent <- cursor.get[Option[OrderId]]("parent")
      mark <- cursor.get[Option[OrderMark]]("mark")
      isSuspended <- cursor.getOrElse[Boolean]("isSuspended")(false)
      isResumed <- cursor.getOrElse[Boolean]("isResumed")(false)
      deleteWhenTerminated <- cursor.getOrElse[Boolean]("deleteWhenTerminated")(false)
      forceJobAdmission <- cursor.getOrElse[Boolean]("forceJobAdmission")(false)
      innerBlock <- cursor.getOrElse[BranchPath]("innerBlock")(BranchPath.empty)
      stopPositions <- cursor.getOrElse[Set[PositionOrLabel]]("stopPositions")(Set.empty)
      stickySubagentId <- cursor.getOrElse[List[StickySubagent]]("stickySubagents")(Nil)
      historicOutcomes <- cursor.getOrElse[Vector[HistoricOutcome]]("historicOutcomes")(Vector.empty)
    yield
      Order(id, workflowPosition, state, arguments, planId, scheduledFor,
        externalOrder, historicOutcomes,
        attachedState, parent, mark,
        isSuspended = isSuspended,
        isResumed = isResumed,
        deleteWhenTerminated = deleteWhenTerminated,
        forceJobAdmission = forceJobAdmission,
        stickySubagentId,
        innerBlock, stopPositions)

  implicit val FreshOrReadyOrderJsonEncoder: Encoder.AsObject[Order[IsFreshOrReady]] =
    o => jsonEncoder.encodeObject(o)

  implicit val FreshOrReadyOrderJsonDecoder: Decoder[Order[IsFreshOrReady]] = cursor =>
    jsonDecoder(cursor) flatMap:
      o => o.ifState[IsFreshOrReady] match
        case None => Left(DecodingFailure(
          s"Order is not Fresh or Ready, but: ${o.state.getClass.simpleScalaName}", cursor.history))
        case Some(x) => Right(x)

  implicit val ProcessingOrderJsonEncoder: Encoder.AsObject[Order[Processing]] =
    o => jsonEncoder.encodeObject(o)

  implicit val ProcessingOrderJsonDecoder: Decoder[Order[Processing]] = c =>
    jsonDecoder(c).flatMap:
      o => o.ifState[Processing] match
        case None => Left(DecodingFailure(
          s"Order is not Fresh or Ready, but: ${o.state.getClass.simpleScalaName}",
          c.history))
        case Some(x) => Right(x)


  final case class ExternalOrderLink(
    orderWatchPath: OrderWatchPath,
    name: ExternalOrderName,
    vanished: Boolean = false):

    def externalOrderKey: ExternalOrderKey =
      ExternalOrderKey(orderWatchPath, name)

    override def toString =
      if vanished then
        s"ExternalOrderLink($externalOrderKey vanished)"
      else
        externalOrderKey.toString

  object ExternalOrderLink:
    def added(externalOrderKey: ExternalOrderKey): ExternalOrderLink =
      ExternalOrderLink(
        externalOrderKey.orderWatchPath,
        externalOrderKey.name)

    given Codec[ExternalOrderLink] = deriveConfiguredCodec


  final case class StickySubagent(
    agentPath: AgentPath,
    subagentBundleId: Option[SubagentBundleId],
    stuckSubagentId: Option[SubagentId] = None)
  object StickySubagent:
    given Codec[StickySubagent] = deriveRenamingCodec(Map(
      "subagentSelectionId" -> "subagentBundleId"))

  final case class InapplicableOrderEventProblem(event: OrderEvent, order: Order[State])
  extends Problem.Coded:
    def arguments: Map[String, String] = Map(
      "orderId" -> order.id.string,
      "event" -> event.toString,
      "workflowPosition" -> order.workflowPosition.toString,
      "state" -> order.state.getClass.simpleScalaName,
      "more" -> (order.markString.fold("")(o => s"$o, ") + order.attachedStateString))
