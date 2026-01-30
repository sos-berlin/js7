package js7.data.execution.workflow

import cats.instances.either.*
import cats.syntax.traverse.*
import js7.base.log.Logger
import js7.base.metering.CallMeter
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import js7.base.time.Timestamp
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.typeclasses.IsEmpty.syntax.*
import js7.data.Problems.{CancelStartedOrderProblem, GoOrderInapplicableProblem}
import js7.data.board.NoticeEvent.NoticeDeleted
import js7.data.command.{CancellationMode, SuspensionMode}
import js7.data.controller.ControllerState
import js7.data.event.EventColl.extensions.*
import js7.data.event.{Event, EventCalc, EventCalcCtx, EventColl, KeyedEvent}
import js7.data.execution.workflow.instructions.{FinishExecutor, InstructionExecutor}
import js7.data.order.Order.{Broken, Cancelled, Failed, FailedInFork, IsDelayingRetry, IsTerminated, ProcessingKilled, Stopped, StoppedWhileFresh}
import js7.data.order.OrderEvent.{OrderAwoke, OrderBroken, OrderCancellationMarked, OrderCancelled, OrderCaught, OrderCoreEvent, OrderDeleted, OrderDeletionMarked, OrderDetachable, OrderFailed, OrderFailedInFork, OrderGoMarked, OrderLocksReleased, OrderMoved, OrderNoticesConsumed, OrderOutcomeAdded, OrderPromptAnswered, OrderResumed, OrderResumptionMarked, OrderStateReset, OrderStickySubagentLeaved, OrderStopped, OrderSuspended, OrderSuspensionMarked}
import js7.data.order.{Order, OrderEvent, OrderId, OrderMark, OrderOutcome}
import js7.data.plan.PlanEvent.{PlanDeleted, PlanFinished}
import js7.data.plan.PlanFinishedEvent
import js7.data.problems.{CannotResumeOrderProblem, CannotSuspendOrderProblem, UnreachableOrderPositionProblem}
import js7.data.state.EngineEventColl.extensions.{order, orderAndWorkflow, useOrder, workflow}
import js7.data.state.EngineStateExtensions.{atController, orderToInstruction}
import js7.data.state.{EngineState, EngineState_}
import js7.data.workflow.instructions.{End, Finish, ForkInstruction, LockInstruction, Options, Retry, TryInstruction}
import js7.data.workflow.position.BranchPath.Segment
import js7.data.workflow.position.BranchPath.syntax.*
import js7.data.workflow.position.{BranchId, Position, TryBranchId}
import js7.data.workflow.{Workflow, WorkflowPathControlPath}
import org.jetbrains.annotations.TestOnly
import scala.annotation.tailrec

// Test is in js7-test subproject, because it uses AgentState

object OrderEventSource:
  private val logger = Logger[this.type]
  private val meterNextEvents = CallMeter("OrderEventSource.nextEvents")

  type ResultEvent = OrderCoreEvent | PlanFinishedEvent

  @TestOnly
  def nextEvents[S <: EngineState_[S]](orderId: OrderId, engineState: S, now: Timestamp)
  : Checked[Vector[KeyedEvent[OrderEvent | PlanFinished | NoticeDeleted | PlanDeleted]]] =
    nextEvents[S](orderId)
      .calculateEvents:
        EventColl[S, Event](engineState, now)
          .forwardAs[OrderEvent | PlanFinished | NoticeDeleted | PlanDeleted]
      .map(_.toVector)

  def nextEvents[S <: EngineState_[S]](orderId: OrderId)
  : EventCalc[S, OrderEvent | PlanFinished | NoticeDeleted | PlanDeleted] =
    EventCalc: coll =>
     meterNextEvents:
      coll.aggregate.idToOrder.get(orderId).fold(coll.nix): order =>
        if !coll.aggregate.weHave(order) then
          coll.nix
        else
          maybeOrderDeleted(order).widen.ifHasEventsAddToCollElse(coll):
            for
              coll <- coll:
                orderMarkEvent(order) // event if Order is Broken
              order <- coll.order(orderId)
              coll <-
                if order.isState[Order.Broken] then
                  coll.nix
                else if order.isSuspendedOrStopped then
                  coll.nix
                else if coll.aggregate.isOrderAtBreakpoint(order) then
                  coll:
                    coll.aggregate.atController(orderId):
                      OrderSuspended
                else if coll.aggregate.isWorkflowSuspended(order.workflowPath) then
                  coll.nix
                else if order.shouldFail then
                  coll:
                    fail[S](orderId, uncatchable = isUncatchable(order.lastOutcome))
                else
                  coll:
                    nextEvent2(order)
            yield coll.widen
          .recoverProblem: problem =>
            logger.debug(s"💥 $orderId execution failed: $problem")
            coll:
              failOrBreak(order, problem)

  private def nextEvent2[S <: EngineState_[S]](order: Order[Order.State])
  : EventCalc[S, OrderCoreEvent] =
    val orderId = order.id
    EventCalc: coll =>
      catchNonFatalFlatten:
        awokeEvent(order, coll.now).ifNonEmpty.map: events =>
          coll.addWithKey(orderId):
            events
        .getOrElse:
          joinedEvents(order).ifHasEventsAddToCollElse(coll):
            if coll.aggregate.isOrderAtStopPosition(order) then
              coll:
                FinishExecutor.toEventCalc(Finish(), orderId)
            else
              ifSkippedDueToWorkflowPathControlThenMove(coll, order).map: orderMoved =>
                coll:
                  orderId <-: orderMoved
              .getOrElse:
                if !coll.aggregate.isOrderExecutable(order) then // Since v2.8.3
                  coll.nix
                else
                  for
                    coll <- coll:
                      InstructionExecutor.toEventCalc(orderId)
                    coll <-
                      if coll.aggregate.idToOrder.contains(orderId) then
                        coll:
                          // Still required, because some other events than OrderMoved move too,
                          // without calling anticipateNextOrderMoved.
                          // May anticipateNextOrderMoved only, when last event is such an event.
                          anticipateNextOrderMoved(order.id)
                      else
                        coll.nix
                  yield coll

  private def failOrBreak[S <: EngineState_[S]](order: Order[Order.State], problem: Problem)
  : EventCalc[S, OrderCoreEvent] =
    val orderId = order.id
    EventCalc: coll =>
      if order.isFailable then
        // Before v2.5.6, due to JS-2087 uncatchable got lost in transfer back to Controller !!!
        coll:
          fail(orderId, Some(OrderOutcome.Disrupted(problem/*, uncatchable = true*/)))
        .recoverProblem: prblm =>
          logger.debug(s"💥 $orderId: $prblm")
          coll:
            (orderId <-: OrderOutcomeAdded(OrderOutcome.Disrupted(problem))) ::
              (orderId <-: OrderBroken()) ::
              order.isDetachable.thenList:
                orderId <-: OrderDetachable
      else
        coll(
          orderId <-: OrderOutcomeAdded(OrderOutcome.Disrupted(problem)),
          orderId <-: OrderBroken())

  final def fail[S <: EngineState_[S]](
    orderId: OrderId,
    outcome: Option[OrderOutcome.NotSucceeded] = None,
    uncatchable: Boolean = false)
  : EventCalc[S, OrderCoreEvent] =
    EventCalc: coll =>
      for
        coll <- coll:
          outcome.map(orderId <-: OrderOutcomeAdded(_))
        (order, workflow) <- coll.orderAndWorkflow(orderId)
        coll <-
          if isStopOnFailure(workflow, order.position) then
            coll:
              coll.aggregate.atController(order.id):
                OrderStopped
          else
            coll:
              failAndLeave(orderId, workflow, uncatchable)
      yield coll

  @tailrec
  private def isStopOnFailure(workflow: Workflow, position: Position): Boolean =
    workflow.instruction(position) match
      case Options(Some(stopOnFailure), _, _) => stopOnFailure
      case _ => position.parent match
        case Some(parent) => isStopOnFailure(workflow, parent)
        case None => false

  private def failAndLeave[S <: EngineState_[S]](
    orderId: OrderId, workflow: Workflow, uncatchable: Boolean)
  : EventCalc[S, OrderCoreEvent] =
    EventCalc: coll =>
      coll:
        leaveBlocksThen[S](orderId, workflow, catchable = !uncatchable):
          case (None | Some(BranchId.IsFailureBoundary(_)), failPosition) =>
            EventCalc: coll =>
              for
                order <- coll.order(orderId)
                coll <- coll:
                  orderId <-:
                    coll.aggregate.atController:
                      // Order is moved to Controller, which joins the orders anyway.
                      val joinIfFailed = order.parent
                        .flatMap: forkOrderId =>
                          coll.aggregate.orderToInstruction[ForkInstruction](forkOrderId).toOption
                        .fold(false):
                          _.joinIfFailed
                      if joinIfFailed then
                        OrderFailedInFork(failPosition)
                      else
                        OrderFailed(failPosition)
              yield coll

          case (Some(TryBranchId(_)), catchPos) =>
            EventCalc.pure:
              orderId <-: OrderCaught(catchPos)


  // Return OrderJoined or nothing
  private def joinedEvents[S <: EngineState_[S]](order: Order[Order.State])
  : EventCalc[S, OrderCoreEvent] =
    if order.parent.isDefined
      && order.isDetachedOrAttached
      && (order.isState[FailedInFork] || order.isState[Cancelled])
    then
      EventCalc: coll =>
        for
          forkPosition <- order.forkPosition
          fork <- coll.aggregate.instruction_[ForkInstruction](order.workflowId /: forkPosition)
          coll <- coll:
            InstructionExecutor.onReturnFromSubworkflow(fork, order)
        yield
          coll
    else
      EventCalc.empty

  private def awokeEvent(order: Order[Order.State], now: Timestamp): List[OrderAwoke | OrderMoved] =
    order.ifState[IsDelayingRetry].flatMap: order =>
      order.awokeEventsIfRipe(now) match
        case Left(problem) =>
          logger.error(s"awokeEvent: ${order.id}: $problem")
          None
        case Right(events) =>
          Some(events)
    .getOrElse:
      Nil

  def orderDeletionEvent[S <: EngineState_[S]](order: Order[Order.State])
  : EventCalc[S, OrderDeletionMarked | OrderDeleted | PlanFinishedEvent] =
    EventCalc: coll =>
      maybeOrderDeleted(order.copy(deleteWhenTerminated = true))
        .widen
        .ifHasEventsAddToCollElse(coll):
          coll:
            order.markDeletion.map: marked =>
              order.id <-: marked

  private def maybeOrderDeleted[S <: EngineState_[S]](order: Order[Order.State])
  : EventCalc[S, OrderDeleted | PlanFinished | NoticeDeleted | PlanDeleted] =
    order.maybeDeleted.fold(EventCalc.empty): orderDeleted =>
      EventCalc: coll =>
        coll.ifIs[ControllerState].map: coll =>
          // When KeyedEventChunk is implemented: return a transaction
          for
            coll <- coll:
              order.id <-: orderDeleted
            coll <- coll:
              coll.aggregate.maybePlanFinished(order.planId, coll.now)
          yield coll.widen
        .getOrElse:
          coll.nix

  private def orderMarkEvent[S <: EngineState_[S]](order: Order[Order.State])
  : EventCalc[S, OrderCoreEvent] =
    order.mark.fold(EventCalc.empty):
      case OrderMark.Cancelling(mode) =>
        tryCancel(order, mode)

      case OrderMark.Suspending(mode) =>
        trySuspendNow(order, mode).widen

      case OrderMark.Resuming(position, historyOperations, asSucceeded, restartKilledJob) =>
        tryResume(order, position, historyOperations, asSucceeded, restartKilledJob).widen

      case OrderMark.Go(_) =>
        // OrderMark.go is used only at the Controller to remember sending a MarkOrder command
        // to the Agent.
        // The Agent executes the MarkOrder command immediately
        EventCalc.empty

  def markOrder[S <: EngineState_[S]](orderId: OrderId, mark: OrderMark): EventCalc[S, OrderCoreEvent] =
    //assertThat(coll.aggregate.isAgent)
    mark match
      case OrderMark.Cancelling(mode) =>
        cancel(orderId, mode)

      case OrderMark.Suspending(mode) =>
        suspend(orderId, mode).widen

      case OrderMark.Resuming(position, historicOutcomes, asSucceeded, restartKilledJob) =>
        resume(orderId, position, historicOutcomes, asSucceeded, Some(restartKilledJob))
          .widen

      case OrderMark.Go(position /*dynamic*/) =>
        go(orderId, position)

  /** Returns `Right(Some(OrderCancelled | OrderCancellationMarked))` iff order is not already marked as cancelling. */
  def cancel[S <: EngineState_[S]](orderId: OrderId, mode: CancellationMode)
  : EventCalc[S, OrderCoreEvent] =
    EventCalc: coll =>
      catchNonFatalFlatten:
        coll.useOrder(orderId): order =>
          if mode == CancellationMode.FreshOnly && order.isStarted then
            // On Agent, the Order may already have been started without notice of the Controller
            Left(CancelStartedOrderProblem(orderId))
          else
            tryCancel(order, mode)
              .ifHasEventsAddToCollElse(coll):
                coll:
                  (!order.isState[IsTerminated] &&
                    !order.isState[ProcessingKilled] &&
                    !order.mark.contains(OrderMark.Cancelling(mode))
                  ).thenList:
                    order.id <-: OrderCancellationMarked(mode)

  private def tryCancel[S <: EngineState_[S]](order: Order[Order.State], mode: CancellationMode)
  : EventCalc[S, OrderCoreEvent] =
    EventCalc: coll =>
      if isOrderCancelable(coll.aggregate, order, mode) then
        if coll.aggregate.isAgent then
          coll:
            order.id <-: OrderDetachable
        else
          coll.workflow(order.workflowId).flatMap: workflow =>
            coll:
              leaveBlocks(order.id, workflow, OrderCancelled :: Nil)
      else
        coll.nix

  private def isOrderCancelable(engineState: EngineState, order: Order[Order.State], mode: CancellationMode): Boolean =
    engineState.weHave(order) &&
      order.isCancelable(mode) &&
      // If End instruction (last workflow instruction) is reached unsuspended,
      // the order is finished normally
      (!order.isStarted
        || !engineState.instructionIs[End](order.workflowPosition)
        || engineState.isSuspendedOrStopped(order)
        || order.isState[Broken]
        || order.isState[Failed]) // Since v2.8.3

  /** Returns a `Right(OrderSuspended | OrderSuspensionMarked)` iff order is not already marked as suspending. */
  def suspend[S <: EngineState_[S]](orderId: OrderId, mode: SuspensionMode)
  : EventCalc[S, OrderStateReset | OrderSuspended | OrderSuspensionMarked | OrderDetachable] =
    EventCalc: coll =>
      catchNonFatalFlatten:
        coll.useOrder(orderId): order =>
          if !order.isSuspendible then
            Left(CannotSuspendOrderProblem)
          else if order.isSuspended && !order.isResuming then
            coll.nix
          else
            trySuspendNow(order, mode).widen
              .ifHasEventsAddToCollElse(coll):
                if order.mark.contains(OrderMark.Suspending(mode)) then
                  // Already marked, duplicate suspend
                  coll.nix
                else
                  coll:
                    if order.isDetachable then
                      // TODO Does this return duplicate OrderDetachable ?
                      // For example, Order.DelayedAfterError (retry)
                      coll.aggregate.atController(orderId):
                        OrderSuspensionMarked(mode)
                    else
                      order.id <-: OrderSuspensionMarked(mode)

  private def trySuspendNow[S <: EngineState_[S]](order: Order[Order.State], mode: SuspensionMode)
  : EventCalc[S, OrderStateReset | OrderSuspended | OrderDetachable] =
    EventCalc: coll =>
      coll:
        val maybeReset = if mode.resetState then order.resetState else Nil
        if coll.aggregate.weHave(order)
          && order.isDetachedOrDetachable
          && order.applyEvents(maybeReset).exists(_.isSuspendibleNow)
        then
          coll.aggregate.atController(order.id):
            maybeReset ::: OrderSuspended :: Nil
              : List[OrderStateReset | OrderSuspended]
        else
          Nil

  def go[S <: EngineState_[S]](orderId: OrderId, position: Position): EventCalc[S, OrderCoreEvent] =
    EventCalc: coll =>
      coll.order(orderId).flatMap: order =>
        if !order.isGoCommandable(position) then
          Left(GoOrderInapplicableProblem(order.id))
        else if coll.aggregate.weHave(order) then
          coll.addWithKey(order.id):
            order.go
        else if order.isAttached then
          // Emit OrderGoMarked event even if already marked. The user wishes so.
          // In case the last OrderMark.Go was futile, the repeated OrderGoMarked event induces a
          // new AgentCommand.MarkOrder which may be effective this time.
          coll:
            order.id <-: OrderGoMarked(position)
        else
          Left(GoOrderInapplicableProblem(order.id)) // Just in case

  /** Returns a `Right(Some(OrderResumed | OrderResumptionMarked))`
   * iff order is not already marked as resuming. */
  def resume[S <: EngineState_[S]](
    orderId: OrderId,
    position: Option[Position],
    historyOperations: Seq[OrderResumed.HistoryOperation],
    asSucceeded: Boolean,
    restartKilledJob: Option[Boolean])
  : EventCalc[S, OrderResumed | OrderResumptionMarked] =
    EventCalc: coll =>
      catchNonFatalFlatten:
        for
          (order, workflow) <- coll.orderAndWorkflow(orderId)
          _ <- position.fold(Checked.unit): position =>
            workflow.isMoveable(order.position, position) !! UnreachableOrderPositionProblem
          _ <- historyOperations.flatMap(_.positions).traverse: pos =>
            workflow.checkPosition(pos).rightAs(pos)
          coll <-
            order.mark match
              case Some(_: OrderMark.Cancelling) =>
                Left(CannotResumeOrderProblem)

              case Some(OrderMark.Resuming(`position`, `historyOperations`, asSucceeded, restartKilledJob)) =>
                coll:
                  tryResume(order, position, historyOperations, asSucceeded, restartKilledJob)

              case Some(OrderMark.Resuming(_, _, _, _)) =>
                Left(CannotResumeOrderProblem)

              case Some(OrderMark.Go(_)) =>
                Left(CannotResumeOrderProblem)

              case Some(OrderMark.Suspending(_))
                if position.isDefined || historyOperations.nonEmpty =>
                Left(CannotResumeOrderProblem)

              case None | Some(OrderMark.Suspending(_)) =>
                val okay = order.isSuspended ||
                  order.mark.exists(_.isInstanceOf[OrderMark.Suspending]) ||
                  order.isDetached && (
                    order.isState[Failed] ||
                      order.isState[Stopped] ||
                      order.isState[StoppedWhileFresh])
                if !okay then
                  Left(CannotResumeOrderProblem)
                else
                  val restart = restartKilledJob.getOrElse:
                    order.lastOutcome.isInstanceOf[OrderOutcome.Killed]
                      && coll.aggregate.workflowJob(order.workflowPosition).exists(_.isRestartable)
                  tryResume(order, position, historyOperations, asSucceeded, restart)
                    .widen.ifHasEventsAddToCollElse(coll):
                      coll:
                        orderId <-:
                          OrderResumptionMarked(position, historyOperations, asSucceeded, restart)
        yield coll

  private def tryResume[S <: EngineState_[S]](
    order: Order[Order.State],
    position: Option[Position],
    historyOperations: Seq[OrderResumed.HistoryOperation],
    asSucceeded: Boolean,
    restartKilledJob: Boolean)
  : EventCalc[S, OrderResumed] =
    EventCalc: coll =>
      coll:
        coll.aggregate.weHave(order) && order.isResumableNow thenSome:
          order.id <-: OrderResumed(position, historyOperations, asSucceeded, restartKilledJob)

  def answerPrompt[S <: EngineState_[S]](orderId: OrderId): EventCalc[S, OrderCoreEvent] =
    EventCalc: coll =>
      catchNonFatalFlatten:
        for
          order <- coll.order(orderId)
          _ <- order.checkedState[Order.Prompting]
          coll <- coll:
            orderId <-: OrderPromptAnswered()
          order <- coll.order(orderId)
          coll <- coll:
            moveOrderToNextInstruction(order, forCommand = true)
        yield coll

  def moveOrderToNextInstruction[S <: EngineState_[S]](
    order: Order[Order.State],
    reason: Option[OrderMoved.Reason] = None,
    forCommand: Boolean = false)
  : EventCalc[S, OrderMoved] =
    moveOrder(order.id, to = order.position.increment, reason, forCommand = forCommand)

  def moveOrderDownToBranch[S <: EngineState_[S]](
    order: Order[Order.State],
    branchId: BranchId,
    reason: Option[OrderMoved.Reason] = None)
  : EventCalc[S, OrderMoved] =
    moveOrder(order.id, to = order.position / branchId % 0, reason)

  def moveOrder[S <: EngineState_[S]](
    orderId: OrderId,
    to: Position,
    reason: Option[OrderMoved.Reason] = None,
    forCommand: Boolean = false)
  : EventCalc[S, OrderMoved] =
    val event = OrderMoved(to, reason)
    if forCommand then
      // A command must not execute the following, possibly failing if-instructions.
      // therefore, we emit only a single OrderMoved event.
      EventCalc.pure:
        orderId <-: event
    else
      anticipateNextOrderMoved(orderId, Some(event))

  private[workflow] def anticipateNextOrderMoved[S <: EngineState_[S]](
    orderId: OrderId,
    firstOrderMoved: Option[OrderMoved] = None)
  : EventCalc[S, OrderMoved] =
    EventCalc: coll =>

      //@tailrec ???
      def loop(order: Order[Order.State], visited: Vector[OrderMoved], coll: EventColl[S, OrderMoved])
      : Checked[Vector[OrderMoved]] =
        nextMove(coll, order) match
          case Left(problem) => Left(problem)
          case Right(None) => Right(visited)

          case Right(Some(orderMoved)) =>
            if visited.exists(_.to == orderMoved.to) then
              Left(Problem(s"$orderId is in a workflow loop: " +
                visited.reverse
                  .map(moved => moved.toString + " " +
                    coll.workflow(order.workflowId)
                      .flatMap(_.labeledInstruction(moved.to))
                      .fold(_.toString, _.toString)
                      .truncateWithEllipsis(50))
                  .mkString(" --> ")))
            else
              for
                coll <- coll:
                  orderId <-: orderMoved
                order <- coll.order(orderId)
                events <- loop(
                  order,
                  if orderMoved.reason.isEmpty && visited.lastOption.exists(_.reason.isEmpty) then
                    visited.updated(visited.length - 1, orderMoved)
                  else
                    visited :+ orderMoved,
                  coll)
              yield
                events

      firstOrderMoved match
        // TODO Simplify this code
        case None =>
          coll.order(orderId).flatMap: order =>
            coll.addWithKey(orderId):
              loop(order, Vector.empty, coll.forwardAs)
        case Some(orderMoved) =>
          coll:
            orderId <-: orderMoved
          .flatMap: coll2 =>
            coll2.useOrder(orderId): order =>
              coll.addWithKey(orderId):
                loop(order, Vector(orderMoved), coll2.forwardAs)

  private def nextMove[S <: EngineState_[S]](
    coll: EventColl[S, ? <: OrderCoreEvent],
    order: Order[Order.State])
  : Checked[Option[OrderMoved]] =
    for
      workflow <- coll.workflow(order.workflowId)
      maybeMoved <-
        if coll.aggregate.isOrderAtBreakpoint(order) then
          Right(None)
        else if workflow.isOrderAtStopPosition(order) then
          Right(None)
        else
          ifSkippedDueToWorkflowPathControlThenMove(coll, order) match
            case Some(orderMoved) =>
              Right(Some(orderMoved))
            case None =>
              InstructionExecutor.nextMove(order.id, coll.aggregate)
    yield
      maybeMoved

  private def ifSkippedDueToWorkflowPathControlThenMove[S <: EngineState_[S]](
    coll: EventColl[S, ? <: OrderCoreEvent],
    order: Order[Order.State])
  : Option[OrderMoved] =
    (order.isSkippable(coll.now) && isSkippedDueToWorkflowPathControl(coll.aggregate, order)) ?
      OrderMoved(order.position.increment, Some(OrderMoved.SkippedDueToWorkflowPathControl))

  private def isSkippedDueToWorkflowPathControl(engineState: EngineState, order: Order[Order.State])
  : Boolean =
    !order.isState[Order.BetweenCycles] &&
      engineState.pathToWorkflowPathControl.get(WorkflowPathControlPath(order.workflowPath))
        .exists: control =>
          engineState.workflowPositionToLabel(order.workflowPosition)
            .toOption.flatten
            .exists(control.skip.contains)

  def leaveBlocks[S <: EngineState_[S]](
    orderId: OrderId,
    workflow: Workflow,
    events: List[OrderCoreEvent])
  : EventCalc[S, OrderCoreEvent] =
    leaveBlocks(orderId, workflow,
      _ => EventCalc.pure(events.map(orderId <-: _)),
      until = _ => false)

  def leaveBlocks[S <: EngineState_[S]](
    orderId: OrderId,
    workflow: Workflow,
    events: Option[BranchId] => EventCalc[S, OrderCoreEvent],
    until: BranchId => Boolean)
  : EventCalc[S, OrderCoreEvent] =
    leaveBlocksThen(orderId, workflow, catchable = false, until = until): (maybeBranchId, _) =>
      events(maybeBranchId)

  private def leaveBlocksThen[S <: EngineState_[S]](
    orderId: OrderId,
    workflow: Workflow,
    catchable: Boolean,
    until: BranchId => Boolean = _ => false)
    (toEvent: PartialFunction[(Option[BranchId], Position), EventCalc[S, OrderCoreEvent]])
  : EventCalc[S, OrderCoreEvent] =
    EventCalc: coll =>
      for
        order <- coll.order(orderId)
        coll <- coll.addWithKey(orderId):
          order.resetState
        coll <- coll:
          leaveBlocksThen2(orderId, workflow, catchable, until)(toEvent)
      yield coll

  private def leaveBlocksThen2[S <: EngineState_[S]](
    orderId: OrderId,
    workflow: Workflow,
    catchable: Boolean,
    until: BranchId => Boolean)
    (toEvent: PartialFunction[(Option[BranchId], Position), EventCalc[S, OrderCoreEvent]])
  : EventCalc[S, OrderCoreEvent] =
    def callToEvent(branchId: Option[BranchId], pos: Position) =
      toEvent.lift((branchId, pos))
        .getOrElse:
          EventCalc.problem:
            Problem(s"Unexpected BranchId '$branchId' while leaving instruction blocks")

    def loop(reverseBranchPath: List[Segment], failPosition: Position)
    : EventCalc[S, OrderCoreEvent] =
      EventCalc: coll =>
        coll.useOrder(orderId): order =>
          if reverseBranchPath == order.innerBlock.reverse then
            coll(callToEvent(None, failPosition))
          else
            reverseBranchPath match
              case Nil =>
                coll(callToEvent(None, failPosition))

              case Segment(nr, branchId) :: _ if until(branchId) =>
                for
                  coll <- coll(orderId <-: OrderMoved(reverseBranchPath.reverse % nr.increment))
                  coll <- coll(callToEvent(Some(branchId), failPosition))
                yield coll

              case Segment(_, branchId @ BranchId.IsFailureBoundary(_)) :: _ =>
                coll(callToEvent(Some(branchId), failPosition))

              case Segment(nr, BranchId.Lock) :: prefix =>
                if order.isAttached then
                  coll:
                    orderId <-: OrderDetachable
                else
                  val pos = prefix.reverse % nr
                  for
                    lock <- workflow.instruction_[LockInstruction](pos)
                    coll <- coll(orderId <-: OrderLocksReleased(lock.lockPaths))
                    coll <- coll(loop(prefix, pos))
                  yield
                    coll

              case Segment(nr, BranchId.ConsumeNotices) :: prefix =>
                if order.isAttached then
                  coll(orderId <-: OrderDetachable)
                else
                  for
                    coll <- coll(orderId <-: OrderNoticesConsumed(failed = true))
                    coll <- coll(loop(prefix, prefix.reverse % nr))
                  yield coll

              case Segment(nr, BranchId.StickySubagent) :: prefix =>
                for
                  coll <- coll(orderId <-: OrderStickySubagentLeaved)
                  coll <- coll(loop(prefix, prefix.reverse % nr))
                yield coll

              case Segment(nr, branchId @ TryBranchId(retry)) :: prefix if catchable =>
                val catchPos = prefix.reverse % nr / BranchId.catch_(retry) % 0
                if isMaxRetriesReached(workflow, catchPos) then
                  coll(loop(prefix, failPosition))
                else
                  coll(callToEvent(Some(branchId), catchPos))

              case Segment(_, _) :: prefix =>
                coll(loop(prefix, failPosition))
    end loop

    EventCalc: coll =>
      coll.useOrder(orderId): order =>
        coll:
          loop(order.position.branchPath.reverse, order.position)
  end leaveBlocksThen2


  // Special handling for try with maxRetries and catch block with retry instruction only:
  // try (maxRetries=n) ... catch retry
  // In this case, OrderFailed event must have original failure's position, not failed retry's position.
  private def isMaxRetriesReached(workflow: Workflow, firstCatchPos: Position): Boolean =
    val catchStartsWithRetry = workflow.instruction(firstCatchPos).withoutSourcePos == Retry()
    catchStartsWithRetry &&
      firstCatchPos.parent.forall: parentPos =>
        workflow.instruction(parentPos) match // Parent must be a TryInstruction
          case t: TryInstruction => t.maxTries.exists(firstCatchPos.tryCount >= _)

  /** Used in combination with `isFailed` to handle failed Orders transferred back to Controller. */
  private def isUncatchable(outcome: OrderOutcome): Boolean =
    outcome match
      case o: OrderOutcome.NotSucceeded => o.uncatchable
      case _ => false
