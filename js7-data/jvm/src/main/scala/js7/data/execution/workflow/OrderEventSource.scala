package js7.data.execution.workflow

import cats.instances.either.*
import cats.instances.list.*
import cats.syntax.flatMap.*
import cats.syntax.traverse.*
import js7.base.log.Logger
import js7.base.problem.Checked.catchNonFatalFlatten
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Assertions.assertThat
import js7.base.utils.ScalaUtils.checkedCast
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.typeclasses.IsEmpty.syntax.*
import js7.data.Problems.{CancelStartedOrderProblem, GoOrderInapplicableProblem}
import js7.data.agent.AgentPath
import js7.data.board.NoticeEvent.NoticeDeleted
import js7.data.command.{CancellationMode, SuspensionMode}
import js7.data.controller.ControllerState
import js7.data.event.{<-:, KeyedEvent}
import js7.data.execution.workflow.OrderEventSource.*
import js7.data.execution.workflow.instructions.InstructionExecutorService
import js7.data.order.Order.{Broken, Cancelled, Failed, FailedInFork, IsDelayingRetry, IsTerminated, ProcessingKilled, Stopped, StoppedWhileFresh}
import js7.data.order.OrderEvent.*
import js7.data.order.{Order, OrderId, OrderMark, OrderOutcome}
import js7.data.problems.{CannotResumeOrderProblem, CannotSuspendOrderProblem, UnreachableOrderPositionProblem}
import js7.data.state.StateView
import js7.data.state.StateViewForEvents.atController
import js7.data.workflow.instructions.{End, Finish, ForkInstruction, Gap, LockInstruction, Options, Retry, TryInstruction}
import js7.data.workflow.position.BranchPath.Segment
import js7.data.workflow.position.BranchPath.syntax.*
import js7.data.workflow.position.{BranchId, Position, TryBranchId, WorkflowPosition}
import js7.data.workflow.{Instruction, Workflow, WorkflowPathControlPath}
import scala.annotation.tailrec
import scala.collection.View
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
final class OrderEventSource(state: StateView/*idToOrder must be a Map!!!*/)
  (using executorService: InstructionExecutorService):

  import executorService.clock
  import state.{idToWorkflow, isAgent, weHave}

  // TODO Updates to StateView should be solved immutably. Refactor OrderEventSource?
  private var idToOrder = state.idToOrder

  def nextEvents(orderId: OrderId): Seq[KeyedEvent[OrderActorEvent | NoticeDeleted]] =
    val order = idToOrder(orderId)
    if !weHave(order) then
      Nil
    else
      tryDelete(order)
        .ifEmpty:
          orderMarkKeyedEvent(order)
        .ifEmpty:
          if order.isState[Order.Broken] then
            Nil // Avoid issuing a second OrderBroken (would be a loop)
          else if order.isSuspendedOrStopped then
            Nil
          else if state.isOrderAtBreakpoint(order) then
            atController(OrderSuspended :: Nil)
              .map(order.id <-: _)
          else if state.isWorkflowSuspended(order.workflowPath) then
            Nil
          else
            checkedNextEvents(order) match
              case Left(problem) => invalidToEvent(order, problem)
              case Right(keyedEvents) => keyedEvents

  private def checkedNextEvents(order: Order[Order.State])
  : Checked[Seq[KeyedEvent[OrderActorEvent]]] =
    if order.shouldFail then
      fail(order, uncatchable = isUncatchable(order.lastOutcome))
        .map(_.map(order.id <-: _))
    else
      catchNonFatalFlatten:
        awokeEvent(order).ifNonEmpty.map: events =>
          Right(events.map(order.id <-: _))
        .getOrElse:
          joinedEvents(order).flatMap: events =>
            if events.nonEmpty then
              Right(events)
            else if state.isOrderAtStopPosition(order) then
              executorService.finishExecutor.toEvents(Finish(), order, state)
            else
              ifSkippedDueToWorkflowPathControlThenMove(order) match
                case Some(orderMoved) =>
                  Right((order.id <-: orderMoved) :: Nil)
                case None =>
                  executorService.toEvents(instruction(order.workflowPosition), order, state)
                    // Multiple returned events are expected to be independent
                    // and are applied to the same order !!!
                    .flatMap: events =>
                      val (first, maybeLast) = events.splitAt(events.length - 1)
                      updateIdToOrder(first) >>
                        maybeLast
                          .flatTraverse:
                            case orderId <-: (moved: OrderMoved) =>
                              applyMoveInstructions(idToOrder(orderId), moved)
                                .map(_.map(orderId <-: _))

                            case orderId <-: OrderFailedIntermediate_(outcome) =>
                              // OrderFailedIntermediate_ is used internally only
                              fail(idToOrder(orderId), outcome)
                                .map(_.map(orderId <-: _))

                            case o => Right(o :: Nil)
                          .flatTap(updateIdToOrder)
                          .map(first ::: _)
        .flatMap(checkEvents)

  private def updateIdToOrder(keyedEvents: IterableOnce[KeyedEvent[OrderActorEvent]])
  : Checked[Unit] =
    val it = keyedEvents.iterator
    if !it.hasNext then
      Checked.unit
    else
      checkedCast[Map[OrderId, Order[Order.State]]](idToOrder)
        .flatMap: idToOrder_ =>
          var iToO = idToOrder_
          var problem: Problem | Null = null
          while it.hasNext && problem == null do
            val KeyedEvent(orderId, event) = it.next()
            iToO.checked(orderId).flatMap(_.applyEvent(event)) match
              case Left(prblm) => problem = prblm
              case Right(order) => iToO = iToO.updated(order.id, order)
          this.idToOrder = iToO
          problem.toChecked

  private def checkEvents(keyedEvents: Seq[KeyedEvent[OrderActorEvent]]) =
    var id2o = Map.empty[OrderId, Checked[Order[Order.State]]]
    var problem: Option[Problem] = None
    for KeyedEvent(orderId, event) <- keyedEvents if problem.isEmpty do
      id2o.getOrElse(orderId, state.idToOrder.checked(orderId))
        .flatMap(_.applyEvent(event)) match
          case Left(prblm) => problem = Some(prblm)
          case Right(order) => id2o += orderId -> Right(order)
    problem.toLeft(keyedEvents)

  private def invalidToEvent(order: Order[Order.State], problem: Problem)
  : Seq[KeyedEvent[OrderActorEvent]] =
    val events =
      if order.isFailable then
        // Before v2.5.6, due to JS-2087 uncatchable got lost in transfer back to Controller !!!
        fail(order, Some(OrderOutcome.Disrupted(problem/*, uncatchable = true*/))) match
          case Left(prblm) =>
            logger.debug(s"WARN ${order.id}: $prblm")
            OrderOutcomeAdded(OrderOutcome.Disrupted(problem)) ::
              OrderBroken() ::
              order.isDetachable.thenList:
                OrderDetachable
          case Right(events) => events
      else
        OrderOutcomeAdded(OrderOutcome.Disrupted(problem)) :: OrderBroken() :: Nil
    events.map(order.id <-: _)

  private[data] def fail(
    order: Order[Order.State],
    outcome: Option[OrderOutcome.NotSucceeded] = None,
    uncatchable: Boolean = false)
  : Checked[List[OrderActorEvent]] =
    for
      workflow <- idToWorkflow.checked(order.workflowId)
      events <- fail(workflow, order, outcome,
        uncatchable = uncatchable || outcome.exists(isUncatchable))
    yield
      events

  private[workflow] def fail(
    workflow: Workflow,
    order: Order[Order.State],
    outcome: Option[OrderOutcome.NotSucceeded],
    uncatchable: Boolean)
  : Checked[List[OrderActorEvent]] =
    val outcomeAdded = outcome.map(OrderOutcomeAdded(_)).toList
    if isStopOnFailure(workflow, order.position) then
      Right(outcomeAdded ::: atController(OrderStopped :: Nil))
    else
      for events <- failAndLeave(workflow, order, uncatchable) yield
        outcomeAdded ++: events

  @tailrec
  private def isStopOnFailure(workflow: Workflow, position: Position): Boolean =
    workflow.instruction(position) match
      case Options(Some(stopOnFailure), _, _) => stopOnFailure
      case _ => position.parent match
        case Some(parent) => isStopOnFailure(workflow, parent)
        case None => false

  private def failAndLeave(
    workflow: Workflow,
    order: Order[Order.State],
    uncatchable: Boolean)
  : Checked[List[OrderActorEvent]] =
    leaveBlocksThen(workflow, order, catchable = !uncatchable):
      case (None | Some(BranchId.IsFailureBoundary(_)), failPosition) =>
        atController:
          // TODO Transfer parent order to Agent to access joinIfFailed there !
          // For now, order will be moved to Controller, which joins the orders anyway.
          val joinIfFailed = order.parent
            .flatMap(forkOrderId => instruction_[ForkInstruction](forkOrderId).toOption)
            .fold(false)(_.joinIfFailed)
          if joinIfFailed then
            OrderFailedInFork(failPosition) :: Nil
          else
            OrderFailed(failPosition) :: Nil

      case (Some(TryBranchId(_)), catchPos) =>
        OrderCaught(catchPos) :: Nil

  // Return Nil or List(OrderJoined)
  private def joinedEvents(order: Order[Order.State]): Checked[List[KeyedEvent[OrderActorEvent]]] =
    if order.parent.isDefined
      && order.isDetachedOrAttached
      && (order.isState[FailedInFork] || order.isState[Cancelled])
    then
      for
        forkPosition <- order.forkPosition
        fork <- state.instruction_[ForkInstruction](order.workflowId /: forkPosition)
        events <- executorService.onReturnFromSubworkflow(fork, order, state)
      yield events
    else
      Right(Nil)

  private def awokeEvent(order: Order[Order.State]): List[OrderAwoke | OrderMoved] =
    order.ifState[IsDelayingRetry].flatMap: order =>
      order.awokeEventsIfRipe(clock.now()) match
        case Left(problem) =>
          logger.error(s"awokeEvent: ${order.id}: $problem")
          None
        case Right(events) =>
          Some(events)
    .getOrElse:
      Nil

  def orderDeletedEvent(order: Order[Order.State])
  : Checked[Seq[KeyedEvent[OrderDeletionMarked | OrderDeleted | NoticeDeleted]]] =
    val events = tryDelete(order.copy(deleteWhenTerminated = true))
    if events.nonEmpty then
      Right(events)
    else
      order.markDeletion.map: event =>
        (order.id <-: event) :: Nil

  private def tryDelete(order: Order[Order.State]): Vector[KeyedEvent[OrderDeleted | NoticeDeleted]] =
    state match
      case controllerState: ControllerState =>
        order.tryDelete.fold(Vector.empty): orderDeleted =>
          controllerState.applyKeyedEvent(order.id <-: orderDeleted) match
            case Left(problem) => logger.error(s"tryDelete: ${order.id}: $problem")
              Vector.empty
            case Right(controllerState) =>
              (controllerState.deleteNoticesOfDeadPlan(order.planId) :+ (order.id <-: orderDeleted))
                .toVector
      case _ => Vector.empty

  private def orderMarkKeyedEvent(order: Order[Order.State]): List[KeyedEvent[OrderActorEvent]] =
    orderMarkEvent(order)
      .map(order.id <-: _)

  private def orderMarkEvent(order: Order[Order.State]): List[OrderActorEvent] =
    order.mark.toList.flatMap:
      case OrderMark.Cancelling(mode) =>
        tryCancel(order, mode)

      case OrderMark.Suspending(mode) =>
        trySuspendNow(order, mode)

      case OrderMark.Resuming(position, historyOperations, asSucceeded) =>
        tryResume(order, position, historyOperations, asSucceeded)
          .toList

      case OrderMark.Go(_) =>
        // OrderMark.go is used only at the Controller to remember sending a MarkOrder command
        // to the Agent.
        // The Agent executes the MarkOrder command immediately
        Nil

  def markOrder(orderId: OrderId, mark: OrderMark): Checked[List[OrderActorEvent]] =
    catchNonFatalFlatten:
      assertThat(isAgent)
      mark match
        case OrderMark.Cancelling(mode) =>
          cancel(orderId, mode)

        case OrderMark.Suspending(mode) =>
          suspend(orderId, mode)

        case OrderMark.Resuming(position, historicOutcomes, asSucceeded) =>
          resume(orderId, position, historicOutcomes, asSucceeded)

        case OrderMark.Go(position/*dynamic*/) =>
          go(orderId, position)

  /** Returns `Right(Some(OrderCancelled | OrderCancellationMarked))` iff order is not already marked as cancelling. */
  def cancel(orderId: OrderId, mode: CancellationMode): Checked[List[OrderActorEvent]] =
    catchNonFatalFlatten:
      withOrder(orderId): order =>
        if mode == CancellationMode.FreshOnly && order.isStarted then
          // On Agent, the Order may already have been started without notice of the Controller
          Left(CancelStartedOrderProblem(orderId))
        else
          Right:
            tryCancel(order, mode)
              .ifNonEmpty.getOrElse:
                ( !order.isState[IsTerminated] &&
                  !order.isState[ProcessingKilled] &&
                  !order.mark.contains(OrderMark.Cancelling(mode))
                ).thenList:
                  OrderCancellationMarked(mode)

  private def tryCancel(order: Order[Order.State], mode: CancellationMode)
  : List[OrderActorEvent] =
    if isOrderCancelable(order, mode) then
      atController:
        val workflow = state.idToWorkflow(order.workflowId)
        leaveBlocks(workflow, order, OrderCancelled :: Nil)
          .orThrow
    else
      Nil

  private def isOrderCancelable(order: Order[Order.State], mode: CancellationMode): Boolean =
    weHave(order) &&
      order.isCancelable(mode) &&
      // If workflow End is reached unsuspended, the order is finished normally
      // TODO Correct? Or should we check only the end of the main/forked workflow?
      (!instruction(order.workflowPosition).isInstanceOf[End]
        || state.isSuspendedOrStopped(order)
        || order.isState[Broken])

  /** Returns a `Right(OrderSuspended | OrderSuspensionMarked)` iff order is not already marked as suspending. */
  def suspend(orderId: OrderId, mode: SuspensionMode)
  : Checked[List[OrderSuspended | OrderStateReset| OrderSuspensionMarked | OrderDetachable]] =
    catchNonFatalFlatten:
      withOrder(orderId): order =>
        if !order.isSuspendible then
          Left(CannotSuspendOrderProblem)
        else
          Right:
            if order.isSuspended && !order.isResuming then
              Nil
            else
              trySuspendNow(order, mode)
                .ifEmpty:
                  if order.mark.contains(OrderMark.Suspending(mode)) then
                    // Already marked, duplicate suspend
                    Nil
                  else
                    val marked = OrderSuspensionMarked(mode) :: Nil
                    if order.isDetachable then
                      // For example, Order.DelayedAfterError (retry)
                      atController(marked)
                    else
                      marked

  private def trySuspendNow(order: Order[Order.State], mode: SuspensionMode)
  : List[OrderSuspended | OrderStateReset | OrderDetachable] =
    val maybeReset = if mode.resetState then order.resetState else Nil
    if weHave(order)
      && order.isDetachedOrDetachable
      && order.applyEvents(maybeReset).exists(_.isSuspendibleNow)
    then
      atController[OrderSuspended | OrderStateReset | OrderDetachable]:
        maybeReset ::: OrderSuspended :: Nil
    else
      Nil

  def go(orderId: OrderId, position: Position): Checked[List[OrderActorEvent]] =
    idToOrder.checked(orderId).flatMap: order =>
      if !order.isGoCommandable(position) then
        Left(GoOrderInapplicableProblem(order.id))
      else if weHave(order) then
        order.go
      else if order.isAttached then
        // Emit OrderGoMarked event even if already marked. The user wishes so.
        // In case the last OrderMark.Go was futile, the repeated OrderGoMarked event induces a
        // new AgentCommand.MarkOrder which may be effective this time.
        Right:
          OrderGoMarked(position) :: Nil
      else
        Left(GoOrderInapplicableProblem(order.id)) // Just in case

  /** Returns a `Right(Some(OrderResumed | OrderResumptionMarked))`
   * iff order is not already marked as resuming. */
  def resume(
    orderId: OrderId,
    position: Option[Position],
    historyOperations: Seq[OrderResumed.HistoryOperation],
    asSucceeded: Boolean)
  : Checked[List[OrderResumed | OrderResumptionMarked]] =
    catchNonFatalFlatten:
      withOrder(orderId): order =>
        lazy val checkedWorkflow = idToWorkflow.checked(order.workflowId)

        val checkPosition = position.fold(Checked.unit): position =>
          checkedWorkflow.flatMap(workflow =>
            workflow.isMoveable(order.position, position) !! UnreachableOrderPositionProblem)

        val checkHistoricPositions =
          checkedWorkflow.flatMap(workflow =>
            historyOperations
              .flatMap(_.positions)
              .traverse(pos => workflow.checkPosition(pos).rightAs(pos)))

        checkPosition
          .flatMap(_ => checkHistoricPositions)
          .flatMap: _ =>
            order.mark match
              case Some(_: OrderMark.Cancelling) =>
                Left(CannotResumeOrderProblem)

              case Some(OrderMark.Resuming(`position`, `historyOperations`, asSucceeded)) =>
                Right(order.isDetached.thenList:
                  // should already have happened
                  OrderResumed(
                    position,
                    historyOperations,
                    asSucceeded))

              case Some(OrderMark.Resuming(_, _, _)) =>
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
                  Right:
                    tryResume(order, position, historyOperations, asSucceeded)
                      .getOrElse:
                        OrderResumptionMarked(position, historyOperations, asSucceeded)
                      :: Nil

  /** Retrieve the Order, check if calculated event is applicable. */
  private def withOrder[E <: OrderCoreEvent](orderId: OrderId)
    (body: Order[Order.State] => Checked[List[E]])
  : Checked[List[E]] =
    idToOrder.checked(orderId).flatMap: order =>
      body(order).flatMap: events =>
        order.applyEvents(events)
          .rightAs(events)

  private def tryResume(
    order: Order[Order.State],
    position: Option[Position],
    historyOperations: Seq[OrderResumed.HistoryOperation],
    asSucceeded: Boolean)
  : Option[OrderResumed] =
    (weHave(order) && order.isResumableNow) ?
      OrderResumed(position, historyOperations, asSucceeded)

  def answerPrompt(orderId: OrderId): Checked[Seq[KeyedEvent[OrderCoreEvent]]] =
    catchNonFatalFlatten:
      for
        order <- idToOrder.checked(orderId)
        _ <- order.checkedState[Order.Prompting]
      yield
        Seq(
          orderId <-: OrderPromptAnswered(),
          orderId <-: OrderMoved(order.position.increment))

  def nextAgent(order: Order[Order.State]): Checked[Option[AgentPath]] =
    catchNonFatalFlatten:
      for moves <- applyMoveInstructions(order) yield
        for
          workflow <- idToWorkflow.get(order.workflowId)
          agentPath <- workflow.agentPath(moves.lastOption.fold(order.position)(_.to))
        yield agentPath

  private def applyMoveInstructions(order: Order[Order.State], orderMoved: OrderMoved)
  : Checked[List[OrderMoved]] =
    applyMoveInstructions(order, Some(orderMoved))
      .map(_.toList)

  private[workflow] def applyMoveInstructions(order: Order[Order.State], firstMove: Option[OrderMoved] = None)
  : Checked[Vector[OrderMoved]] =
    //@tailrec
    def loop(order: Order[Order.State], visited: Vector[OrderMoved]): Checked[Vector[OrderMoved]] =
      if state.isOrderAtBreakpoint(order) then
        Right(visited)
      else
        nextMove(order) match
          case Left(problem) => Left(problem)

          case Right(Some(orderMoved)) =>
            if visited.exists(_.to == orderMoved.to) then
              Left(Problem(s"${order.id} is in a workflow loop: " +
                visited.reverse
                  .map(moved => moved.toString + " " +
                    idToWorkflow.checked(order.workflowId)
                      .flatMap(_.labeledInstruction(moved.to))
                      .fold(_.toString, _.toString)
                      .truncateWithEllipsis(50))
                  .mkString(" --> ")))
            else
              for
                order <- order.applyEvent(orderMoved)
                events <- loop(
                  order,
                  if orderMoved.reason.isEmpty && visited.lastOption.exists(_.reason.isEmpty) then
                    visited.updated(visited.length - 1, orderMoved)
                  else
                    visited :+ orderMoved)
              yield events

          case Right(None) => Right(visited)

    firstMove match
      case None => loop(order, Vector.empty)
      case Some(move) =>
        for
          order <- order.applyEvent(move)
          events <- loop(order, Vector(move))
        yield events

  private def nextMove(order: Order[Order.State]): Checked[Option[OrderMoved]] =
    for
      workflow <- idToWorkflow.checked(order.workflowId)
      maybeMoved <-
        if workflow.isOrderAtStopPosition(order) then
          Right(None)
        else
          ifSkippedDueToWorkflowPathControlThenMove(order) match
            case Some(orderMoved) =>
              Right(Some(orderMoved))
            case None =>
              executorService.nextMove(workflow.instruction(order.position), order, state)
    yield
      maybeMoved

  private def ifSkippedDueToWorkflowPathControlThenMove(order: Order[Order.State])
  : Option[OrderMoved] =
    (order.isSkippable(clock.now()) && isSkippedDueToWorkflowPathControl(order)) ?
      OrderMoved(order.position.increment, Some(OrderMoved.SkippedDueToWorkflowPathControl))

  private def isSkippedDueToWorkflowPathControl(order: Order[Order.State]): Boolean =
    !order.isState[Order.BetweenCycles] &&
      state.pathToWorkflowPathControl.get(WorkflowPathControlPath(order.workflowPath))
        .exists: control =>
          state.workflowPositionToLabel(order.workflowPosition)
            .toOption.flatten
            .exists(control.skip.contains)

  private def instruction_[A <: Instruction: ClassTag](orderId: OrderId): Checked[A] =
    for
      order <- idToOrder.checked(orderId)
      instr <- instruction_[A](order.workflowPosition)
    yield instr

  private def instruction_[A <: Instruction: ClassTag](workflowPosition: WorkflowPosition)
  : Checked[A] =
    for
      workflow <- idToWorkflow.checked(workflowPosition.workflowId)
      instr <- workflow.instruction_[A](workflowPosition.position)
    yield instr

  private def instruction(workflowPosition: WorkflowPosition): Instruction =
    idToWorkflow.checked(workflowPosition.workflowId) match
      case Left(_) =>
        logger.error(s"Missing ${workflowPosition.workflowId}")
        Gap.empty
      case Right(workflow) =>
        workflow.instruction(workflowPosition.position)

  private def atController[E <: OrderActorEvent](events: => List[E]): List[E | OrderDetachable] =
    state.atController(events)


object OrderEventSource:
  private val logger = Logger[this.type]

  def leaveBlocks(
    workflow: Workflow,
    order: Order[Order.State],
    events: List[OrderActorEvent])
  : Checked[List[OrderActorEvent]] =
    leaveBlocks(workflow, order, _ => events, until = _ => false)

  def leaveBlocks(
    workflow: Workflow, order: Order[Order.State],
    events: Option[BranchId] => List[OrderActorEvent],
    until: BranchId => Boolean)
  : Checked[List[OrderActorEvent]] =
    leaveBlocksThen(workflow, order, catchable = false, until = until): (maybeBranchId, _) =>
      events(maybeBranchId)

  private def leaveBlocksThen(
    workflow: Workflow,
    order: Order[Order.State],
    catchable: Boolean,
    until: BranchId => Boolean = _ => false)
    (toEvent: PartialFunction[(Option[BranchId], Position), List[OrderActorEvent]])
  : Checked[List[OrderActorEvent]] =
    leaveBlocksThen2(workflow, order, catchable, until)(toEvent).map: events =>
      order.resetState ::: events

  private def leaveBlocksThen2(
    workflow: Workflow, order: Order[Order.State],
    catchable: Boolean,
    until: BranchId => Boolean = _ => false)
    (toEvent: PartialFunction[(Option[BranchId], Position), List[OrderActorEvent]])
  : Checked[List[OrderActorEvent]] =
    catchNonFatalFlatten:
      def callToEvent(branchId: Option[BranchId], pos: Position) =
        toEvent.lift((branchId, pos))
          .map(Right(_))
          .getOrElse(Left(Problem(
            s"Unexpected BranchId '$branchId' while leaving instruction blocks")))

      val reverseInnerBlock = order.innerBlock.reverse

      def loop(reverseBranchPath: List[Segment], failPosition: Position)
      : Checked[List[OrderActorEvent]] =
        if reverseBranchPath == reverseInnerBlock then
          callToEvent(None, failPosition)
        else
          reverseBranchPath match
            case Nil =>
              callToEvent(None, failPosition)

            case Segment(nr, branchId) :: _ if until(branchId) =>
              callToEvent(Some(branchId), failPosition).map: events =>
                OrderMoved(reverseBranchPath.reverse % nr.increment) :: events

            case Segment(_, branchId @ BranchId.IsFailureBoundary(_)) :: _ =>
              callToEvent(Some(branchId), failPosition)

            case Segment(nr, BranchId.Lock) :: prefix =>
              if order.isAttached then
                Right(OrderDetachable :: Nil)
              else
                val pos = prefix.reverse % nr
                for
                  lock <- workflow.instruction_[LockInstruction](pos)
                  events <- loop(prefix, pos)
                yield
                  OrderLocksReleased(lock.lockPaths) :: events

            case Segment(nr, BranchId.ConsumeNotices) :: prefix =>
              if order.isAttached then
                Right(OrderDetachable :: Nil)
              else
                for events <- loop(prefix, prefix.reverse % nr) yield
                  OrderNoticesConsumed(failed = true) :: events

            case Segment(nr, BranchId.StickySubagent) :: prefix =>
              for events <- loop(prefix, prefix.reverse % nr) yield
                OrderStickySubagentLeaved :: events

            case Segment(nr, branchId @ TryBranchId(retry)) :: prefix if catchable =>
              val catchPos = prefix.reverse % nr / BranchId.catch_(retry) % 0
              if isMaxRetriesReached(workflow, catchPos) then
                loop(prefix, failPosition)
              else
                callToEvent(Some(branchId), catchPos)

            case Segment(_, _) :: prefix =>
              loop(prefix, failPosition)
      end loop

      loop(order.position.branchPath.reverse, order.position)
  end leaveBlocksThen2

  // Special handling for try with maxRetries and catch block with retry instruction only:
  // try (maxRetries=n) ... catch retry
  // In this case, OrderFailed event must have original failure's position, not failed retry's position.
  private def isMaxRetriesReached(workflow: Workflow, firstCatchPos: Position): Boolean =
    val catchStartsWithRetry =
      workflow.instruction(firstCatchPos).withoutSourcePos == Retry()
    catchStartsWithRetry &&
      firstCatchPos.parent.forall: parentPos =>
        workflow.instruction(parentPos) match  // Parent must be a TryInstruction
          case t: TryInstruction => t.maxTries.exists(firstCatchPos.tryCount >= _)

  /** Used in combination with `isFailed` to handle failed Orders transferred back to Controller. */
  private def isUncatchable(outcome: OrderOutcome): Boolean =
    outcome match
      case o: OrderOutcome.NotSucceeded => o.uncatchable
      case _ => false
