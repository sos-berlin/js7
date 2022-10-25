package js7.data.execution.workflow

import cats.instances.either.*
import cats.instances.list.*
import cats.syntax.traverse.*
import js7.base.problem.Checked.catchNonFatalFlatten
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Assertions.assertThat
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.Problems.CancelStartedOrderProblem
import js7.data.agent.AgentPath
import js7.data.command.{CancellationMode, SuspensionMode}
import js7.data.event.{<-:, KeyedEvent}
import js7.data.execution.workflow.OrderEventSource.*
import js7.data.execution.workflow.instructions.InstructionExecutorService
import js7.data.order.Order.{Cancelled, Failed, FailedInFork, IsFreshOrReady, IsTerminated, ProcessingKilled}
import js7.data.order.OrderEvent.{OrderActorEvent, OrderAwoke, OrderBroken, OrderCancellationMarked, OrderCancelled, OrderCaught, OrderCoreEvent, OrderDeleted, OrderDetachable, OrderFailed, OrderFailedInFork, OrderFailedIntermediate_, OrderLocksDequeued, OrderLocksReleased, OrderMoved, OrderNoticesConsumed, OrderOperationCancelled, OrderPromptAnswered, OrderResumed, OrderResumptionMarked, OrderStepFailed, OrderSuspended, OrderSuspensionMarked}
import js7.data.order.{Order, OrderId, OrderMark, Outcome}
import js7.data.problems.{CannotResumeOrderProblem, CannotSuspendOrderProblem, UnreachableOrderPositionProblem}
import js7.data.state.StateView
import js7.data.workflow.instructions.{End, Finish, ForkInstruction, Gap, LockInstruction, Retry, TryInstruction}
import js7.data.workflow.position.BranchPath.Segment
import js7.data.workflow.position.{BranchId, Position, TryBranchId, WorkflowPosition}
import js7.data.workflow.{Instruction, Workflow}
import scala.annotation.tailrec
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
final class OrderEventSource(state: StateView)
  (implicit executorService: InstructionExecutorService)
{
  import state.{idToOrder, idToWorkflow, isAgent}

  def nextEvents(orderId: OrderId): Seq[KeyedEvent[OrderActorEvent]] = {
    val order = idToOrder(orderId)
    if (order.isState[Order.Broken])
      Nil // Avoid issuing a second OrderBroken (would be a loop)
    else if (!weHave(order))
      Nil
    else
      orderMarkKeyedEvent(order).getOrElse(
        if (order.isSuspended)
          Nil
        else if (state.isOrderAtBreakpoint(order))
          atController(OrderSuspended :: Nil)
            .map(order.id <-: _)
        else if (state.isWorkflowSuspended(order.workflowPath))
          Nil
        else
          checkedNextEvents(order) |> (invalidToEvent(order, _)))
  }

  private def checkedNextEvents(order: Order[Order.State])
  : Checked[Seq[KeyedEvent[OrderActorEvent]]] =
    if (!order.lastOutcome.isSucceeded && order.isDetached && order.isState[IsFreshOrReady])
      // Do not go here when Order.Processed(Outcome.processLost) !
      // ExecuteExecutor handles this to allow to retry execution.
      fail(order)
        .map(_.map(order.id <-: _))
    else
      catchNonFatalFlatten {
        def ifDefinedElse(o: Option[List[OrderActorEvent]], orElse: Checked[List[KeyedEvent[OrderActorEvent]]]) =
          o.fold(orElse)(events => Right(events.map(order.id <-: _)))

        ifDefinedElse(awokeEvent(order).map(_ :: Nil),
          joinedEvent(order) match {
            case Left(problem) => Left(problem)
            case Right(Some(event)) => Right(event :: Nil)
            case Right(None) =>
              if (state.isOrderAtStopPosition(order))
                executorService.finishExecutor.toEvents(Finish(), order, state)
              else
                executorService.toEvents(instruction(order.workflowPosition), order, state)
                  // Multiple returned events are expected to be independent
                  // and are applied to the same idToOrder !!!
                  .flatMap(_
                    .flatTraverse {
                      case orderId <-: (moved: OrderMoved) =>
                        applyMoveInstructions(order, moved)
                          .map(event => (orderId <-: event) :: Nil)

                      case orderId <-: OrderFailedIntermediate_(outcome, uncatchable) =>
                        // OrderFailedIntermediate_ is used internally only
                        assertThat(orderId == order.id)
                        fail(order, outcome, uncatchable)
                          .map(_.map(orderId <-: _))

                      case o => Right(o :: Nil)
                    })
          })
      .flatMap(checkEvents)
    }

  private def checkEvents(keyedEvents: Seq[KeyedEvent[OrderActorEvent]]) = {
    var id2o = Map.empty[OrderId, Checked[Order[Order.State]]]
    var problem: Option[Problem] = None
    for (KeyedEvent(orderId, event) <- keyedEvents if problem.isEmpty) {
      id2o.getOrElse(orderId, idToOrder.checked(orderId)).flatMap(_.applyEvent(event)) match {
        case Left(prblm) => problem = Some(prblm)
        case Right(order) => id2o += orderId -> Right(order)
      }
    }
    problem.toLeft(keyedEvents)
  }

  private def invalidToEvent(order: Order[Order.State], checkedEvents: Checked[Seq[KeyedEvent[OrderActorEvent]]])
  : Seq[KeyedEvent[OrderActorEvent]] =
    checkedEvents match {
      case Left(problem) =>
        val events =
          if (order.isOrderFailedApplicable)
            fail(order, Some(Outcome.Disrupted(problem)), uncatchable = true) match {
              case Left(prblm) =>
                logger.debug(s"WARN ${order.id}: $prblm")
                OrderBroken(problem) :: Nil
              case Right(events) => events
            }
          else if (order.isAttached && order.isInDetachableState
            && order.copy(attachedState = None).isOrderFailedApplicable) {
            logger.debug(s"Detaching ${order.id} after failure: $problem")
            OrderStepFailed(Outcome.Disrupted(problem)) :: OrderDetachable :: Nil
          } else
            OrderBroken(problem) :: Nil
        events.map(order.id <-: _)

      case Right(o) => o
    }

  private[data] def fail(
    order: Order[Order.State],
    outcome: Option[Outcome.NotSucceeded] = None,
    uncatchable: Boolean = false)
  : Checked[List[OrderActorEvent]] =
    for {
      workflow <- idToWorkflow.checked(order.workflowId)
      events <- fail(workflow, order, outcome, uncatchable = uncatchable)
    } yield events

  private[workflow] def fail(
    workflow: Workflow,
    order: Order[Order.State],
    outcome: Option[Outcome.NotSucceeded],
    uncatchable: Boolean)
  : Checked[List[OrderActorEvent]] = {
    val stepFailed = outcome.map(OrderStepFailed(_)).toList
    leaveBlocks(workflow, order, catchable = !uncatchable) {
      case (None | Some(BranchId.IsFailureBoundary(_)), failPosition) =>
        atController {
          // TODO Transfer parent order to Agent to access joinIfFailed there !
          // For now, order will be moved to Controller, which joins the orders anyway.
          lazy val joinIfFailed = order.parent
            .flatMap(forkOrder => instruction_[ForkInstruction](forkOrder).toOption)
            .fold(false)(_.joinIfFailed)
          if (joinIfFailed)
            OrderFailedInFork(failPosition) :: Nil
          else
            OrderFailed(failPosition) :: Nil
        }

      case (Some(TryBranchId(_)), catchPos) =>
        OrderCaught(catchPos) :: Nil
    }.map(stepFailed ++: _)
  }

  private def joinedEvent(order: Order[Order.State]): Checked[Option[KeyedEvent[OrderActorEvent]]] =
    if (order.parent.isDefined
      && (order.isDetached || order.isAttached)
      && (order.isState[FailedInFork] || order.isState[Cancelled]))
      for {
        forkPosition <- order.forkPosition
        fork <- state.instruction_[ForkInstruction](order.workflowId /: forkPosition)
      } yield executorService.tryJoinChildOrder(fork, order, state)
    else
      Right(None)

  private def awokeEvent(order: Order[Order.State]): Option[OrderActorEvent] =
    ((order.isDetached || order.isAttached) && order.isState[Order.DelayedAfterError]) ?
      OrderAwoke  // AgentOrderKeeper has already checked time

  private def orderMarkKeyedEvent(order: Order[Order.State])
  : Option[List[KeyedEvent[OrderActorEvent]]] =
    orderMarkEvent(order)
      .map(_.map(order.id <-: _))

  private def orderMarkEvent(order: Order[Order.State]): Option[List[OrderActorEvent]] =
    if (order.deleteWhenTerminated && order.isState[IsTerminated] && order.parent.isEmpty)
      Some(OrderDeleted :: Nil)
    else
      order.mark.flatMap(mark =>
        mark match {
          case OrderMark.Cancelling(mode) =>
            tryCancel(order, mode)

          case OrderMark.Suspending(_) =>
            trySuspend(order)
              .map(_ :: Nil)

          case OrderMark.Resuming(position, historyOperations, asSucceeded) =>
            tryResume(order, position, historyOperations, asSucceeded)
              .map(_ :: Nil)

          case _ => None
        })

  def markOrder(orderId: OrderId, mark: OrderMark): Checked[Option[List[OrderActorEvent]]] =
    catchNonFatalFlatten {
      assertThat(isAgent)
      mark match {
        case OrderMark.Cancelling(mode) =>
          cancel(orderId, mode)

        case OrderMark.Suspending(mode) =>
          suspend(orderId, mode)

        case OrderMark.Resuming(position, historicOutcomes, asSucceeded) =>
          resume(orderId, position, historicOutcomes, asSucceeded)
      }
    }

  /** Returns `Right(Some(OrderCancelled | OrderCancellationMarked))` iff order is not already marked as cancelling. */
  def cancel(orderId: OrderId, mode: CancellationMode): Checked[Option[List[OrderActorEvent]]] =
    catchNonFatalFlatten {
      withOrder(orderId)(order =>
        if (mode == CancellationMode.FreshOnly && order.isStarted)
          // On Agent, the Order may already have been started without notice of the Controller
          Left(CancelStartedOrderProblem(orderId))
        else Right(
          tryCancel(order, mode).orElse(
            ( !order.isState[IsTerminated] &&
              !order.isState[ProcessingKilled] &&
              !order.mark.contains(OrderMark.Cancelling(mode))
            ) ? (OrderCancellationMarked(mode) :: Nil))))
    }

  private def tryCancel(order: Order[Order.State], mode: CancellationMode): Option[List[OrderActorEvent]] =
    (weHave(order) && isOrderCancelable(order, mode)) ?
      atController(
        order.state.isOperationCancelable.thenList(OrderOperationCancelled) :::
          leaveBlocks(idToWorkflow(order.workflowId), order, OrderCancelled)
            .orThrow/*???*/)

  private def isOrderCancelable(order: Order[Order.State], mode: CancellationMode): Boolean =
    (mode != CancellationMode.FreshOnly || order.isState[Order.Fresh]) &&
      order.isCancelable &&
      // If workflow End is reached, the order is finished normally
      !instruction(order.workflowPosition).isInstanceOf[End]  // TODO Correct? Or should we check only the end of the main/forked workflow?

  /** Returns a `Right(Some(OrderSuspended | OrderSuspensionMarked))` iff order is not already marked as suspending. */
  def suspend(orderId: OrderId, mode: SuspensionMode): Checked[Option[List[OrderActorEvent]]] =
    catchNonFatalFlatten {
      withOrder(orderId)(order =>
        order.mark match {
          case Some(_: OrderMark.Cancelling) =>
            Left(CannotSuspendOrderProblem)
          case Some(_: OrderMark.Suspending) =>  // Already marked
            Right(None)
          case None | Some(_: OrderMark.Resuming) =>
            if (order.isState[Failed] || order.isState[IsTerminated])
              Left(CannotSuspendOrderProblem)
            else
              Right(
                (!order.isSuspended || order.isResuming) ?
                  (trySuspend(order).getOrElse(OrderSuspensionMarked(mode))
                    :: Nil))
        })
    }

  private def trySuspend(order: Order[Order.State]): Option[OrderActorEvent] =
    if (!weHave(order) || !order.isSuspendible)
      None
    else if (isAgent)
      Some(OrderDetachable)
    else if (!order.isSuspended || order.isResuming)
      Some(OrderSuspended)
    else
      None

  /** Returns a `Right(Some(OrderResumed | OrderResumptionMarked))`
   * iff order is not already marked as resuming. */
  def resume(
    orderId: OrderId,
    position: Option[Position],
    historyOperations: Seq[OrderResumed.HistoryOperation],
    asSucceeded: Boolean)
  : Checked[Option[List[OrderActorEvent]]] =
    catchNonFatalFlatten {
      withOrder(orderId) { order =>
        lazy val checkedWorkflow = idToWorkflow.checked(order.workflowId)

        val checkPosition = position.fold(Checked.unit)(position =>
          checkedWorkflow.flatMap(workflow =>
            workflow.isMoveable(order.position, position) !! UnreachableOrderPositionProblem))

        val checkHistoricPositions =
          checkedWorkflow.flatMap(workflow =>
            historyOperations
              .flatMap(_.positions)
              .traverse(workflow.checkedPosition))

        checkPosition
          .flatMap(_ => checkHistoricPositions)
          .flatMap(_ =>
            order.mark match {
              case Some(_: OrderMark.Cancelling) =>
                Left(CannotResumeOrderProblem)

              case Some(OrderMark.Resuming(`position`, `historyOperations`, asSucceeded)) =>
                Right(order.isDetached ?
                  // should already have happened
                  List(OrderResumed(
                    position,
                    historyOperations,
                    asSucceeded)))

              case Some(OrderMark.Resuming(_, _, _)) =>
                 Left(CannotResumeOrderProblem)

              case Some(OrderMark.Suspending(_)) if position.isDefined || historyOperations.nonEmpty =>
                 Left(CannotResumeOrderProblem)

              case None | Some(OrderMark.Suspending(_)) =>
                val okay = order.isSuspended ||
                  order.mark.exists(_.isInstanceOf[OrderMark.Suspending]) ||
                  order.isState[Failed] && order.isDetached
                if (!okay)
                  Left(CannotResumeOrderProblem)
                else
                  Right(Some(
                    tryResume(order, position, historyOperations, asSucceeded)
                      .getOrElse(OrderResumptionMarked(position, historyOperations, asSucceeded))
                      :: Nil))
            })
      }
    }

  /** Retrieve the Order, check if calculated event is applicable. */
  private def withOrder[E <: OrderCoreEvent](orderId: OrderId)
    (body: Order[Order.State] => Checked[Option[List[E]]])
  : Checked[Option[List[E]]] =
    idToOrder
      .checked(orderId)
      .flatMap(order => body(order)
        .flatMapT(events =>
          order
            .applyEvents(events)
            .map(_ => Some(events))))

  private def tryResume(
    order: Order[Order.State],
    position: Option[Position],
    historyOperations: Seq[OrderResumed.HistoryOperation],
    asSucceeded: Boolean)
  : Option[OrderActorEvent] =
    (weHave(order) && order.isResumable) ? OrderResumed(position, historyOperations, asSucceeded)

  def answer(orderId: OrderId): Checked[Seq[KeyedEvent[OrderCoreEvent]]] =
    catchNonFatalFlatten {
      for {
        order <- idToOrder.checked(orderId)
        _ <- order.checkedState[Order.Prompting]
      } yield
        Seq(
          orderId <-: OrderPromptAnswered(),
          orderId <-: OrderMoved(order.position.increment))
    }

  private def weHave(order: Order[Order.State]) =
    order.isDetached && !isAgent ||
    order.isAttached && isAgent

  def nextAgent(order: Order[Order.State]): Checked[Option[AgentPath]] =
    catchNonFatalFlatten {
      for (pos <- applyMoveInstructions(order)) yield
        for {
          workflow <- idToWorkflow.get(order.workflowId)
          agentPath <- workflow.agentPath(pos)
        } yield agentPath
    }

  private def applyMoveInstructions(order: Order[Order.State], orderMoved: OrderMoved): Checked[OrderMoved] =
    applyMoveInstructions(order.withPosition(orderMoved.to))
      .map(OrderMoved(_))

  private[workflow] def applyMoveInstructions(order: Order[Order.State]): Checked[Position] = {
    @tailrec
    def loop(order: Order[Order.State], visited: List[Position]): Checked[Option[Position]] =
      applySingleMoveInstruction(order) match {
        case o @ Left(_) => o

        case Right(Some(position)) =>
          if (visited contains position)
            Left(Problem(s"${order.id} is in a workflow loop: " +
              visited.reverse.map(pos => pos.toString + " " +
                idToWorkflow.checked(order.workflowId).flatMap(_.labeledInstruction(pos))
                  .fold(_.toString, _.toString).truncateWithEllipsis(50)).mkString(" --> ")))
          else
            loop(order.withPosition(position), position :: visited)

        case Right(None) => Right(Some(order.position))
      }

    loop(order, Nil) map {
      case Some(n) => n
      case None => order.position
    }
  }

  private def applySingleMoveInstruction(order: Order[Order.State]): Checked[Option[Position]] =
    for {
      workflow <- idToWorkflow.checked(order.workflowId)
      maybePosition <-
        if (workflow.isOrderAtStopPosition(order))
          Right(None)
        else
          executorService.nextPosition(workflow.instruction(order.position), order, state)
    } yield maybePosition

  private def instruction_[A <: Instruction: ClassTag](orderId: OrderId): Checked[A] = {
    for {
      order <- idToOrder.checked(orderId)
      instr <- instruction_[A](order.workflowPosition)
    } yield instr
  }

  private def instruction_[A <: Instruction: ClassTag](workflowPosition: WorkflowPosition)
  : Checked[A] =
    for {
      workflow <- idToWorkflow.checked(workflowPosition.workflowId)
      instr <- workflow.instruction_[A](workflowPosition.position)
    } yield instr

  private def instruction(workflowPosition: WorkflowPosition): Instruction =
    idToWorkflow.checked(workflowPosition.workflowId) match {
      case Left(_) =>
        logger.error(s"Missing ${workflowPosition.workflowId}")
        Gap.empty
      case Right(workflow) =>
        workflow.instruction(workflowPosition.position)
    }

  private def atController(events: List[OrderActorEvent]): List[OrderActorEvent] =
    if (isAgent)
      OrderDetachable :: Nil
    else
      events
}

object OrderEventSource {
  private val logger = scribe.Logger[this.type]

  def leaveBlocks(workflow: Workflow, order: Order[Order.State], event: OrderActorEvent)
  : Checked[List[OrderActorEvent]] =
    leaveBlocks(workflow, order, catchable = false) {
      case _ => event :: Nil
    }

  private def leaveBlocks(workflow: Workflow, order: Order[Order.State], catchable: Boolean)
    (toEvent: PartialFunction[(Option[BranchId], Position), List[OrderActorEvent]])
  : Checked[List[OrderActorEvent]] =
    catchNonFatalFlatten {
      def callToEvent(branchId: Option[BranchId], pos: Position) =
        toEvent.lift((branchId, pos))
          .map(Right(_))
          .getOrElse(Left(Problem(
            s"Unexpected BranchId '$branchId' while leaving instruction blocks")))

      def loop(reverseBranchPath: List[Segment], failPosition: Position)
      : Checked[List[OrderActorEvent]] =
        reverseBranchPath match {
          case Nil =>
            callToEvent(None, failPosition)

          case Segment(_, branchId @ BranchId.IsFailureBoundary(_)) :: _ =>
            callToEvent(Some(branchId), failPosition)

          case Segment(nr, BranchId.Lock) :: prefix =>
            val pos = prefix.reverse % nr
            for {
              lock <- workflow.instruction_[LockInstruction](pos)
              events <- loop(prefix, pos)
            } yield OrderLocksReleased(lock.lockPaths) :: events

          case Segment(nr, BranchId.ConsumeNotices) :: prefix =>
            val pos = prefix.reverse % nr
            loop(prefix, pos)
              .map(OrderNoticesConsumed(failed = true) :: _)

          case Segment(nr, branchId @ TryBranchId(retry)) :: prefix if catchable =>
            val catchPos = prefix.reverse % nr / BranchId.catch_(retry) % 0
            if (isMaxRetriesReached(workflow, catchPos))
              loop(prefix, failPosition)
            else
              callToEvent(Some(branchId), catchPos)

          case Segment(_, _) :: prefix =>
            loop(prefix, failPosition)
        }

      order
        .ifState[Order.WaitingForLock]
        .traverse(order =>
          for (lock <- workflow.instruction_[LockInstruction](order.position)) yield
            OrderLocksDequeued(lock.lockPaths))
        .flatMap(maybeEvent =>
          loop(order.position.branchPath.reverse, order.position)
            .map(maybeEvent.toList ::: _))
    }

  // Special handling for try with maxRetries and catch block with retry instruction only:
  // try (maxRetries=n) ... catch retry
  // In this case, OrderFailed event must have original failure's position, not failed retry's position.
  private def isMaxRetriesReached(workflow: Workflow, firstCatchPos: Position): Boolean = {
    val catchStartsWithRetry =
      workflow.instruction(firstCatchPos).withoutSourcePos == Retry()
    catchStartsWithRetry &&
      firstCatchPos.parent.forall(parentPos =>
        workflow.instruction(parentPos) match { // Parent must be a TryInstruction
          case t: TryInstruction => t.maxTries.forall(firstCatchPos.tryCount >= _)
        })
  }
}
