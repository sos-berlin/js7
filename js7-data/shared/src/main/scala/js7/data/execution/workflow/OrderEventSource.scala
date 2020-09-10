package js7.data.execution.workflow

import cats.instances.either._
import js7.base.problem.Checked._
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Assertions.assertThat
import js7.base.utils.ScalaUtils.syntax._
import js7.data.Problems.{CancelChildOrderProblem, CancelStartedOrderProblem}
import js7.data.command.CancelMode
import js7.data.event.{<-:, KeyedEvent}
import js7.data.execution.workflow.context.OrderContext
import js7.data.execution.workflow.instructions.{ForkExecutor, InstructionExecutor}
import js7.data.order.Order.{IsFinal, ProcessingCancelled}
import js7.data.order.OrderEvent.{OrderActorEvent, OrderAwoke, OrderBroken, OrderCancelMarked, OrderCancelled, OrderCatched, OrderDetachable, OrderFailed, OrderFailedCatchable, OrderFailedInFork, OrderMoved, OrderResumeMarked, OrderResumed, OrderSuspendMarked, OrderSuspended}
import js7.data.order.{Order, OrderId, OrderMark, Outcome}
import js7.data.workflow.instructions.{End, Fork, Goto, IfFailedGoto, Retry, TryInstruction}
import js7.data.workflow.position.{Position, WorkflowPosition}
import js7.data.workflow.{Instruction, Workflow, WorkflowId}
import scala.annotation.tailrec

/**
  * @author Joacim Zschimmer
  */
final class OrderEventSource(
  idToWorkflow: WorkflowId => Checked[Workflow],
  idToOrder: OrderId => Checked[Order[Order.State]],
  isAgent: Boolean)
{
  private val context = new OrderContext {
    // This and idToOrder are mutable, do not use in Future !!!
    def idToOrder                                   = OrderEventSource.this.idToOrder
    def instruction(position: WorkflowPosition)     = OrderEventSource.this.instruction(position)
    def idToWorkflow(id: WorkflowId)                = OrderEventSource.this.idToWorkflow(id)

    def childOrderEnded(order: Order[Order.State]): Boolean =
      order.parent.flatMap(o => idToOrder(o).toOption) match {
        case Some(parentOrder) =>
          lazy val endReached = instruction(order.workflowPosition).isInstanceOf[End] &&
            order.state == Order.Ready &&
            order.position.dropChild.contains(parentOrder.position)
          order.attachedState == parentOrder.attachedState &&
            (endReached || order.isState[Order.FailedInFork])
        case _ => false
      }
  }

  def nextEvent(orderId: OrderId): Option[KeyedEvent[OrderActorEvent]] = {
    val order = idToOrder(orderId).orThrow
    if (order.isState[Order.Broken])
      None  // Avoid issuing a second OrderBroken (would be a loop)
    else
      checkedNextEvent(order) |> (invalidToEvent(order, _))
  }

  private def checkedNextEvent(order: Order[Order.State]): Checked[Option[KeyedEvent[OrderActorEvent]]] = {
    def f(o: Option[OrderActorEvent]) = Checked(o.map(order.id <-: _))
    f(orderMarkEvent(order)).orElseT(
      if (!weHave(order))
        Right(None)
      else
        f(awokeEvent(order)).orElseT(
          joinedEvent(order).orElseT(
            InstructionExecutor.toEvent(instruction(order.workflowPosition), order, context) match {
              case Right(Some(orderId <-: (moved: OrderMoved))) =>
                applyMoveInstructions(orderId, moved) map Some.apply

              case Right(Some(orderId <-: OrderFailedCatchable(outcome))) =>  // OrderFailedCatchable is used internally only
                assertThat(orderId == order.id)
                findCatchPosition(order) match {
                  case Some(firstCatchPos) if !isMaxRetriesReached(order, firstCatchPos) =>
                    applyMoveInstructions(order.withPosition(firstCatchPos))
                      .flatMap(movedPos => Right(Some(orderId <-: OrderCatched(outcome, movedPos))))
                  case _ =>
                    Right(Some(orderId <-: (if (order.position.isInFork) OrderFailedInFork(outcome) else OrderFailed(outcome))))
                }

              case o => o
            })))
  }

  // Special handling for try with maxRetries and catch block with retry instruction only:
  // try (maxRetries=n) ... catch retry
  // In this case, OrderFailed event must have original failures's position, not failed retry's position.
  private def isMaxRetriesReached(order: Order[Order.State], firstCatchPos: Position): Boolean =
    catchStartsWithRetry(order.workflowId /: firstCatchPos) &&
      firstCatchPos.dropChild.forall(parentPos =>
        instruction(order.workflowId /: parentPos) match {  // Parent must be a TryInstruction
          case t: TryInstruction => t.maxTries.forall(firstCatchPos.tryCount >= _)
        })

  private def catchStartsWithRetry(firstCatchPos: WorkflowPosition) =
    instruction(firstCatchPos).withoutSourcePos == Retry()

  private def invalidToEvent[A](order: Order[Order.State], checkedEvent: Checked[Option[KeyedEvent[OrderActorEvent]]])
  : Option[KeyedEvent[OrderActorEvent]] =
    checkedEvent match {
      case Left(problem)  =>
        if (order.isOrderFailedApplicable)
          Some(order.id <-: OrderFailed(Outcome.Disrupted(problem)))
        else
          Some(order.id <-: OrderBroken(problem))

      case Right(o) => o
    }

  private def findCatchPosition(order: Order[Order.State]): Option[Position] =
    for {
      workflow <- idToWorkflow(order.workflowId).toOption
      position <- workflow.findCatchPosition(order.position)
    } yield position

  private def joinedEvent(order: Order[Order.State]): Checked[Option[KeyedEvent[OrderActorEvent]]] =
    if ((order.isDetached || order.isAttached) && order.isState[Order.FailedInFork])
      order.forkPosition.flatMap(forkPosition =>
        context.instruction(order.workflowId /: forkPosition) match {
          case fork: Fork =>
            Right(ForkExecutor.tryJoinChildOrder(context, order, fork))
          case _ =>
            // Self-test
            Left(Problem.pure(s"Order '${order.id}' is in state FailedInFork but forkPosition does not denote a fork instruction"))
      })
    else Right(None)

  private def awokeEvent(order: Order[Order.State]): Option[OrderActorEvent] =
    ((order.isDetached || order.isAttached) && order.isState[Order.DelayedAfterError]) ?
      OrderAwoke  // AgentOrderKeeper has already checked time

  /** Convert OrderMark to an event.
    * Returns `Some(OrderCancelled | OrderSuspended | OrderResumed)` or `Some(OrderDetachable)` or None.
    */
  private def orderMarkEvent(order: Order[Order.State]): Option[OrderActorEvent] =
    order.mark.flatMap(mark =>
      mark match {
        case OrderMark.Cancelling(mode) => tryCancel(order, mode)
        case OrderMark.Suspending => trySuspend(order)
        case OrderMark.Resuming => tryResume(order)
        case _ => None
      })

  def markOrder(orderId: OrderId, mark: OrderMark): Checked[Option[OrderActorEvent]] = {
    assertThat(isAgent)
    mark match {
      case OrderMark.Cancelling(mode) =>
        cancel(orderId, mode)

      case OrderMark.Suspending =>
        suspend(orderId)

      case OrderMark.Resuming =>
        resume(orderId)
    }
  }

  /** Returns `Right(Some(OrderCancelled | OrderCancelMarked))` iff order is not already marked as cancelling. */
  def cancel(orderId: OrderId, mode: CancelMode): Checked[Option[OrderActorEvent]] =
    idToOrder(orderId).flatMap(order =>
      if (order.parent.isDefined)
        Left(CancelChildOrderProblem(orderId))
      else if (mode == CancelMode.NotStarted && order.isStarted) {
        // On Agent, the Order may already have been started without notice of the Controller
        Left(CancelStartedOrderProblem(orderId))
      } else Right(
        tryCancel(order, mode).orElse(
          (!order.isCancelling && !order.isState[IsFinal] && !order.isState[ProcessingCancelled]) ?
            OrderCancelMarked(mode))))

  private def tryCancel(order: Order[Order.State], mode: CancelMode): Option[OrderActorEvent] =
    if (isOrderCancelable(order, mode))
      if (order.isAttached && isAgent) Some(OrderDetachable)
      else if (order.isDetached && !isAgent) Some(OrderCancelled)
      else None
    else None

  private def isOrderCancelable(order: Order[Order.State], mode: CancelMode): Boolean =
    (mode != CancelMode.NotStarted || order.isState[Order.Fresh]) &&
      order.isCancelable &&
      // If workflow End is reached, the order is finished normally
      !instruction(order.workflowPosition).isInstanceOf[End]

  /** Returns a `Right(Some(OrderSuspended | OrderSuspendMarked))` iff order is not already marked as suspending. */
  def suspend(orderId: OrderId): Checked[Option[OrderActorEvent]] =
    idToOrder(orderId).flatMap(order =>
      if (order.isSuspended)
        Right(None)
      else
        order.mark match {
          case Some(_: OrderMark.Cancelling) =>
            Left(Problem.pure("Order cannot be suspended because it is cancelled"))
          case Some(OrderMark.Suspending)  =>  // Already marked
            Right(None)
          case None | Some(OrderMark.Resuming) =>
            Right(!order.isSuspended ? trySuspend(order).getOrElse(OrderSuspendMarked))
        })

  private def trySuspend(order: Order[Order.State]): Option[OrderActorEvent] =
    if (weHave(order) && isOrderSuspendible(order))
      if (order.isAttached && isAgent) Some(OrderDetachable)
      else if (order.isDetached) Some(OrderSuspended)
      else None
    else None

  private def isOrderSuspendible(order: Order[Order.State]): Boolean =
    weHave(order) && order.isSuspendible &&
      !instruction(order.workflowPosition).isInstanceOf[End]  // End reached? Then normal OrderFinished (not OrderSuspended)

  /** Returns a `Right(Some(OrderResumed | OrderResumeMarked))` iff order is not already marked as resuming. */
  def resume(orderId: OrderId): Checked[Option[OrderActorEvent]] =
    idToOrder(orderId).flatMap(order =>
      order.mark match {
        case Some(_: OrderMark.Cancelling) =>
          Left(Problem("Order cannot resume because it is being cancelled"))

        case None | Some(OrderMark.Resuming | OrderMark.Suspending) =>
          if (!order.isSuspended && !order.mark.contains(OrderMark.Suspending))
            Left(Problem("Order cannot resume because it is not suspended"))
          else
            Right(Some(tryResume(order) getOrElse OrderResumeMarked))
      })

  private def tryResume(order: Order[Order.State]): Option[OrderActorEvent] =
    (weHave(order) && order.isResumable) ?
      OrderResumed

  private def weHave(order: Order[Order.State]) =
    order.isDetached && !isAgent ||
    order.isAttached && isAgent

  private def applyMoveInstructions(orderId: OrderId, orderMoved: OrderMoved): Checked[KeyedEvent[OrderMoved]] =
    for (pos <- applyMoveInstructions(idToOrder(orderId).orThrow.withPosition(orderMoved.to)))
      yield orderId <-: OrderMoved(pos)

  private[workflow] def applyMoveInstructions(order: Order[Order.State]): Checked[Position] =
    applyMoveInstructions(order, Nil) map {
      case Some(n) => n
      case None => order.position
    }

  @tailrec
  private def applyMoveInstructions(order: Order[Order.State], visited: List[Position]): Checked[Option[Position]] =
    applySingleMoveInstruction(order) match {
      case o @ Left(_) => o
      case Right(Some(position)) =>
        if (visited contains position)
          Left(Problem(s"${order.id} is in a workflow loop: " +
            visited.reverse.map(pos => pos.toString + " " +
              idToWorkflow(order.workflowId).orThrow.labeledInstruction(pos).toString.truncateWithEllipsis(50)).mkString(" --> ")))
        else
          applyMoveInstructions(order.withPosition(position), position :: visited)
      case Right(None) => Right(Some(order.position))
    }

  private def applySingleMoveInstruction(order: Order[Order.State]): Checked[Option[Position]] =
    idToWorkflow(order.workflowId) flatMap { workflow =>
      workflow.instruction(order.position) match {
        case Goto(label, _) =>
          workflow.labelToPosition(order.position.branchPath, label) map Some.apply

        case IfFailedGoto(label, _) =>
          if (order.lastOutcome.isFailed)
            workflow.labelToPosition(order.position.branchPath, label) map Some.apply
          else
            Right(Some(order.position.increment))

        case instr: Instruction =>
          InstructionExecutor.nextPosition(context, order, instr)

        //case _: End if order.position.isNested =>
        //  order.position.dropChild flatMap (returnPosition =>
        //    workflow.instruction(returnPosition) match {
        //      case _: If =>
        //        nextPosition(order withPosition returnPosition)
        //      case _ =>
        //        None
        //    })

        case _ => Right(None)
      }
  }

  private def instruction(workflowPosition: WorkflowPosition): Instruction =
    idToWorkflow(workflowPosition.workflowId).orThrow.instruction(workflowPosition.position)
}
