package com.sos.jobscheduler.core.workflow

import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.ScalaUtils.RichPartialFunction
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.base.utils.Strings.RichString
import com.sos.jobscheduler.core.problems.{CancelChildOrderProblem, CancelStartedOrderProblem}
import com.sos.jobscheduler.core.workflow.instructions.{ForkExecutor, InstructionExecutor}
import com.sos.jobscheduler.data.command.CancelMode
import com.sos.jobscheduler.data.event.{<-:, KeyedEvent}
import com.sos.jobscheduler.data.order.OrderEvent.{OrderActorEvent, OrderAwoke, OrderBroken, OrderCancelationMarked, OrderCanceled, OrderCatched, OrderDetachable, OrderFailed, OrderFailedCatchable, OrderFailedInFork, OrderMoved}
import com.sos.jobscheduler.data.order.{Order, OrderId, Outcome}
import com.sos.jobscheduler.data.workflow.instructions.{End, Fork, Goto, IfFailedGoto, Retry, TryInstruction}
import com.sos.jobscheduler.data.workflow.position.{Position, WorkflowPosition}
import com.sos.jobscheduler.data.workflow.{Instruction, Workflow, WorkflowId}
import scala.annotation.tailrec

/**
  * @author Joacim Zschimmer
  */
final class OrderEventSource(
  idToWorkflow: WorkflowId => Checked[Workflow],
  idToOrder: PartialFunction[OrderId, Order[Order.State]])
{
  private val context = new OrderContext {
    // This and idToOrder are mutable, do not use in Future !!!
    def idToOrder                                   = OrderEventSource.this.idToOrder
    def instruction(position: WorkflowPosition)     = OrderEventSource.this.instruction(position)
    def idToWorkflow(id: WorkflowId)                = OrderEventSource.this.idToWorkflow(id)

    def childOrderEnded(order: Order[Order.State]): Boolean =
      order.parent flatMap idToOrder.lift match {
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
    val order = idToOrder(orderId)
    if (order.isState[Order.Broken])
      None  // Avoid issuing a second OrderBroken (will be a loop)
    else
      invalidToEvent(order, checkedNextEvent(order))
  }

  private def checkedNextEvent(order: Order[Order.State]): Checked[Option[KeyedEvent[OrderActorEvent]]] =
    joinedEvent(order) flatMap {
      case Some(o) => Right(Some(o))
      case None =>
        awokeEvent(order) orElse canceledEvent(order) match {
          case Some(event) => Right(Some(order.id <-: event))
          case None => checkedNextEvent2(order)
        }
    }

  private def checkedNextEvent2(order: Order[Order.State]): Checked[Option[KeyedEvent[OrderActorEvent]]] =
    InstructionExecutor.toEvent(instruction(order.workflowPosition), order, context) match {
      case Right(Some(oId <-: (moved: OrderMoved))) =>
        applyMoveInstructions(oId, moved) map Some.apply

      case Right(Some(oId <-: OrderFailedCatchable(outcome))) =>  // OrderFailedCatchable is used internally only
        assert(oId == order.id)
        findCatchPosition(order) match {
          case Some(firstCatchPos) if !isMaxRetriesReached(order, firstCatchPos) =>
            applyMoveInstructions(order.withPosition(firstCatchPos))
              .flatMap(movedPos => Right(Some(oId <-: OrderCatched(outcome, movedPos))))
          case _ =>
            if (order.position.isInFork)
              Right(Some(oId <-: OrderFailedInFork(outcome)))
            else
              Right(Some(oId <-: OrderFailed(outcome)))
        }

      case o => o
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
    if (order.isState[Order.FailedInFork])
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
    order.ifState[Order.DelayedAfterError]
      .map(_ => OrderAwoke)  // AgentOrderKeeper has already checked time

  /** Returns `Some(OrderDetachable | OrderCanceled)` iff order is marked as cancelable and order is in a cancelable state. */
  private def canceledEvent(order: Order[Order.State]): Option[OrderActorEvent] =
    if (isOrderCancelable(order))
      (order.isAttached ? OrderDetachable) orElse (order.isDetached ? OrderCanceled)
    else
      None

  /** Returns a `Right(Some(OrderCanceled | OrderCancelationMarked))` iff order is not already marked as cancelable. */
  def cancel(orderId: OrderId, mode: CancelMode, isAgent: Boolean): Checked[Option[OrderActorEvent]] =
    idToOrder.checked(orderId) flatMap (order =>
      if (order.parent.isDefined)
        Left(CancelChildOrderProblem(orderId))
      else if (mode == CancelMode.NotStarted && order.isStarted)
        Left(CancelStartedOrderProblem(orderId))
      else if (order.cancel.nonEmpty)
        Right(None)  // Already marked as being canceled
      else if (isAgent)
        if (isOrderCancelable(order, mode))
          Right(Some(OrderDetachable))
        else
          Right(Some(OrderCancelationMarked(mode)))
      else if (order.isDetached && isOrderCancelable(order, mode))
        Right(Some(OrderCanceled))
      else
        Right(Some(OrderCancelationMarked(mode))))

  def isOrderCancelable(order: Order[Order.State]): Boolean =
    order.cancel match {
      case None => false
      case Some(mode) => isOrderCancelable(order, mode)
    }

  private def isOrderCancelable(order: Order[Order.State], mode: CancelMode): Boolean =
    (order.isState[Order.Fresh] ||
      (mode == CancelMode.FreshOrStarted && (
        order.isState[Order.Ready] ||
        order.isState[Order.FailedWhileFresh] ||
        order.isState[Order.Failed] ||
        order.isState[Order.Broken]))
    ) &&
      (order.isDetached || order.isAttached) &&
      order.parent.isEmpty &&
      !instruction(order.workflowPosition).isInstanceOf[End]  // End reached? Then normal OrderFinished (not OrderCanceled)

  private def applyMoveInstructions(orderId: OrderId, orderMoved: OrderMoved): Checked[KeyedEvent[OrderMoved]] =
    for (pos <- applyMoveInstructions(idToOrder(orderId).withPosition(orderMoved.to)))
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
