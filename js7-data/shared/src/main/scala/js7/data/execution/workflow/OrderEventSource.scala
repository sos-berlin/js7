package js7.data.execution.workflow

import cats.instances.either._
import cats.instances.vector._
import cats.instances.list._
import cats.syntax.traverse._
import js7.base.problem.Checked._
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Assertions.assertThat
import js7.base.utils.ScalaUtils.syntax._
import js7.data.Problems.{CancelChildOrderProblem, CancelStartedOrderProblem}
import js7.data.command.{CancelMode, SuspendMode}
import js7.data.event.{<-:, KeyedEvent}
import js7.data.execution.workflow.context.OrderContext
import js7.data.execution.workflow.instructions.{ForkExecutor, InstructionExecutor}
import js7.data.lock.{LockName, LockState}
import js7.data.order.Order.{IsTerminated, ProcessingKilled}
import js7.data.order.OrderEvent.{OrderActorEvent, OrderAwoke, OrderBroken, OrderCancelMarked, OrderCancelled, OrderCatched, OrderCoreEvent, OrderDetachable, OrderFailed, OrderFailedInFork, OrderFailedIntermediate_, OrderMoved, OrderRemoved, OrderResumeMarked, OrderResumed, OrderSuspendMarked, OrderSuspended}
import js7.data.order.{HistoricOutcome, Order, OrderId, OrderMark, Outcome}
import js7.data.problems.{CannotResumeOrderProblem, CannotSuspendOrderProblem}
import js7.data.workflow.instructions.{End, Fork, Goto, IfFailedGoto, Retry, TryInstruction}
import js7.data.workflow.position.{BranchId, BranchPath, Position, WorkflowPosition}
import js7.data.workflow.{Instruction, Workflow, WorkflowId}
import scala.annotation.tailrec

/**
  * @author Joacim Zschimmer
  */
final class OrderEventSource(
  idToOrder: OrderId => Checked[Order[Order.State]],
  idToWorkflow: WorkflowId => Checked[Workflow],
  pathToLock: LockName => Checked[LockState],
  isAgent: Boolean)
{
  private val context = new OrderContext {
    def idToOrder                               = OrderEventSource.this.idToOrder
    def idToWorkflow(id: WorkflowId)            = OrderEventSource.this.idToWorkflow(id)
    def nameToLockState                         = OrderEventSource.this.pathToLock

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

  def nextEvents(orderId: OrderId): List[KeyedEvent[OrderActorEvent]] = {
    val order = idToOrder(orderId).orThrow
    if (order.isState[Order.Broken])
      Nil  // Avoid issuing a second OrderBroken (would be a loop)
    else
      checkedNextEvents(order) |> (invalidToEvent(order, _))
  }

  private def checkedNextEvents(order: Order[Order.State]): Checked[List[KeyedEvent[OrderActorEvent]]] = {
    def ifDefinedElse(o: Option[OrderActorEvent], orElse: Checked[List[KeyedEvent[OrderActorEvent]]]) =
      o.fold(orElse)(e => Right((order.id <-: e) :: Nil))
    ifDefinedElse(orderMarkEvent(order),
      if (!weHave(order) || order.isSuspended)
        Right(Nil)
      else
        ifDefinedElse(awokeEvent(order),
          joinedEvent(order) match {
            case Left(problem) => Left(problem)
            case Right(Some(event)) => Right(event :: Nil)
            case Right(None) =>
              InstructionExecutor.toEvents(instruction(order.workflowPosition), order, context)
                // Multiple returned events are expected to be independant and are applied to same idToOrder
                .flatMap(_
                  .traverse {
                    case orderId <-: (moved: OrderMoved) =>
                      applyMoveInstructions(orderId, moved)
                        .map(_ :: Nil)

                    case orderId <-: OrderFailedIntermediate_(outcome, uncatchable) =>
                      // OrderFailedIntermediate_ is used internally only
                      assertThat(orderId == order.id)
                      fail(order, outcome, uncatchable)

                    case o => Right(o :: Nil)
                  })
                .map(_.flatten)
          }))
      .flatMap(checkEvents)
  }

  private def checkEvents(keyedEvents: List[KeyedEvent[OrderActorEvent]]) = {
    var id2o = Map.empty[OrderId, Checked[Order[Order.State]]]
    var problem: Option[Problem] = None
    for (KeyedEvent(orderId, event) <- keyedEvents if problem.isEmpty) {
      id2o.getOrElse(orderId, idToOrder(orderId)).flatMap(_.update(event)) match {
        case Left(prblm) => problem = Some(prblm)
        case Right(order) => id2o += orderId -> Right(order)
      }
    }
    problem.toLeft(keyedEvents)
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

  private def invalidToEvent[A](order: Order[Order.State], checkedEvent: Checked[List[KeyedEvent[OrderActorEvent]]])
  : List[KeyedEvent[OrderActorEvent]] =
    checkedEvent match {
      case Left(problem) =>
        if (order.isOrderFailedApplicable)
          orderFailed(order, Some(Outcome.Disrupted(problem)))
        else if (order.isAttached && order.isInDetachableState && order.copy(attachedState = None).isOrderFailedApplicable) {
          scribe.warn(s"Detaching ${order.id} after failure: $problem")
          // Introduce a new 'OrderPrefailed' event for not loosing the problem ???
          // Controller should reproduce the problem.
          (order.id <-: OrderDetachable) :: Nil
        } else
          (order.id <-: OrderBroken(problem)) :: Nil

      case Right(o) => o
    }

  private def fail(order: Order[Order.State], outcome: Option[Outcome.NotSucceeded], uncatchable: Boolean)
  : Checked[List[KeyedEvent[OrderActorEvent]]] =
    findCatchPosition(order) match {
      case Some(firstCatchPos) if !uncatchable && !isMaxRetriesReached(order, firstCatchPos) =>
        applyMoveInstructions(order.withPosition(firstCatchPos))
          .flatMap(movedPos => Right((order.id <-: OrderCatched(outcome, movedPos)) :: Nil))
      case _ =>
        Right(orderFailed(order, outcome))
    }

  private def orderFailed(order: Order[Order.State], outcome: Option[Outcome.NotSucceeded]) =
    List(order.id <-: (
      if (order.position.isInFork)
        OrderFailedInFork(outcome)
      else if (order.isAttached)
        OrderDetachable
      else
        OrderFailed(outcome)))

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

  private def orderMarkEvent(order: Order[Order.State]): Option[OrderActorEvent] =
    if (order.removeWhenTerminated && order.isState[IsTerminated] && order.parent.isEmpty)
      Some(OrderRemoved)
    else
      order.mark.flatMap(mark =>
        mark match {
          case OrderMark.Cancelling(mode) => tryCancel(order, mode)
          case OrderMark.Suspending(_) => trySuspend(order)
          case OrderMark.Resuming(position, historicOutcomes) => tryResume(order, position, historicOutcomes)
          case _ => None
        })

  def markOrder(orderId: OrderId, mark: OrderMark): Checked[Option[OrderActorEvent]] = {
    assertThat(isAgent)
    mark match {
      case OrderMark.Cancelling(mode) =>
        cancel(orderId, mode)

      case OrderMark.Suspending(mode) =>
        suspend(orderId, mode)

      case OrderMark.Resuming(position, historicOutcomes) =>
        resume(orderId, position, historicOutcomes)
    }
  }

  /** Returns `Right(Some(OrderCancelled | OrderCancelMarked))` iff order is not already marked as cancelling. */
  def cancel(orderId: OrderId, mode: CancelMode): Checked[Option[OrderActorEvent]] =
    withOrder(orderId)(order =>
      if (order.parent.isDefined)
        Left(CancelChildOrderProblem(orderId))
      else if (mode == CancelMode.FreshOnly && order.isStarted)
        // On Agent, the Order may already have been started without notice of the Controller
        Left(CancelStartedOrderProblem(orderId))
      else Right(
        tryCancel(order, mode).orElse(
          (!order.isCancelling &&
            !order.isState[IsTerminated] &&
            !order.isState[ProcessingKilled] &&
            !order.mark.contains(OrderMark.Cancelling(mode))
          ) ? OrderCancelMarked(mode))))

  private def tryCancel(order: Order[Order.State], mode: CancelMode): Option[OrderActorEvent] =
    if (isOrderCancelable(order, mode))
      if (order.isAttached && isAgent) Some(OrderDetachable)
      else if (order.isDetached && !isAgent) Some(OrderCancelled)
      else None
    else None

  private def isOrderCancelable(order: Order[Order.State], mode: CancelMode): Boolean =
    (mode != CancelMode.FreshOnly || order.isState[Order.Fresh]) &&
      order.isCancelable &&
      // If workflow End is reached, the order is finished normally
      !instruction(order.workflowPosition).isInstanceOf[End]

  /** Returns a `Right(Some(OrderSuspended | OrderSuspendMarked))` iff order is not already marked as suspending. */
  def suspend(orderId: OrderId, mode: SuspendMode): Checked[Option[OrderActorEvent]] =
    withOrder(orderId)(order =>
      if (order.isSuspended)
        Right(trySuspend(order))
      else
        order.mark match {
          case Some(_: OrderMark.Cancelling) =>
            Left(CannotSuspendOrderProblem)
          case Some(_: OrderMark.Suspending)  =>  // Already marked
            Right(None)
          case None | Some(_: OrderMark.Resuming) =>
            Right((!order.isSuspended || order.isResuming) ? trySuspend(order).getOrElse(OrderSuspendMarked(mode)))
        })

  private def trySuspend(order: Order[Order.State]): Option[OrderActorEvent] =
    if (weHave(order) && isOrderSuspendible(order))
      if (order.isAttached && isAgent)
        Some(OrderDetachable)
      else if (order.isDetached && (!order.isSuspended || order.isResuming))
        Some(OrderSuspended)
      else
        None
    else
      None

  private def isOrderSuspendible(order: Order[Order.State]): Boolean =
    weHave(order) && order.isSuspendible

  /** Returns a `Right(Some(OrderResumed | OrderResumeMarked))` iff order is not already marked as resuming. */
  def resume(
    orderId: OrderId,
    position: Option[Position],
    historicOutcomes: Option[Seq[HistoricOutcome]])
  : Checked[Option[OrderActorEvent]] =
    withOrder(orderId)(order =>
      (order.mark match {
        case Some(_: OrderMark.Cancelling) =>
          Left(CannotResumeOrderProblem)

        case Some(OrderMark.Resuming(`position`, `historicOutcomes`)) =>
          Right(order.isDetached ? OrderResumed(position, historicOutcomes)/*should already be happened*/)

        case Some(OrderMark.Resuming(_, _)) =>
           Left(CannotResumeOrderProblem)

        case Some(OrderMark.Suspending(_)) if position.isDefined || historicOutcomes.isDefined =>
           Left(CannotResumeOrderProblem)

        case None | Some(OrderMark.Suspending(_)) =>
          if (!order.isSuspended && !order.mark.exists(_.isInstanceOf[OrderMark.Suspending]))
            Left(CannotResumeOrderProblem)
          else {
            lazy val checkedWorkflow = idToWorkflow(order.workflowId)
            val checkedPosition = position match {
              case None => Right(None)
              case Some(position) =>
                checkedWorkflow.flatMap(workflow =>
                  if (!workflow.isMoveable(order.position, position))
                    Left(Problem.pure("ResumeOrder: Unreachable order position"))
                  else
                    Right(Some(position)))
            }
            val checkedHistoricOutcomes = historicOutcomes match {
              case None => Right(None)
              case Some(historicOutcomes) =>
                checkedWorkflow.flatMap(workflow =>
                  historicOutcomes.toVector.traverse(o => workflow.checkedPosition(o.position).map(_ => o)))
                .map(Some.apply)
            }
            for {
              position <- checkedPosition
              historicOutcomes <- checkedHistoricOutcomes
            } yield Some(
              tryResume(order, position, historicOutcomes)
                .getOrElse(OrderResumeMarked(position, historicOutcomes)))
          }
      }))

  /** Retrieve the Order, check if calculated event is applicable. */
  private def withOrder[E <: OrderCoreEvent](orderId: OrderId)(body: Order[Order.State] => Checked[Option[E]]): Checked[Option[E]] =
    idToOrder(orderId).flatMap(order =>
      body(order).flatMapT(event =>
        order.update(event)
          .map(_ => Some(event))))

  private def tryResume(
    order: Order[Order.State],
    position: Option[Position],
    historicOutcomes: Option[Seq[HistoricOutcome]])
  : Option[OrderActorEvent] =
    (weHave(order) && order.isResumable) ? OrderResumed(position, historicOutcomes)

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
              idToWorkflow(order.workflowId).flatMap(_.labeledInstruction(pos))
                .fold(_.toString, _.toString).truncateWithEllipsis(50)).mkString(" --> ")))
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
          InstructionExecutor.nextPosition(instr, order, context)

        //case _: End if order.position.isNested =>
        //  order.position.dropChild.flatMap(returnPosition =>
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
