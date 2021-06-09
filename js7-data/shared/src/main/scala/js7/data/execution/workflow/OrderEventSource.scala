package js7.data.execution.workflow

import cats.instances.either._
import cats.syntax.traverse._
import js7.base.problem.Checked._
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Assertions.assertThat
import js7.base.utils.ScalaUtils.checkedCast
import js7.base.utils.ScalaUtils.syntax._
import js7.data.Problems.{CancelChildOrderProblem, CancelStartedOrderProblem}
import js7.data.command.{CancellationMode, SuspensionMode}
import js7.data.event.{<-:, KeyedEvent}
import js7.data.execution.workflow.context.StateView
import js7.data.execution.workflow.instructions.{ForkExecutor, InstructionExecutor}
import js7.data.lock.{LockPath, LockState}
import js7.data.order.Order.{Failed, IsTerminated, ProcessingKilled}
import js7.data.order.OrderEvent.{OrderActorEvent, OrderAwoke, OrderBroken, OrderCancellationMarked, OrderCancelled, OrderCatched, OrderCoreEvent, OrderDetachable, OrderFailed, OrderFailedEvent, OrderFailedInFork, OrderFailedIntermediate_, OrderMoved, OrderPromptAnswered, OrderRemoved, OrderResumptionMarked, OrderResumed, OrderSuspensionMarked, OrderSuspended}
import js7.data.order.{HistoricOutcome, Order, OrderId, OrderMark, Outcome}
import js7.data.problems.{CannotResumeOrderProblem, CannotSuspendOrderProblem, UnreachableOrderPositionProblem}
import js7.data.workflow.instructions.{End, Fork, Gap, Goto, IfFailedGoto, LockInstruction, Retry, TryInstruction}
import js7.data.workflow.position.BranchPath.Segment
import js7.data.workflow.position.{BranchId, ForkBranchId, Position, TryBranchId, WorkflowPosition}
import js7.data.workflow.{Instruction, Workflow, WorkflowId}
import scala.annotation.tailrec

/**
  * @author Joacim Zschimmer
  */
final class OrderEventSource(
  idToOrder: OrderId => Checked[Order[Order.State]],
  idToWorkflow: WorkflowId => Checked[Workflow],
  pathToLockState: LockPath => Checked[LockState],
  isAgent: Boolean)
{
  private val stateView = new StateView {
    def idToOrder                    = OrderEventSource.this.idToOrder
    def idToWorkflow(id: WorkflowId) = OrderEventSource.this.idToWorkflow(id)
    def pathToLockState                = OrderEventSource.this.pathToLockState

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

  def nextEvents(orderId: OrderId): Seq[KeyedEvent[OrderActorEvent]] = {
    val order = idToOrder(orderId).orThrow
    if (order.isState[Order.Broken])
      Nil  // Avoid issuing a second OrderBroken (would be a loop)
    else
      checkedNextEvents(order) |> (invalidToEvent(order, _))
  }

  private def checkedNextEvents(order: Order[Order.State]): Checked[Seq[KeyedEvent[OrderActorEvent]]] = {
    def ifDefinedElse(o: Option[OrderActorEvent], orElse: Checked[Seq[KeyedEvent[OrderActorEvent]]]) =
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
              InstructionExecutor.toEvents(instruction(order.workflowPosition), order, stateView)
                // Multiple returned events are expected to be independent and are applied to same idToOrder
                .flatMap(_
                  .traverse {
                    case orderId <-: (moved: OrderMoved) =>
                      applyMoveInstructions(order, moved)
                        .map(orderId <-: _)

                    case orderId <-: OrderFailedIntermediate_(outcome, uncatchable) =>
                      // OrderFailedIntermediate_ is used internally only
                      assertThat(orderId == order.id)
                      failOrDetach(order, outcome, uncatchable)
                        .map(orderId <-: _)

                    case o => Right(o)
                  })
          }))
      .flatMap(checkEvents)
  }

  private def checkEvents(keyedEvents: Seq[KeyedEvent[OrderActorEvent]]) = {
    var id2o = Map.empty[OrderId, Checked[Order[Order.State]]]
    var problem: Option[Problem] = None
    for (KeyedEvent(orderId, event) <- keyedEvents if problem.isEmpty) {
      id2o.getOrElse(orderId, idToOrder(orderId)).flatMap(_.applyEvent(event)) match {
        case Left(prblm) => problem = Some(prblm)
        case Right(order) => id2o += orderId -> Right(order)
      }
    }
    problem.toLeft(keyedEvents)
  }

  // Special handling for try with maxRetries and catch block with retry instruction only:
  // try (maxRetries=n) ... catch retry
  // In this case, OrderFailed event must have original failures's position, not failed retry's position.
  private def isMaxRetriesReached(workflowId: WorkflowId, firstCatchPos: Position): Boolean =
    catchStartsWithRetry(workflowId /: firstCatchPos) &&
      firstCatchPos.dropChild.forall(parentPos =>
        instruction(workflowId /: parentPos) match {  // Parent must be a TryInstruction
          case t: TryInstruction => t.maxTries.forall(firstCatchPos.tryCount >= _)
        })

  private def catchStartsWithRetry(firstCatchPos: WorkflowPosition) =
    instruction(firstCatchPos).withoutSourcePos == Retry()

  private def invalidToEvent[A](order: Order[Order.State], checkedEvents: Checked[Seq[KeyedEvent[OrderActorEvent]]])
  : Seq[KeyedEvent[OrderActorEvent]] =
    checkedEvents match {
      case Left(problem) =>
        val event =
          if (order.isOrderFailedApplicable)
            failOrDetach(order, Some(Outcome.Disrupted(problem)), uncatchable = true) match {
              case Left(prblm) =>
                scribe.debug(s"WARN ${order.id}: $prblm")
                OrderBroken(problem)
              case Right(event) => event
            }
          else if (order.isAttached && order.isInDetachableState && order.copy(attachedState = None).isOrderFailedApplicable) {
            scribe.debug(s"Detaching ${order.id} after failure: $problem")
            // Controller is expected to repeat this call and to reproduce the problem.
            OrderDetachable
          } else
            OrderBroken(problem)
        (order.id <-: event) :: Nil

      case Right(o) => o
    }

  private[data] def failOrDetach(
    order: Order[Order.State],
    outcome: Option[Outcome.NotSucceeded],
    uncatchable: Boolean)
  : Checked[OrderActorEvent] =
    fail(order, outcome, uncatchable).map {
      case _: OrderFailed if order.isAttached =>
        scribe.debug(s"Detaching ${order.id} to allow Controller emitting OrderFailed(${outcome getOrElse ""})")
        // Controller is expected to reproduce the problem !!!
        OrderDetachable
      case o => o
    }

  private def fail(order: Order[Order.State], outcome: Option[Outcome.NotSucceeded], uncatchable: Boolean)
  : Checked[OrderFailedEvent] =
    idToWorkflow(order.workflowId)
      .flatMap(workflow => failToPosition(workflow, order.position, outcome, uncatchable = uncatchable))

  private[workflow] def failToPosition(
    workflow: Workflow,
    position: Position,
    outcome: Option[Outcome.NotSucceeded],
    uncatchable: Boolean)
  : Checked[OrderFailedEvent] = {
    def loop(reverseBranchPath: List[Segment], failPosition: Position, lockPaths: Vector[LockPath] = Vector.empty)
    : Checked[OrderFailedEvent] =
      reverseBranchPath match {
        case Nil =>
          Right(OrderFailed(failPosition, outcome, lockPaths))

        case Segment(_, ForkBranchId(_)) :: _ =>
          Right(OrderFailedInFork(failPosition, outcome, lockPaths))

        case Segment(_, BranchId.Lock) :: prefix =>
          val pos = prefix.reverse % 0
          checkedCast[LockInstruction](workflow.instruction(pos))
            .flatMap(lockInstr => loop(prefix, pos, lockPaths :+ lockInstr.lockPath))

        case Segment(nr, TryBranchId(retry)) :: prefix if !uncatchable =>
          val catchPos = prefix.reverse % nr / BranchId.catch_(retry) % 0
          if (isMaxRetriesReached(workflow.id, catchPos))
            loop(prefix, failPosition, lockPaths)
          else
            Right(OrderCatched(catchPos, outcome, lockPaths))

        case Segment(_, _) :: prefix =>
          loop(prefix, failPosition, lockPaths)
      }

    loop(position.branchPath.reverse, position, Vector.empty)
  }

  private def joinedEvent(order: Order[Order.State]): Checked[Option[KeyedEvent[OrderActorEvent]]] =
    if ((order.isDetached || order.isAttached) && order.isState[Order.FailedInFork])
      order.forkPosition.flatMap(forkPosition =>
        stateView.instruction(order.workflowId /: forkPosition) match {
          case fork: Fork =>
            Right(ForkExecutor.tryJoinChildOrder(stateView, order, fork))
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

  /** Returns `Right(Some(OrderCancelled | OrderCancellationMarked))` iff order is not already marked as cancelling. */
  def cancel(orderId: OrderId, mode: CancellationMode): Checked[Option[OrderActorEvent]] =
    withOrder(orderId)(order =>
      if (order.parent.isDefined)
        Left(CancelChildOrderProblem(orderId))
      else if (mode == CancellationMode.FreshOnly && order.isStarted)
        // On Agent, the Order may already have been started without notice of the Controller
        Left(CancelStartedOrderProblem(orderId))
      else Right(
        tryCancel(order, mode).orElse(
          ( !order.isState[IsTerminated] &&
            !order.isState[ProcessingKilled] &&
            !order.mark.contains(OrderMark.Cancelling(mode))
          ) ? OrderCancellationMarked(mode))))

  private def tryCancel(order: Order[Order.State], mode: CancellationMode): Option[OrderActorEvent] =
    if (isOrderCancelable(order, mode))
      if (order.isAttached && isAgent) Some(OrderDetachable)
      else if (order.isDetached && !isAgent) Some(OrderCancelled)
      else None
    else None

  private def isOrderCancelable(order: Order[Order.State], mode: CancellationMode): Boolean =
    (mode != CancellationMode.FreshOnly || order.isState[Order.Fresh]) &&
      order.isCancelable &&
      // If workflow End is reached, the order is finished normally
      !instruction(order.workflowPosition).isInstanceOf[End]

  /** Returns a `Right(Some(OrderSuspended | OrderSuspensionMarked))` iff order is not already marked as suspending. */
  def suspend(orderId: OrderId, mode: SuspensionMode): Checked[Option[OrderActorEvent]] =
    withOrder(orderId)(order =>
      if (order.isSuspended)
        Right(trySuspend(order))
      else
        order.mark match {
          case Some(_: OrderMark.Cancelling) =>
            Left(CannotSuspendOrderProblem)
          case Some(_: OrderMark.Suspending) =>  // Already marked
            Right(None)
          case None | Some(_: OrderMark.Resuming) =>
            if (order.isState[Failed] || order.isState[IsTerminated])
              Left(CannotSuspendOrderProblem)
            else
              Right((!order.isSuspended || order.isResuming) ?
                trySuspend(order).getOrElse(OrderSuspensionMarked(mode)))
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

  /** Returns a `Right(Some(OrderResumed | OrderResumptionMarked))` iff order is not already marked as resuming. */
  def resume(
    orderId: OrderId,
    position: Option[Position],
    historicOutcomes: Option[Seq[HistoricOutcome]])
  : Checked[Option[OrderActorEvent]] =
    withOrder(orderId)(order =>
      order.mark match {
        case Some(_: OrderMark.Cancelling) =>
          Left(CannotResumeOrderProblem)

        case Some(OrderMark.Resuming(`position`, `historicOutcomes`)) =>
          Right(order.isDetached ? OrderResumed(position, historicOutcomes)/*should already has happened*/)

        case Some(OrderMark.Resuming(_, _)) =>
           Left(CannotResumeOrderProblem)

        case Some(OrderMark.Suspending(_)) if position.isDefined || historicOutcomes.isDefined =>
           Left(CannotResumeOrderProblem)

        case None | Some(OrderMark.Suspending(_)) =>
          val okay = order.isSuspended ||
            order.mark.exists(_.isInstanceOf[OrderMark.Suspending]) ||
            order.isState[Failed] && order.isDetached
          if (!okay)
            Left(CannotResumeOrderProblem)
          else {
            lazy val checkedWorkflow = idToWorkflow(order.workflowId)
            val checkedPosition = position match {
              case None => Right(None)
              case Some(position) =>
                checkedWorkflow.flatMap(workflow =>
                  if (!workflow.isMoveable(order.position, position))
                    Left(UnreachableOrderPositionProblem)
                  else
                    Right(Some(position)))
            }
            val checkedHistoricOutcomes = historicOutcomes match {
              case None => Right(None)
              case Some(historicOutcomes) =>
                checkedWorkflow.flatMap(workflow =>
                  historicOutcomes.traverse(o => workflow.checkedPosition(o.position).map(_ => o)))
                .map(Some.apply)
            }
            for {
              position <- checkedPosition
              historicOutcomes <- checkedHistoricOutcomes
            } yield Some(
              tryResume(order, position, historicOutcomes)
                .getOrElse(OrderResumptionMarked(position, historicOutcomes)))
          }
      })

  /** Retrieve the Order, check if calculated event is applicable. */
  private def withOrder[E <: OrderCoreEvent](orderId: OrderId)(body: Order[Order.State] => Checked[Option[E]]): Checked[Option[E]] =
    idToOrder(orderId).flatMap(order =>
      body(order).flatMapT(event =>
        order.applyEvent(event)
          .map(_ => Some(event))))

  private def tryResume(
    order: Order[Order.State],
    position: Option[Position],
    historicOutcomes: Option[Seq[HistoricOutcome]])
  : Option[OrderActorEvent] =
    (weHave(order) && order.isResumable) ? OrderResumed(position, historicOutcomes)

  def answer(orderId: OrderId): Checked[Seq[KeyedEvent[OrderCoreEvent]]] =
    for {
      order <- idToOrder(orderId)
      _ <- order.checkedState[Order.Prompting]
      moved <- applyMoveInstructions(order, OrderMoved(order.position.increment))
    } yield
      Seq(
        orderId <-: OrderPromptAnswered(),
        orderId <-: moved)

  private def weHave(order: Order[Order.State]) =
    order.isDetached && !isAgent ||
    order.isAttached && isAgent

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
                idToWorkflow(order.workflowId).flatMap(_.labeledInstruction(pos))
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
          InstructionExecutor.nextPosition(instr, order, stateView)

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
    idToWorkflow(workflowPosition.workflowId) match {
      case Left(_) =>
        scribe.error(s"Missing ${workflowPosition.workflowId}")
        Gap.empty
      case Right(workflow) =>
        workflow.instruction(workflowPosition.position)
    }
}
