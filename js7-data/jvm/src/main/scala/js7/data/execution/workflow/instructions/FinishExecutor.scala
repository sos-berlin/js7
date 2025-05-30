package js7.data.execution.workflow.instructions

import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.{RichBoolean, RichPartialFunction}
import js7.data.event.KeyedEvent
import js7.data.execution.workflow.OrderEventSource
import js7.data.order.Order
import js7.data.order.OrderEvent.{OrderActorEvent, OrderFinished, OrderMoved, OrderOutcomeAdded}
import js7.data.state.StateView
import js7.data.workflow.Workflow
import js7.data.workflow.instructions.{End, Finish, ForkInstruction}
import js7.data.workflow.position.*
import js7.data.workflow.position.BranchPath.syntax.*

private[instructions] final class FinishExecutor(protected val service: InstructionExecutorService)
extends EventInstructionExecutor:

  type Instr = Finish
  val instructionClass = classOf[Finish]

  def toEvents(instr: Finish, order: Order[Order.State], state: StateView) =
    detach(order)
      .orElse:
        start(order)
      .getOrElse:
        state.idToWorkflow
          .checked(order.workflowId)
          .flatMap: workflow =>
            execute(instr, order, workflow, state)

  private def execute(instr: Finish, order: Order[Order.State], workflow: Workflow, state: StateView)
  : Checked[List[KeyedEvent[OrderActorEvent]]] =
    order.state match
      case _: Order.Ready =>
        order.position.forkBranchReversed match
          case Nil =>
            // Not in a fork
            leaveBlocks(order, workflow, OrderFinished(instr.outcome) :: Nil)

          case BranchPath.Segment(nr, branchId) :: reverseInit =>
            // In a fork
            val forkPosition = reverseInit.reverse % nr
            for
              fork <- state.instruction_[ForkInstruction](order.workflowId /: forkPosition)
              branchWorkflow <- fork.workflow(branchId)
              gotoEnd <- branchWorkflow.instructions.iterator.zipWithIndex
                .collectFirst { case (_: End, index) => forkPosition / branchId % InstructionNr(index) }
                .toChecked(Problem(
                  s"Missing End instruction in branch ${forkPosition / branchId}")) // Does not happen
              events <- leaveBlocks(order, workflow,
                instr.outcome.map(OrderOutcomeAdded(_)) ++:
                  (instr.outcome.forall(_.isSucceeded) && order.position != gotoEnd)
                    .thenList(OrderMoved(gotoEnd)))
            yield
              events

      case _ => Right(Nil)

  private def leaveBlocks(order: Order[Order.State], w: Workflow, events: List[OrderActorEvent])
  : Checked[List[KeyedEvent[OrderActorEvent]]] =
    OrderEventSource
      .leaveBlocks(w, order, events)
      .map(_.map(order.id <-: _))
