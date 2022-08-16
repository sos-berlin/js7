package js7.data.execution.workflow.instructions

import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.RichPartialFunction
import js7.data.event.KeyedEvent
import js7.data.execution.workflow.OrderEventSource
import js7.data.order.OrderEvent.{OrderActorEvent, OrderFinished, OrderMoved}
import js7.data.order.{Order, OrderEvent}
import js7.data.state.StateView
import js7.data.workflow.Workflow
import js7.data.workflow.instructions.{End, Finish, ForkInstruction}
import js7.data.workflow.position.{BranchPath, InstructionNr}

private[instructions] final class FinishExecutor(protected val service: InstructionExecutorService)
extends EventInstructionExecutor
{
  type Instr = Finish
  val instructionClass = classOf[Finish]

  def toEvents(instruction: Finish, order: Order[Order.State], state: StateView) =
    state.idToWorkflow
      .checked(order.workflowId)
      .flatMap(workflow =>
        start(order)
          .orElse(detach(order))
          .getOrElse(
            execute(order, workflow, state)))

  private def execute(order: Order[Order.State], workflow: Workflow, state: StateView)
  : Checked[List[KeyedEvent[OrderEvent.OrderActorEvent]]] =
    order.state match {
      case _: Order.Ready =>
        order.position.forkBranchReversed match {
          case Nil =>
            // Not in a fork
            leaveBlocks(order, workflow, OrderFinished)

          case BranchPath.Segment(nr, branchId) :: reverseInit =>
            // In a fork
            val forkPosition = reverseInit.reverse % nr
            for {
              fork <- state.instruction_[ForkInstruction](order.workflowId /: forkPosition)
              branchWorkflow <- fork.workflow(branchId)
              endPos <- branchWorkflow.instructions.iterator.zipWithIndex
                .collectFirst { case (_: End, index) => forkPosition / branchId % InstructionNr(index) }
                .toChecked(Problem(s"Missing End instruction in branch ${ forkPosition / branchId }")) // Does not happen
              events <- leaveBlocks(order, workflow, OrderMoved(endPos))
            } yield events
        }

      case _ => Right(Nil)
    }

  private def leaveBlocks(order: Order[Order.State], workflow: Workflow, event: OrderActorEvent)
  : Checked[List[KeyedEvent[OrderActorEvent]]] =
    OrderEventSource
      .leaveBlocks(workflow, order, event)
      .map(_.map(order.id <-: _))
}
