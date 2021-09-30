package js7.data.execution.workflow.instructions

import js7.base.problem.Checked._
import js7.base.problem.Problem
import js7.data.order.Order
import js7.data.order.OrderEvent.{OrderFinished, OrderMoved}
import js7.data.state.StateView
import js7.data.workflow.instructions.{End, Finish, ForkInstruction}
import js7.data.workflow.position.{BranchPath, InstructionNr}

private[instructions] final class FinishExecutor(protected val service: InstructionExecutorService)
extends EventInstructionExecutor
{
  type Instr = Finish
  val instructionClass = classOf[Finish]

  def toEvents(instruction: Finish, order: Order[Order.State], state: StateView) =
    start(order)
      .orElse(detach(order))
      .getOrElse(
        order.state match {
          case _: Order.Ready =>
            order.position.forkBranchReversed match {
              case Nil =>
                // Not in a fork
                Right((order.id <-: OrderFinished) :: Nil)

              case BranchPath.Segment(nr, branchId) :: reverseInit =>
                // In a fork
                val forkPosition = reverseInit.reverse % nr
                for {
                  fork <- state.instruction_[ForkInstruction](order.workflowId /: forkPosition)
                  branchWorkflow <- fork.workflow(branchId)
                  endPos <- branchWorkflow.instructions.iterator.zipWithIndex
                    .collectFirst { case (_: End, index) => forkPosition / branchId % InstructionNr(index) }
                    .toChecked(Problem(s"Missing End instruction in branch ${forkPosition / branchId}"))  // Does not happen
                } yield (order.id <-: OrderMoved(endPos)) :: Nil
            }

          case _ => Right(Nil)
        })
}
