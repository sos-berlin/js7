package js7.data.execution.workflow.instructions

import js7.base.problem.Checked._
import js7.base.problem.Problem
import js7.data.execution.workflow.context.OrderContext
import js7.data.order.Order
import js7.data.order.OrderEvent.{OrderDetachable, OrderFinished, OrderMoved, OrderStarted}
import js7.data.workflow.instructions.{End, Finish, Fork}
import js7.data.workflow.position.{BranchPath, InstructionNr}

/**
  * @author Joacim Zschimmer
  */
object FinishExecutor extends EventInstructionExecutor
{
  type Instr = Finish

  def toEvents(instruction: Finish, order: Order[Order.State], context: OrderContext) =
    order.state match {
      case _: Order.Fresh =>
        Right((order.id <-: OrderStarted) :: Nil)

      case _: Order.Ready =>
        if (order.isAttached)
          Right((order.id <-: OrderDetachable) :: Nil)
        else
          order.position.forkBranchReversed match {
            case Nil =>
              // Not in a fork
              Right((order.id <-: OrderFinished) :: Nil)

            case BranchPath.Segment(nr, branchId) :: reverseInit =>
              // In a fork
              val forkPosition = reverseInit.reverse % nr
              for {
                fork <- context.instruction_[Fork](order.workflowId /: forkPosition)
                branchWorkflow <- fork.workflow(branchId)
                endPos <- branchWorkflow.instructions.iterator.zipWithIndex
                  .collectFirst { case (_: End, index) => forkPosition / branchId % InstructionNr(index) }
                  .toChecked(Problem(s"Missing End instruction in branch ${forkPosition / branchId}"))  // Does not happen
              } yield (order.id <-: OrderMoved(endPos)) :: Nil
          }

      case _ => Right(Nil)
    }
}
