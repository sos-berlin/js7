package com.sos.jobscheduler.core.workflow.instructions

import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.core.workflow.OrderContext
import com.sos.jobscheduler.data.order.Order
import com.sos.jobscheduler.data.order.OrderEvent.{OrderDetachable, OrderFinished, OrderMoved, OrderStarted}
import com.sos.jobscheduler.data.workflow.instructions.{End, Finish, Fork}
import com.sos.jobscheduler.data.workflow.position.{BranchPath, InstructionNr}

/**
  * @author Joacim Zschimmer
  */
object FinishExecutor extends EventInstructionExecutor
{
  type Instr = Finish

  def toEvent(context: OrderContext, order: Order[Order.State], instruction: Finish) =
    order.state match {
      case _: Order.Fresh =>
        Right(Some(order.id <-: OrderStarted))

      case _: Order.Ready =>
        if (order.isAttached)
          Right(Some(order.id <-: OrderDetachable))
        else
          order.position.forkBranchReversed match {
            case Nil =>
              // Not in a fork
              Right(Some(order.id <-: OrderFinished))

            case BranchPath.Segment(nr, branchId) :: reverseInit =>
              // In a fork
              val forkPosition = reverseInit.reverse % nr
              for {
                fork <- context.instruction_[Fork](order.workflowId /: forkPosition)
                branchWorkflow <- fork.workflow(branchId)
                endPos <- branchWorkflow.instructions.iterator.zipWithIndex
                  .collectFirst { case (_: End, index) => forkPosition / branchId % InstructionNr(index) }
                  .toChecked(Problem(s"Missing End instruction in branch ${forkPosition / branchId}"))  // Does not happen
              } yield Some(order.id <-: OrderMoved(endPos))
          }

      case _ => Right(None)
    }
}
