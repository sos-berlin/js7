package com.sos.jobscheduler.core.workflow.instructions

import com.sos.jobscheduler.data.order.{Order, Outcome}
import com.sos.jobscheduler.data.workflow.instructions.IfReturnCode
import com.sos.jobscheduler.data.workflow.{OrderContext, Position}

/**
  * @author Joacim Zschimmer
  */
object IfReturnCodeExecutor extends PositionInstructionExecutor
{
  type Instr = IfReturnCode

  def nextPosition(context: OrderContext, order: Order[Order.Processed], instruction: IfReturnCode): Option[Position] = {
    assert(order == context.idToOrder(order.id).withPosition(order.position))
    Some(order.state.outcome) collect {
      case Outcome.Succeeded(returnCode) ⇒
        val indexOption =
          if (instruction.returnCodes contains returnCode)
            Some(0)
          else if (instruction.elseWorkflow.isDefined)
            Some(1)
          else
            None
        indexOption match {
          case Some(index) ⇒ Position(Position.Parent(order.position.nr, index) :: Nil, 0)
          case None ⇒ order.position.increment  // Skip statement
        }
    }
  }
}
