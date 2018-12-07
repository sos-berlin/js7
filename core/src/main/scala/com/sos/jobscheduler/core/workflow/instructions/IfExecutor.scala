package com.sos.jobscheduler.core.workflow.instructions

import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.core.expression.{Evaluator, Scope}
import com.sos.jobscheduler.data.order.{Order, Outcome}
import com.sos.jobscheduler.data.workflow.OrderContext
import com.sos.jobscheduler.data.workflow.instructions.If
import com.sos.jobscheduler.data.workflow.position.Position

/**
  * @author Joacim Zschimmer
  */
object IfExecutor extends PositionInstructionExecutor {

  type Instr = If

  private val logger = Logger(getClass)

  def nextPosition(context: OrderContext, order: Order[Order.State], instruction: If): Option[Position] = {
    assert(order == context.idToOrder(order.id).withPosition(order.position))
    order.outcome match {
      case Outcome.Succeeded(returnCode) ⇒
        Evaluator.evalBoolean(new Scope(returnCode, order.variables), instruction.predicate)
          .map {
            case true ⇒ Some(0)  // Then
            case false ⇒ instruction.elseWorkflow.isDefined ? 1  // Else
          }.map {
            case Some(thenOrElse) ⇒ Position((order.position.nr / thenOrElse) :: Nil, 0)
            case None ⇒ order.position.increment  // Skip instruction
          }.onProblem(p ⇒ logger.error(s"$p")) // TODO None is an error. Return Invalid

      case _ ⇒
        None
    }
  }
}
