package com.sos.jobscheduler.core.workflow.instructions

import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.core.expression.{Evaluator, Scope}
import com.sos.jobscheduler.data.event.KeyedEvent
import com.sos.jobscheduler.data.order.OrderEvent.{OrderActorEvent, OrderMoved}
import com.sos.jobscheduler.data.order.{Order, Outcome}
import com.sos.jobscheduler.data.workflow.OrderContext
import com.sos.jobscheduler.data.workflow.instructions.If
import com.sos.jobscheduler.data.workflow.instructions.If.{Else, Then}
import com.sos.jobscheduler.data.workflow.position.Position

/**
  * @author Joacim Zschimmer
  */
object IfExecutor extends EventInstructionExecutor with PositionInstructionExecutor {

  type Instr = If

  private val logger = Logger(getClass)

  def toEvent(context: OrderContext, order: Order[Order.State], instruction: If): Option[KeyedEvent[OrderActorEvent]] =
    nextPosition(context, order, instruction) map (o ⇒ order.id <-: OrderMoved(o))

  def nextPosition(context: OrderContext, order: Order[Order.State], instruction: If): Option[Position] = {
    assert(order == context.idToOrder(order.id).withPosition(order.position))
    order.outcome match {
      case Outcome.Succeeded(returnCode) ⇒
        Evaluator.evalBoolean(new Scope(returnCode, order.variables), instruction.predicate)
          .map {
            case true ⇒ Some(Then)
            case false ⇒ instruction.elseWorkflow.isDefined ? Else
          }
          .map {
            case Some(thenOrElse) ⇒ order.position / thenOrElse % 0
            case None ⇒ order.position.increment  // Skip instruction
          }.onProblem(p ⇒ logger.error(s"$p")) // TODO None is an error. Return Invalid

      case _ ⇒
        None
    }
  }
}
