package com.sos.jobscheduler.core.workflow.instructions

import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.core.expression.Evaluator
import com.sos.jobscheduler.core.workflow.OrderContext
import com.sos.jobscheduler.data.event.KeyedEvent
import com.sos.jobscheduler.data.order.Order
import com.sos.jobscheduler.data.order.OrderEvent.{OrderActorEvent, OrderMoved}
import com.sos.jobscheduler.data.workflow.instructions.If
import com.sos.jobscheduler.data.workflow.instructions.If.{Else, Then}
import com.sos.jobscheduler.data.workflow.position.Position

/**
  * @author Joacim Zschimmer
  */
object IfExecutor extends EventInstructionExecutor with PositionInstructionExecutor
{
  type Instr = If

  private val logger = Logger(getClass)

  def toEvent(context: OrderContext, order: Order[Order.State], instruction: If): Option[KeyedEvent[OrderActorEvent]] =
    nextPosition(context, order, instruction) map (o ⇒ order.id <-: OrderMoved(o))

  def nextPosition(context: OrderContext, order: Order[Order.State], instruction: If): Option[Position] = {
    assert(order == context.idToOrder(order.id).withPosition(order.position))
    Evaluator.evalBoolean(context.makeScope(order), instruction.predicate)
      .map {
        case true ⇒ Some(Then)
        case false ⇒ instruction.elseWorkflow.isDefined ? Else
      }
      .map {
        case Some(thenOrElse) ⇒ order.position / thenOrElse % 0
        case None ⇒ order.position.increment  // No else-part, skip instruction
      }.onProblem(p ⇒ logger.error(s"$p"))  // TODO None is an error. Return Invalid
  }
}
