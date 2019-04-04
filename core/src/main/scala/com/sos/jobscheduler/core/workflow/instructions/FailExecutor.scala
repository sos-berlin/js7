package com.sos.jobscheduler.core.workflow.instructions

import cats.data.Validated.Valid
import cats.instances.option._
import cats.syntax.traverse._
import com.sos.jobscheduler.core.workflow.OrderContext
import com.sos.jobscheduler.data.expression.Evaluator
import com.sos.jobscheduler.data.order.OrderEvent.{OrderDetachable, OrderFailed, OrderFailedCatchable, OrderStarted}
import com.sos.jobscheduler.data.order.{Order, Outcome}
import com.sos.jobscheduler.data.workflow.instructions.Fail

/**
  * @author Joacim Zschimmer
  */
object FailExecutor extends EventInstructionExecutor
{
  type Instr = Fail

  def toEvent(context: OrderContext, order: Order[Order.State], instruction: Fail) =
    order.state match {
      case _: Order.Fresh =>
        Valid(Some(order.id <-: OrderStarted))

      case _: Order.Ready  =>
        if (instruction.uncatchable && order.isAttached)
          Valid(Some(order.id <-: OrderDetachable))
        else
          instruction.errorMessage
            .traverse(o => Evaluator.evalString(context.makeScope(order), o))
            .map(_ getOrElse Outcome.Failed.DefaultErrorMessage)
            .map { errorMessage =>
              val outcome = instruction.returnCode match {
                case Some(returnCode) =>
                  Outcome.Failed(errorMessage, returnCode)
                case None =>
                  order.lastOutcome match {
                    case o: Outcome.NotSucceeded => o
                    case o: Outcome.Succeeded => Outcome.Failed(errorMessage, o.returnCode, Map.empty)
                    case o: Outcome.Disrupted => o
                  }
              }
              Some(order.id <-: (if (instruction.uncatchable) OrderFailed(outcome) else OrderFailedCatchable(outcome)))
            }

      case _ => Valid(None)
    }
}
