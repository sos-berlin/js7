package com.sos.jobscheduler.core.workflow.instructions

import cats.data.Validated.Valid
import cats.instances.option._
import cats.syntax.traverse._
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.core.workflow.OrderContext
import com.sos.jobscheduler.data.event.KeyedEvent
import com.sos.jobscheduler.data.expression.Evaluator
import com.sos.jobscheduler.data.job.ReturnCode
import com.sos.jobscheduler.data.order.OrderEvent.{OrderFailed, OrderStarted}
import com.sos.jobscheduler.data.order.{Order, Outcome}
import com.sos.jobscheduler.data.workflow.instructions.Fail

/**
  * @author Joacim Zschimmer
  */
object FailExecutor extends EventInstructionExecutor
{
  type Instr = Fail

  def toEvent(context: OrderContext, order: Order[Order.State], instruction: Fail) =
    order.ifState[Order.Fresh] match {
      case Some(o) => Valid(Some(o.id <-: OrderStarted))
      case None =>
        order.ifState[Order.Ready]
          .traverse[Checked, KeyedEvent[OrderFailed]](order =>
            instruction.errorMessage
              .traverse(o => Evaluator.evalString(context.makeScope(order), o))
              .map(_ getOrElse Outcome.Failed.DefaultErrorMessage)
              .map(errorMessage =>
                order.id <-: OrderFailed(
                  instruction.returnCode match {
                    case Some(returnCode) =>
                      Outcome.Failed(errorMessage, returnCode)
                    case None =>
                      order.lastOutcome match {
                        case o: Outcome.NotSucceeded => o
                        case _ => Outcome.Failed(errorMessage/*???*/, ReturnCode(-1))
                      }
                  })))
    }
}
