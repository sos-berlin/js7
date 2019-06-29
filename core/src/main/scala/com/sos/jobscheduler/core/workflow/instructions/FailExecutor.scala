package com.sos.jobscheduler.core.workflow.instructions

import cats.data.Validated.Valid
import com.sos.jobscheduler.core.workflow.OrderContext
import com.sos.jobscheduler.data.order.OrderEvent.{OrderDetachable, OrderFailed, OrderFailedCatchable, OrderFailedInFork, OrderStarted}
import com.sos.jobscheduler.data.order.{Order, Outcome}
import com.sos.jobscheduler.data.workflow.instructions.Fail

/**
  * @author Joacim Zschimmer
  */
object FailExecutor extends EventInstructionExecutor
{
  type Instr = Fail

  def toEvent(context: OrderContext, order: Order[Order.State], fail: Fail) =
    order.state match {
      case _: Order.Fresh =>
        Valid(Some(order.id <-: OrderStarted))

      case _: Order.Ready =>
        lazy val maybeErrorMessage = fail.message
          .map(o => context.makeScope(order).evalString(o).valueOr(_.toString))
        lazy val outcome = fail.returnCode match {
          case Some(returnCode) =>
            Outcome.Failed(maybeErrorMessage, returnCode)
          case None => order.lastOutcome match {
            case o: Outcome.NotSucceeded => o
            case o: Outcome.Succeeded => Outcome.Failed(maybeErrorMessage, o.returnCode, Map.empty)
          }
        }
        Valid(Some(order.id <-: (
          if (!fail.uncatchable)
            OrderFailedCatchable(outcome)
          else if (order.position.isInFork)
            OrderFailedInFork(outcome)
          else if (order.isAttached)
            OrderDetachable
          else
            OrderFailed(outcome))))

      case _ => Valid(None)
    }
}
