package js7.data.execution.workflow.instructions

import js7.data.execution.workflow.context.OrderContext
import js7.data.order.OrderEvent.{OrderDetachable, OrderFailed, OrderFailedCatchable_, OrderFailedInFork, OrderStarted}
import js7.data.order.{Order, Outcome}
import js7.data.workflow.instructions.Fail

/**
  * @author Joacim Zschimmer
  */
object FailExecutor extends EventInstructionExecutor
{
  type Instr = Fail

  def toEvent(context: OrderContext, order: Order[Order.State], fail: Fail) =
    order.state match {
      case _: Order.Fresh =>
        Right(Some(order.id <-: OrderStarted))

      case _: Order.Ready =>
        lazy val maybeErrorMessage = fail.message
          .map(o => context.makeScope(order).evalString(o).fold(_.toString, identity))
        lazy val outcome = Outcome.Failed(maybeErrorMessage, fail.namedValues)
        Right(Some(order.id <-: (
          if (!fail.uncatchable) {
            OrderFailedCatchable_(Some(outcome))
         } else if (order.position.isInFork) {
            OrderFailedInFork(Some(outcome))
         } else if (order.isAttached)
            OrderDetachable
          else
            OrderFailed(Some(outcome)))))

      case _ => Right(None)
    }
}
