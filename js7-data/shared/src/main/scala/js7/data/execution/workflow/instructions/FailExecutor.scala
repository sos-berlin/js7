package js7.data.execution.workflow.instructions

import js7.data.execution.workflow.context.OrderContext
import js7.data.order.OrderEvent.{OrderFailedIntermediate_, OrderStarted}
import js7.data.order.{Order, Outcome}
import js7.data.workflow.instructions.Fail

/**
  * @author Joacim Zschimmer
  */
object FailExecutor extends EventInstructionExecutor
{
  type Instr = Fail

  def toEvent(fail: Fail, order: Order[Order.State], context: OrderContext) =
    order.state match {
      case _: Order.Fresh =>
        Right(Some(order.id <-: OrderStarted))

      case _: Order.Ready =>
        lazy val maybeErrorMessage = fail.message
          .map(o => context.makeScope(order).flatMap(_.evalString(o)).fold(_.toString, identity))
        lazy val outcome = Outcome.Failed(maybeErrorMessage, fail.namedValues)
        Right(Some(order.id <-:
          OrderFailedIntermediate_(Some(outcome), uncatchable = fail.uncatchable)))

      case _ => Right(None)
    }
}
