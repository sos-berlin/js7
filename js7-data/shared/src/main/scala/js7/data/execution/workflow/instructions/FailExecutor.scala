package js7.data.execution.workflow.instructions

import js7.data.execution.workflow.context.StateView
import js7.data.order.OrderEvent.{OrderFailedIntermediate_, OrderStarted}
import js7.data.order.{Order, Outcome}
import js7.data.workflow.instructions.Fail

/**
  * @author Joacim Zschimmer
  */
object FailExecutor extends EventInstructionExecutor
{
  type Instr = Fail

  def toEvents(fail: Fail, order: Order[Order.State], state: StateView) =
    order.state match {
      case _: Order.Fresh =>
        Right((order.id <-: OrderStarted) :: Nil)

      case _: Order.Ready =>
        lazy val maybeErrorMessage = fail.message
          .map(o => state.makeScope(order).flatMap(_.evalString(o)).fold(_.toString, identity))
        lazy val outcome = Outcome.Failed(maybeErrorMessage, fail.namedValues)
        Right((order.id <-: OrderFailedIntermediate_(Some(outcome), uncatchable = fail.uncatchable)) :: Nil)

      case _ => Right(Nil)
    }
}
