package js7.data.execution.workflow.instructions

import js7.data.order.OrderEvent.OrderFailedIntermediate_
import js7.data.order.{Order, Outcome}
import js7.data.state.StateView
import js7.data.workflow.instructions.Fail

private[instructions] final class FailExecutor(protected val service: InstructionExecutorService)
extends EventInstructionExecutor
{
  type Instr = Fail

  def toEvents(fail: Fail, order: Order[Order.State], state: StateView) =
    start(order)
      .getOrElse(
        order.state match {
          case Order.Ready =>
            val maybeErrorMessage = fail.message
              .map(messageExpr => state
                .toScope(order)
                .flatMap(messageExpr.evalAsString(_))
                .fold(_.toString, identity))
            val outcome = Outcome.Failed(maybeErrorMessage, fail.namedValues)
            val event = OrderFailedIntermediate_(Some(outcome), uncatchable = fail.uncatchable)
            Right((order.id <-: event) :: Nil)

          case _ => Right(Nil)
        })
}
