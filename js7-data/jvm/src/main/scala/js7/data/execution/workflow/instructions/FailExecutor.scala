package js7.data.execution.workflow.instructions

import js7.data.order.OrderEvent.OrderFailedIntermediate_
import js7.data.order.{Order, OrderOutcome}
import js7.data.state.StateView
import js7.data.workflow.instructions.Fail

private[instructions] final class FailExecutor(protected val service: InstructionExecutorService)
extends EventInstructionExecutor:
  type Instr = Fail
  val instructionClass = classOf[Fail]

  def toEvents(fail: Fail, order: Order[Order.State], state: StateView) =
    start(order)
      .getOrElse:
        order.state match
          case _: Order.Ready =>
            val msg = fail.message
              .map(messageExpr => state
                .toImpureOrderExecutingScope(order, clock.now())
                .flatMap(messageExpr.evalAsString(_))
                .fold(_.toString, identity))
            val outcome = OrderOutcome.Failed(msg, fail.namedValues, uncatchable = fail.uncatchable)
            val event = OrderFailedIntermediate_(Some(outcome))
            Right((order.id <-: event) :: Nil)

          case _ => Right(Nil)
