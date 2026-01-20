package js7.data.execution.workflow.instructions

import js7.data.order.Order
import js7.data.order.OrderEvent.{OrderDetachable, OrderNoticesConsumed}
import js7.data.state.EngineState
import js7.data.workflow.instructions.ConsumeNotices

private[instructions] final class ConsumeNoticesExecutor(
  protected val service: InstructionExecutorService)
extends
  ConsumeOrExpectNoticesExecutor:

  type Instr = ConsumeNotices
  val instructionClass = classOf[ConsumeNotices]

  override def onReturnFromSubworkflow(
    instr: ConsumeNotices,
    order: Order[Order.State],
    state: EngineState) =
    Right(List:
      order.id <-: (
        if order.isAttached then
          OrderDetachable
        else
          OrderNoticesConsumed()))
