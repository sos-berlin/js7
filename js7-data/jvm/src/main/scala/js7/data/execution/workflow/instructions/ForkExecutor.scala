package js7.data.execution.workflow.instructions

import cats.syntax.traverse.*
import js7.base.problem.Checked
import js7.base.time.Timestamp
import js7.data.order.OrderEvent.OrderForked
import js7.data.order.{Order, OrderOutcome}
import js7.data.state.EngineState
import js7.data.workflow.instructions.Fork

private object ForkExecutor extends ForkInstructionExecutor[Fork]:

  protected def toForkedEvent(
    fork: Instr,
    order: Order[Order.IsFreshOrReady],
    engineState: EngineState,
    now: Timestamp)
  : Checked[OrderForked] =
    fork.branches.traverse: branch =>
      order.id.withChild(branch.id.string).map: childOrderId =>
        OrderForked.Child(branch.id, childOrderId)
    .map: children =>
      OrderForked(children)

  protected def forkResult(
    fork: Fork,
    order: Order[Order.Forked],
    engineState: EngineState,
    now: Timestamp)
  : OrderOutcome.Completed =
    OrderOutcome.Completed.fromChecked:
      fork.branches.traverse: branch =>
        calcResult(branch.result, order.id / branch.id.string, engineState, now)
      .map: results =>
        OrderOutcome.Succeeded(results.view.flatten.toMap)
