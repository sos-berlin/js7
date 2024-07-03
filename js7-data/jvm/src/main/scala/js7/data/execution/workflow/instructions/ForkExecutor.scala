package js7.data.execution.workflow.instructions

import cats.syntax.traverse.*
import js7.base.problem.Checked
import js7.base.time.Timestamp
import js7.data.order.OrderEvent.OrderForked
import js7.data.order.{Order, OrderOutcome}
import js7.data.state.StateView
import js7.data.workflow.instructions.Fork

private[instructions] final class ForkExecutor(protected val service: InstructionExecutorService)
extends EventInstructionExecutor, ForkInstructionExecutor:

  type Instr = Fork
  val instructionClass = classOf[Fork]

  protected def toForkedEvent(fork: Instr, order: Order[Order.IsFreshOrReady], state: StateView)
  : Checked[OrderForked] =
    for
      children <- fork.branches
        .traverse: branch =>
          order.id.withChild(branch.id.string)
            .map(childOrderId => OrderForked.Child(branch.id, childOrderId))
    yield
      OrderForked(children)

  protected def forkResult(fork: Fork, order: Order[Order.Forked], state: StateView, now: Timestamp)
  : OrderOutcome.Completed =
    OrderOutcome.Completed.fromChecked(
      fork.branches
        .traverse: branch =>
          calcResult(branch.result, order.id / branch.id.string, state, now)
        .map: results =>
          OrderOutcome.Succeeded(results.view.flatten.toMap))
