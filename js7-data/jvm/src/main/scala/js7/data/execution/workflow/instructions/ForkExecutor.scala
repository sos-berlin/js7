package js7.data.execution.workflow.instructions

import cats.syntax.traverse.*
import js7.base.problem.Checked
import js7.base.time.Timestamp
import js7.data.order.OrderEvent.{OrderActorEvent, OrderForked}
import js7.data.order.{Order, Outcome}
import js7.data.state.StateView
import js7.data.workflow.instructions.Fork

private[instructions] final class ForkExecutor(protected val service: InstructionExecutorService)
extends EventInstructionExecutor with ForkInstructionExecutor
{
  type Instr = Fork
  val instructionClass = classOf[Fork]

  protected def toForkedEvent(fork: Instr, order: Order[Order.Ready], state: StateView)
  : Checked[OrderActorEvent] =
    for {
      children <- fork.branches
        .traverse(branch =>
          order.id.withChild(branch.id.string)
            .map(childOrderId => OrderForked.Child(branch.id, childOrderId)))
      orderForked = OrderForked(children)
      event <- postprocessOrderForked(fork, order, orderForked, state)
    } yield event

  protected def forkResult(fork: Fork, order: Order[Order.Forked], state: StateView, now: Timestamp)
  : Outcome.Completed =
    Outcome.Completed.fromChecked(
      fork.branches
        .traverse(branch =>
          calcResult(branch.result, order.id / branch.id.string, state, now))
        .map(results =>
          Outcome.Succeeded(results.view.flatten.toMap)))
}
