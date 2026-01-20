package js7.data.execution.workflow.instructions

import js7.base.log.Logger
import js7.base.utils.ScalaUtils.syntax.RichPartialFunction
import js7.data.execution.workflow.OrderEventSource.leaveBlocks
import js7.data.execution.workflow.instructions.BreakExecutor.*
import js7.data.order.Order
import js7.data.order.OrderEvent.{OrderCycleFinished, OrderFinished}
import js7.data.state.EngineState
import js7.data.state.StateViewForEvents.atController
import js7.data.workflow.instructions.Break

private[instructions] final class BreakExecutor(
  protected val service: InstructionExecutorService)
extends EventInstructionExecutor:

  type Instr = Break
  val instructionClass = classOf[Break]

  def toEvents(instr: Break, order: Order[Order.State], state: EngineState) =
    // OrderStarted may be needed if Order is placed into a Cycle block
    start(order)
      .getOrElse:
        for
          workflow <- state.idToWorkflow.checked(order.workflowId)
          events <- leaveBlocks(
            workflow, order,
            until = _.isCycle,
            events =
              case Some(branchId) if branchId.isCycle =>
                OrderCycleFinished(None) :: Nil
              case None =>
                state.atController(OrderFinished() :: Nil)
              case Some(branchId) =>
                // Just in case
                logger.warn:
                  s"Unexpected BranchId:$branchId encountered. ${order.id} position=${order.position}"
                // Best guess, to avoid a crash:
                state.atController(OrderFinished() :: Nil))
        yield
          events.map(order.id <-: _)

object BreakExecutor:
 private val logger = Logger[this.type]
