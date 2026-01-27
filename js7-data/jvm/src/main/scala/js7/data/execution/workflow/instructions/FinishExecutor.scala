package js7.data.execution.workflow.instructions

import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.RichBoolean
import js7.data.event.EventCalc
import js7.data.execution.workflow.OrderEventSource.leaveBlocks
import js7.data.order.OrderEvent.{OrderCoreEvent, OrderFinished, OrderMoved, OrderOutcomeAdded}
import js7.data.order.{Order, OrderId}
import js7.data.state.EngineEventColl.extensions.workflow
import js7.data.state.{EngineState, EngineState_}
import js7.data.workflow.Workflow
import js7.data.workflow.instructions.{End, Finish, ForkInstruction}
import js7.data.workflow.position.*
import js7.data.workflow.position.BranchPath.syntax.*

object FinishExecutor extends EventInstructionExecutor_[Finish]:

  def toEventCalc[S <: EngineState_[S]](instr: Finish, orderId: OrderId)
  : EventCalc[S, OrderCoreEvent] =
    detachOrder(orderId): (coll, order) =>
      start(coll, orderId): (coll, order) =>
        for
          workflow <- coll.workflow(order.workflowId)
          coll <- coll.addCheckedEventCalc:
            execute(instr, order, workflow, coll.aggregate)
        yield coll

  private def execute[S <: EngineState_[S]](
    instr: Finish,
    order: Order[Order.State], workflow: Workflow,
    engineState: EngineState)
  : Checked[EventCalc[S, OrderCoreEvent]] =
    order.state match
      case _: Order.Ready =>
        order.position.forkBranchReversed match
          case Nil =>
            // Not in a fork
            Right:
              leaveBlocks(order.id, workflow, OrderFinished(instr.outcome) :: Nil)

          case BranchPath.Segment(nr, branchId) :: reverseInit =>
            // In a fork
            val forkPosition = reverseInit.reverse % nr
            for
              fork <- engineState.instruction_[ForkInstruction](order.workflowId /: forkPosition)
              branchWorkflow <- fork.workflow(branchId)
              gotoEnd <- branchWorkflow.instructions.iterator.zipWithIndex
                .collectFirst:
                  case (_: End, index) => forkPosition / branchId % InstructionNr(index)
                .toChecked(Problem:
                  s"Missing End instruction in branch ${forkPosition / branchId}") // Doesn't happen
            yield
              leaveBlocks(order.id, workflow,
                events =
                  instr.outcome.map:
                    OrderOutcomeAdded(_)
                  ++:
                    (instr.outcome.forall(_.isSucceeded) && order.position != gotoEnd).thenList:
                      OrderMoved(gotoEnd))

      case _ => Right(EventCalc.empty)
