package js7.data.execution.workflow.instructions

import js7.base.problem.Checked
import js7.base.utils.CatsUtils.syntax.view
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.order.Order
import js7.data.order.Order.{Broken, Failed, FailedWhileFresh}
import js7.data.order.OrderEvent.OrderMoved
import js7.data.state.StateView
import js7.data.workflow.instructions.If
import js7.data.workflow.position.*
import js7.data.workflow.position.BranchPath.syntax.*

private[instructions] final class IfExecutor(protected val service: InstructionExecutorService)
extends EventInstructionExecutor, PositionInstructionExecutor:

  type Instr = If
  val instructionClass = classOf[If]

  def toEvents(instruction: If, order: Order[Order.State], state: StateView) =
    if order.isState[Broken] || order.isState[FailedWhileFresh] || order.isState[Failed] then
      Right(Nil)
    else
      nextMove(instruction, order, state)
        .map(_.map(order.id <-: _))
        .map(_.toList)

  def nextMove(instruction: If, order: Order[Order.State], state: StateView) =
    // order may be predicted and different from actual idToOrder(order.id)
    for
      scope <- state.toOrderScope(order)
      branchId <-
        instruction.ifThens.view
          .scanLeft(Checked(false) -> 0):
            case ((_, i), ifThen) => ifThen.predicate.evalAsBoolean(using scope) -> (i + 1)
          .appended:
            Right(true) -> instruction.elseBlock.fold(-1)(_ => 0)
          .takeThrough: o => // Skip false predicates
            o._1.contains(false)
          .last match
            case (checked, -1) => checked.rightAs(None) // No then, no else
            case (checked, 0) => checked.rightAs(Some(BranchId.Else))
            case (checked, i) => checked.rightAs(Some(BranchId.then_(i)))
    yield
      Some(OrderMoved:
        branchId match
          case Some(branchId) => order.position / branchId % 0
          case None => order.position.increment)  // No else-part, skip instruction

  override def subworkflowEndToPosition(parentPos: Position) =
    Some(parentPos.increment)
